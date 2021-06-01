!=======================================================================
! Regional Earth System Model (RegESM)
! Copyright (c) 2013-2019 Ufuk Turuncoglu
! Licensed under the MIT License. 
!=======================================================================
#define FILENAME "mod_esmf_atm_wrf.F90"
!
!-----------------------------------------------------------------------
!     ATM gridded component code 
!-----------------------------------------------------------------------
!
      module mod_esmf_atm
!
!-----------------------------------------------------------------------
!     Used module declarations 
!-----------------------------------------------------------------------
!
      use ESMF
      use NUOPC
      use NUOPC_Model, only :                                           &
          NUOPC_SetServices          => SetServices,                    &
          NUOPC_Label_Advance        => label_Advance,                  &
          NUOPC_Label_DataInitialize => label_DataInitialize,           &
          NUOPC_Label_SetClock       => label_SetClock,                 &
          NUOPC_Label_CheckImport    => label_CheckImport
!
      use mod_types
      use mod_shared
!
      implicit none
      private
!
!-----------------------------------------------------------------------
!     Public subroutines 
!-----------------------------------------------------------------------
!
      public :: ATM_SetServices
!
      contains
!
      subroutine ATM_SetServices(gcomp, rc)
      implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
      type(ESMF_GridComp) :: gcomp
      integer, intent(out) :: rc
!
      rc = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!     Register NUOPC generic routines    
!-----------------------------------------------------------------------
!
      call NUOPC_CompDerive(gcomp, NUOPC_SetServices, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Register initialize routines (Phase 1 and Phase 2)  
!-----------------------------------------------------------------------
!
      call NUOPC_CompSetEntryPoint(gcomp,                               &
                                   methodflag=ESMF_METHOD_INITIALIZE,   &
                                   phaseLabelList=(/"IPDv00p1"/),       &
                                   userRoutine=ATM_SetInitializeP1,     &
                                   rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      call NUOPC_CompSetEntryPoint(gcomp,                               &
                                   methodflag=ESMF_METHOD_INITIALIZE,   &
                                   phaseLabelList=(/"IPDv00p2"/),       &
                                   userRoutine=ATM_SetInitializeP2,     &
                                   rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Attach phase independent specializing methods
!     Setting the slow and fast model clocks  
!-----------------------------------------------------------------------
!
      call NUOPC_CompSpecialize(gcomp,                                  &
                                specLabel=NUOPC_Label_DataInitialize,   &
                                specRoutine=ATM_DataInit, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      call NUOPC_CompSpecialize(gcomp, specLabel=NUOPC_Label_SetClock,  &
                                specRoutine=ATM_SetClock, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!      call NUOPC_CompSpecialize(gcomp,                                  &
!                                specLabel=NUOPC_Label_CheckImport,      &
!                                specPhaseLabel="RunPhase1",             &
!                                specRoutine=ATM_CheckImport, rc=rc)
!      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
!                             line=__LINE__, file=FILENAME)) return
!
      call NUOPC_CompSpecialize(gcomp, specLabel=NUOPC_Label_Advance,   &
                                specRoutine=ATM_ModelAdvance, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Register finalize routine    
!-----------------------------------------------------------------------
! 
      call ESMF_GridCompSetEntryPoint(gcomp,                            &
                                      methodflag=ESMF_METHOD_FINALIZE,  &
                                      userRoutine=ATM_SetFinalize,      &
                                      rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      end subroutine ATM_SetServices
!
      subroutine ATM_SetInitializeP1(gcomp, importState, exportState,   &
                                     clock, rc)
      implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
      type(ESMF_GridComp) :: gcomp
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      integer, intent(out) :: rc
!
!-----------------------------------------------------------------------
!     Local variable declarations 
!-----------------------------------------------------------------------
!
      integer :: i
!
      rc = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!     Set import fields 
!-----------------------------------------------------------------------
!
      do i = 1, ubound(models(Iatmos)%importField, dim=1)
        call NUOPC_Advertise(importState,                               &
             StandardName=trim(models(Iatmos)%importField(i)%long_name),&
             name=trim(models(Iatmos)%importField(i)%short_name), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
                               line=__LINE__, file=FILENAME)) return
      end do 
!
!-----------------------------------------------------------------------
!     Set export fields 
!-----------------------------------------------------------------------
!
      do i = 1, ubound(models(Iatmos)%exportField, dim=1)
        call NUOPC_Advertise(exportState,                               &
             StandardName=trim(models(Iatmos)%exportField(i)%long_name),&
             name=trim(models(Iatmos)%exportField(i)%short_name), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
                               line=__LINE__, file=FILENAME)) return
      end do
!
      end subroutine ATM_SetInitializeP1
!
      subroutine ATM_SetInitializeP2(gcomp, importState, exportState,   &
                                     clock, rc)
!
!-----------------------------------------------------------------------
!     Used module declarations 
!-----------------------------------------------------------------------
!
      use module_wrf_top, only : wrf_init 
!
      implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
      type(ESMF_GridComp) :: gcomp
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      integer, intent(out) :: rc
!
!-----------------------------------------------------------------------
!     Local variable declarations 
!-----------------------------------------------------------------------
!
      real*8 :: tstr
      integer :: comm, localPet, petCount
      type(ESMF_VM) :: vm
      type(ESMF_Time) :: startTime, currTime
!
      rc = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!     Get gridded component 
!-----------------------------------------------------------------------
!
      call ESMF_GridCompGet(gcomp, vm=vm, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      call ESMF_VMGet(vm, localPet=localPet, petCount=petCount,         &
                      mpiCommunicator=comm, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Initialize the gridded component 
!-----------------------------------------------------------------------
!
       call wrf_set_dm_communicator(comm)
       call wrf_init()
!
!-----------------------------------------------------------------------
!     Set-up grid and load coordinate data
!-----------------------------------------------------------------------
!
      call ATM_SetGridArrays2d(gcomp, rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Set-up grid and load coordinate data (3d)
!-----------------------------------------------------------------------
!
      if (models(Icopro)%modActive) then
      call ATM_SetGridArrays3d(gcomp, rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
      end if
!
!-----------------------------------------------------------------------
!     Set-up fields and register to import/export states
!-----------------------------------------------------------------------
!
      call ATM_SetStates2d(gcomp, rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Set-up fields and register to import/export states (3d)
!-----------------------------------------------------------------------
!
      if (models(Icopro)%modActive) then
      call ATM_SetStates3d(gcomp, rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
      end if
!
      end subroutine ATM_SetInitializeP2
!
      subroutine ATM_SetGridArrays2d(gcomp, rc)
!
!-----------------------------------------------------------------------
!     Used module declarations 
!-----------------------------------------------------------------------
!
      use module_domain, only : head_grid, get_ijk_from_grid
      use module_domain, only : find_grid_by_id
      use module_domain_type
      use module_drv, only : drv_allocate
!
      implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
      type(ESMF_GridComp), intent(inout) :: gcomp
      integer :: rc
!
!-----------------------------------------------------------------------
!     Local variable declarations 
!-----------------------------------------------------------------------
!
      integer :: i, j, k, pe
      integer :: ids, ide, jds, jde, kds, kde,                          &
                 ims, ime, jms, jme, kms, kme,                          &
                 ips, ipe, jps, jpe, kps, kpe
      integer :: localPet, petCount 
      integer :: numprocsX, numprocsY
      integer :: lbnd(3), ubnd(3)
      integer :: minIndex(2), maxIndex(2)
      integer, allocatable :: ipatchStarts(:), jpatchStarts(:)
      integer, allocatable :: ipatchEnds(:), jpatchEnds(:)
      integer, allocatable :: deBlockList(:,:,:)
      real(ESMF_KIND_R8), pointer :: ptrX(:,:), ptrY(:,:), ptrA(:,:)
      integer(ESMF_KIND_I4), pointer :: ptrM(:,:)
      character(ESMF_MAXSTR) :: cname, name
      type(domain), pointer :: grid
!
      type(ESMF_VM) :: vm
      type(ESMF_DistGrid) :: distGrid
!
      rc = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!     Get gridded component 
!-----------------------------------------------------------------------
!
      call ESMF_GridCompGet(gcomp, vm=vm, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Get variables related with grid partitioning
!     ids, ide, jds, jde, kds, kde => domain extent
!     ims, ime, jms, jme, kms, kme => memory extent
!     ips, ipe, jps, jpe, kps, kpe => patch extent
!-----------------------------------------------------------------------
!
      call get_ijk_from_grid(head_grid, ids, ide, jds, jde, kds, kde,   &
                                        ims, ime, jms, jme, kms, kme,   &
                                        ips, ipe, jps, jpe, kps, kpe)
!
!-----------------------------------------------------------------------
!     Allocate import arrays in WRF side
!-----------------------------------------------------------------------
!
      call drv_allocate(head_grid)
!
!-----------------------------------------------------------------------
!     Calculate patchs and number of CPUs in each direction 
!-----------------------------------------------------------------------
!
      if (.not. allocated(ipatchStarts)) then
        allocate(ipatchStarts(0:petCount-1))
      end if
!
      if (.not. allocated(jpatchStarts)) then
        allocate(jpatchStarts(0:petCount-1))
      end if
!
      call ESMF_VMAllGatherV(vm, sendData=(/ ips /), sendCount=1,       &
                             recvData=ipatchStarts,                     &
                             recvCounts=(/ (1, k = 0, petCount-1) /),   &
                             recvOffsets=(/ (k, k = 0, petCount-1) /),  &
                             rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      call ESMF_VMAllGatherV(vm, sendData=(/ jps /), sendCount=1,       &
                             recvData=jpatchStarts,                     &
                             recvCounts=(/ (1, k = 0, petCount-1) /),   &
                             recvOffsets=(/ (k, k = 0, petCount-1) /),  &
                             rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      if (.not. allocated(ipatchEnds)) then
        allocate(ipatchEnds(0:petCount-1))
      end if
!
      if (.not. allocated(jpatchEnds)) then
        allocate(jpatchEnds(0:petCount-1))
      end if
!
      call ESMF_VMAllGatherV(vm, sendData=(/ ipe /),                    &
                             sendCount=1, recvData=ipatchEnds,          &
                             recvCounts=(/ (1, k = 0, petCount-1) /),   &
                             recvOffsets=(/ (k, k = 0, petCount-1) /),  &
                             rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      call ESMF_VMAllGatherV(vm, sendData=(/ jpe /),                    &
                             sendCount=1, recvData=jpatchEnds,          &
                             recvCounts=(/ (1, k = 0, petCount-1) /),   &
                             recvOffsets=(/ (k, k = 0, petCount-1) /),  &
                             rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Create ESMF DistGrid based on model domain decomposition
!-----------------------------------------------------------------------
!
      if (.not.allocated(deBlockList)) then
        allocate(deBlockList(2,2,petCount))
      end if
!
      do pe = 1, petCount
        deBlockList(1,1,pe) = ipatchStarts(pe-1)
        deBlockList(1,2,pe) = ipatchEnds(pe-1)
        deBlockList(2,1,pe) = jpatchStarts(pe-1) 
        deBlockList(2,2,pe) = jpatchEnds(pe-1)
      end do
!
      minIndex = (/ minval(ipatchStarts), minval(jpatchStarts) /)
      maxIndex = (/ maxval(ipatchEnds), maxval(jpatchEnds) /)
!
      distGrid = ESMF_DistGridCreate(minIndex=minIndex,                 &
                                     maxIndex=maxIndex,                 &
                                     deBlockList=deBlockList,           &
                                     rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Define component grid (dot and cross points)
!-----------------------------------------------------------------------
!
      if (.not. allocated(models(Iatmos)%mesh)) then
        allocate(models(Iatmos)%mesh(1))
        models(Iatmos)%mesh(1)%gtype = Icross
      end if
!
!-----------------------------------------------------------------------
!     Create ESMF Grid
!-----------------------------------------------------------------------
!
      models(Iatmos)%grid = ESMF_GridCreate(distgrid=distGrid,          &
                                            indexflag=ESMF_INDEX_GLOBAL,&
                                            name="atm_grid2d",          &
                                            rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Allocate coordinates 
!-----------------------------------------------------------------------
!
      call ESMF_GridAddCoord(models(Iatmos)%grid,                       &
                             staggerLoc=ESMF_STAGGERLOC_CENTER,         &
                             rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Allocate items for masking
!-----------------------------------------------------------------------
!
      call ESMF_GridAddItem(models(Iatmos)%grid,                        &
                            staggerLoc=ESMF_STAGGERLOC_CENTER,          &
                            itemflag=ESMF_GRIDITEM_MASK,                &
                            rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Set mask value for land and ocean 
!-----------------------------------------------------------------------
!
      models(Iatmos)%isLand = 1
      models(Iatmos)%isOcean = 0
!
!-----------------------------------------------------------------------
!     Allocate items for grid area 
!-----------------------------------------------------------------------
!
      call ESMF_GridAddItem(models(Iatmos)%grid,                        &
                            staggerLoc=ESMF_STAGGERLOC_CENTER,          &
                            itemflag=ESMF_GRIDITEM_AREA,                &
                            rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Get pointers and set coordinates for the grid 
!-----------------------------------------------------------------------
! 
      call ESMF_GridGetCoord(models(Iatmos)%grid,                       &
                             localDE=0,                                 &
                             staggerLoc=ESMF_STAGGERLOC_CENTER,         &
                             coordDim=1,                                &
                             farrayPtr=ptrX,                            &
                             rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      call ESMF_GridGetCoord(models(Iatmos)%grid,                       &
                             localDE=0,                                 &
                             staggerLoc=ESMF_STAGGERLOC_CENTER,         &
                             coordDim=2,                                &
                             computationalLBound=lbnd,                  &
                             computationalUBound=ubnd,                  &
                             farrayPtr=ptrY,                            &
                             rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      call ESMF_GridGetItem(models(Iatmos)%grid,                        &
                            localDE=0,                                  &
                            staggerLoc=ESMF_STAGGERLOC_CENTER,          &
                            itemflag=ESMF_GRIDITEM_MASK,                &
                            farrayPtr=ptrM,                             &
                            rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      call ESMF_GridGetItem (models(Iatmos)%grid,                       &
                             localDE=0,                                 &
                             staggerLoc=ESMF_STAGGERLOC_CENTER,         &
                             itemflag=ESMF_GRIDITEM_AREA,               &
                             farrayPtr=ptrA,                            &
                             rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Debug: write size of pointers    
!-----------------------------------------------------------------------
!
      name = GRIDDES(models(Iatmos)%mesh(1)%gtype)
!
      if (debugLevel > 0) then
        write(*,30) localPet, 1, adjustl("PTR/ATM/GRD/"//name),         &
                    lbnd(1), ubnd(1), lbnd(2), ubnd(2)   
      end if
!
!-----------------------------------------------------------------------
!     Fill the pointers    
!-----------------------------------------------------------------------
!
      if (debugLevel > 0) then
          write(*,30) localPet, 1, adjustl("DAT/ATM/GRD/"//name),       &
                      lbound(head_grid%xlong, dim=1),                   &
                      ubound(head_grid%xlong, dim=1),                   &
                      lbound(head_grid%xlong, dim=2),                   &
                      ubound(head_grid%xlong, dim=2),                   &
                      head_grid%bdy_mask(1), head_grid%bdy_mask(2),     &
                      head_grid%bdy_mask(3), head_grid%bdy_mask(4)
      end if
!
      do i = lbnd(1), ubnd(1)
        do j = lbnd(2), ubnd(2)
          ptrX(i,j) = head_grid%xlong(i,j)
          ptrY(i,j) = head_grid%xlat(i,j) 
          ptrM(i,j) = head_grid%landmask(i,j) 
          ptrA(i,j) = head_grid%dx*head_grid%dy
        end do
      end do
!
      if (head_grid%bdy_mask(2)) then
        ptrX(ubnd(1),:) = ptrX(ubnd(1)-1,:)+(ptrX(ubnd(1)-1,:)-ptrX(ubnd(1)-2,:))
        ptrY(ubnd(1),:) = ptrY(ubnd(1)-1,:)+(ptrY(ubnd(1)-1,:)-ptrY(ubnd(1)-2,:))
      end if
!
      if (head_grid%bdy_mask(4)) then
         ptrX(:,ubnd(2)) = ptrX(:,ubnd(2)-1)+(ptrX(:,ubnd(2)-1)-ptrX(:,ubnd(2)-2))
         ptrY(:,ubnd(2)) = ptrY(:,ubnd(2)-1)+(ptrY(:,ubnd(2)-1)-ptrY(:,ubnd(2)-2))
      end if
!
!-----------------------------------------------------------------------
!     Nullify pointers 
!-----------------------------------------------------------------------
!
      if (associated(ptrX)) then
        nullify(ptrX)
      end if
      if (associated(ptrY)) then
        nullify(ptrY)
      end if
      if (associated(ptrM)) then
        nullify(ptrM)
      end if
      if (associated(ptrA)) then
        nullify(ptrA)
      end if
!
!-----------------------------------------------------------------------
!     Deallocate arrays    
!-----------------------------------------------------------------------
!
      if (allocated(ipatchStarts)) then
        deallocate(ipatchStarts)
      end if
      if (allocated(jpatchStarts)) then
        deallocate(jpatchStarts)
      end if
      if (allocated(ipatchEnds)) then
        deallocate(ipatchEnds)
      end if
      if (allocated(jpatchEnds)) then
        deallocate(jpatchEnds)
      end if
      if (allocated(deBlockList)) then
        deallocate(deBlockList)
      end if
!
!-----------------------------------------------------------------------
!     Assign grid to gridded component 
!-----------------------------------------------------------------------
!
      call ESMF_GridCompSet(gcomp, grid=models(Iatmos)%grid, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return      
!
!-----------------------------------------------------------------------
!     Debug: write out component grid in VTK format 
!-----------------------------------------------------------------------
!
      if (debugLevel > 1) then
      call ESMF_GridWriteVTK(models(Iatmos)%grid,&
                         filename="atmos_"//&
                         trim(GRIDDES(models(Iatmos)%mesh(1)%gtype))//&
                         "point",&
                         staggerLoc=ESMF_STAGGERLOC_CENTER,&
                         rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,&
                             line=__LINE__, file=FILENAME)) return
      end if
!
!-----------------------------------------------------------------------
!     Format definition 
!-----------------------------------------------------------------------
!
 30   format(" PET(",I3.3,") - DE(",I2.2,") - ", A20, " : ",            &
             4I8," ",L," ",L," ",L," ",L)
!
      end subroutine ATM_SetGridArrays2d
!
      subroutine ATM_SetGridArrays3d(gcomp, rc)
!
!-----------------------------------------------------------------------
!     Used module declarations 
!-----------------------------------------------------------------------
!
      use module_domain, only : head_grid, get_ijk_from_grid
      use module_domain, only : find_grid_by_id
      use module_domain_type
      use module_drv, only : drv_allocate
!
      implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
      type(ESMF_GridComp), intent(inout) :: gcomp
      integer :: rc
!
!-----------------------------------------------------------------------
!     Local variable declarations 
!-----------------------------------------------------------------------
!
      integer :: i, j, k, kz, pe
      integer :: ids, ide, jds, jde, kds, kde,                          &
                 ims, ime, jms, jme, kms, kme,                          &
                 ips, ipe, jps, jpe, kps, kpe
      integer :: localPet, petCount 
      integer :: numprocsX, numprocsY
      integer :: lbnd(3), ubnd(3)
      integer :: minIndex(3), maxIndex(3)
      integer, allocatable :: ipatchStarts(:), jpatchStarts(:), kpatchStarts(:)
      integer, allocatable :: ipatchEnds(:), jpatchEnds(:), kpatchEnds(:)
      integer, allocatable :: deBlockList(:,:,:)
      real(ESMF_KIND_R8), pointer :: ptrX(:,:,:)
      real(ESMF_KIND_R8), pointer :: ptrY(:,:,:)
      real(ESMF_KIND_R8), pointer :: ptrZ(:,:,:)
      real(ESMF_KIND_R8), pointer :: ptrA(:,:,:)
      integer(ESMF_KIND_I4), pointer :: ptrM(:,:,:)
      character(ESMF_MAXSTR) :: cname, name
      type(domain), pointer :: grid
!
      type(ESMF_VM) :: vm
      type(ESMF_DistGrid) :: distGrid
!
      rc = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!     Get gridded component 
!-----------------------------------------------------------------------
!
      call ESMF_GridCompGet(gcomp, vm=vm, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Get variables related with grid partitioning
!     ids, ide, jds, jde, kds, kde => domain extent
!     ims, ime, jms, jme, kms, kme => memory extent
!     ips, ipe, jps, jpe, kps, kpe => patch extent
!-----------------------------------------------------------------------
!
      call get_ijk_from_grid(head_grid, ids, ide, jds, jde, kds, kde,   &
                                        ims, ime, jms, jme, kms, kme,   &
                                        ips, ipe, jps, jpe, kps, kpe)
!
!-----------------------------------------------------------------------
!     Allocate import arrays in WRF side
!-----------------------------------------------------------------------
!
      call drv_allocate(head_grid)
!
!-----------------------------------------------------------------------
!     Calculate patchs and number of CPUs in each direction 
!-----------------------------------------------------------------------
!
      if (.not. allocated(ipatchStarts)) then
        allocate(ipatchStarts(0:petCount-1))
      end if
!
      if (.not. allocated(jpatchStarts)) then
        allocate(jpatchStarts(0:petCount-1))
      end if
!
      if (.not. allocated(kpatchStarts)) then
        allocate(kpatchStarts(0:petCount-1))
      end if
!
      call ESMF_VMAllGatherV(vm, sendData=(/ ips /), sendCount=1,       &
                             recvData=ipatchStarts,                     &
                             recvCounts=(/ (1, k = 0, petCount-1) /),   &
                             recvOffsets=(/ (k, k = 0, petCount-1) /),  &
                             rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      call ESMF_VMAllGatherV(vm, sendData=(/ jps /), sendCount=1,       &
                             recvData=jpatchStarts,                     &
                             recvCounts=(/ (1, k = 0, petCount-1) /),   &
                             recvOffsets=(/ (k, k = 0, petCount-1) /),  &
                             rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      call ESMF_VMAllGatherV(vm, sendData=(/ kps /), sendCount=1,       &
                             recvData=kpatchStarts,                     &
                             recvCounts=(/ (1, k = 0, petCount-1) /),   &
                             recvOffsets=(/ (k, k = 0, petCount-1) /),  &
                             rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      if (.not. allocated(ipatchEnds)) then
        allocate(ipatchEnds(0:petCount-1))
      end if
!
      if (.not. allocated(jpatchEnds)) then
        allocate(jpatchEnds(0:petCount-1))
      end if
!
      if (.not. allocated(kpatchEnds)) then
        allocate(kpatchEnds(0:petCount-1))
      end if
!
      call ESMF_VMAllGatherV(vm, sendData=(/ ipe /),                    &
                             sendCount=1, recvData=ipatchEnds,          &
                             recvCounts=(/ (1, k = 0, petCount-1) /),   &
                             recvOffsets=(/ (k, k = 0, petCount-1) /),  &
                             rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      call ESMF_VMAllGatherV(vm, sendData=(/ jpe /),                    &
                             sendCount=1, recvData=jpatchEnds,          &
                             recvCounts=(/ (1, k = 0, petCount-1) /),   &
                             recvOffsets=(/ (k, k = 0, petCount-1) /),  &
                             rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      call ESMF_VMAllGatherV(vm, sendData=(/ kpe /),                    &
                             sendCount=1, recvData=kpatchEnds,          &
                             recvCounts=(/ (1, k = 0, petCount-1) /),   &
                             recvOffsets=(/ (k, k = 0, petCount-1) /),  &
                             rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Create ESMF DistGrid based on model domain decomposition
!-----------------------------------------------------------------------
!
      kz = models(Iatmos)%nLevs
!
      if (.not.allocated(deBlockList)) then
        allocate(deBlockList(3,2,petCount))
      end if
!
      do pe = 1, petCount
        deBlockList(1,1,pe) = ipatchStarts(pe-1)
        deBlockList(1,2,pe) = ipatchEnds(pe-1)
        deBlockList(2,1,pe) = jpatchStarts(pe-1) 
        deBlockList(2,2,pe) = jpatchEnds(pe-1)
        deBlockList(3,1,pe) = 1 
        deBlockList(3,2,pe) = kz
      end do
!
      minIndex = (/ minval(ipatchStarts), minval(jpatchStarts), 1 /)
      maxIndex = (/ maxval(ipatchEnds), maxval(jpatchEnds), kz /)
!
      distGrid = ESMF_DistGridCreate(minIndex=minIndex,                 &
                                     maxIndex=maxIndex,                 &
                                     deBlockList=deBlockList,           &
                                     rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Define component grid (dot and cross points)
!-----------------------------------------------------------------------
!
      if (.not. allocated(models(Iatmos)%mesh)) then
        allocate(models(Iatmos)%mesh(1))
        models(Iatmos)%mesh(1)%gtype = Icross
      end if
!
!-----------------------------------------------------------------------
!     Create ESMF Grid
!-----------------------------------------------------------------------
!
      models(Iatmos)%grid3d = ESMF_GridCreate(distgrid=distGrid,        &
                                            indexflag=ESMF_INDEX_GLOBAL,&
                                            name="atm_grid3d",          &
                                            rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Allocate coordinates 
!-----------------------------------------------------------------------
!
      call ESMF_GridAddCoord(models(Iatmos)%grid3d,                     &
                             staggerLoc=ESMF_STAGGERLOC_CENTER,         &
                             rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Allocate items for masking
!-----------------------------------------------------------------------
!
      call ESMF_GridAddItem(models(Iatmos)%grid3d,                      &
                            staggerLoc=ESMF_STAGGERLOC_CENTER,          &
                            itemflag=ESMF_GRIDITEM_MASK,                &
                            rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Set mask value for land and ocean 
!-----------------------------------------------------------------------
!
      models(Iatmos)%isLand = 1
      models(Iatmos)%isOcean = 0
!
!-----------------------------------------------------------------------
!     Allocate items for grid area 
!-----------------------------------------------------------------------
!
      call ESMF_GridAddItem(models(Iatmos)%grid3d,                      &
                            staggerLoc=ESMF_STAGGERLOC_CENTER,          &
                            itemflag=ESMF_GRIDITEM_AREA,                &
                            rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Get pointers and set coordinates for the grid 
!-----------------------------------------------------------------------
! 
      call ESMF_GridGetCoord(models(Iatmos)%grid3d,                     &
                             localDE=0,                                 &
                             staggerLoc=ESMF_STAGGERLOC_CENTER,         &
                             coordDim=1,                                &
                             farrayPtr=ptrX,                            &
                             rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      call ESMF_GridGetCoord(models(Iatmos)%grid3d,                     &
                             localDE=0,                                 &
                             staggerLoc=ESMF_STAGGERLOC_CENTER,         &
                             coordDim=2,                                &
                             farrayPtr=ptrY,                            &
                             rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      call ESMF_GridGetCoord(models(Iatmos)%grid3d,                     &
                             localDE=0,                                 &
                             staggerLoc=ESMF_STAGGERLOC_CENTER,         &
                             coordDim=3,                                &
                             computationalLBound=lbnd,                  &
                             computationalUBound=ubnd,                  &
                             farrayPtr=ptrZ,                            &
                             rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      call ESMF_GridGetItem(models(Iatmos)%grid3d,                      &
                            localDE=0,                                  &
                            staggerLoc=ESMF_STAGGERLOC_CENTER,          &
                            itemflag=ESMF_GRIDITEM_MASK,                &
                            farrayPtr=ptrM,                             &
                            rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      call ESMF_GridGetItem (models(Iatmos)%grid3d,                     &
                             localDE=0,                                 &
                             staggerLoc=ESMF_STAGGERLOC_CENTER,         &
                             itemflag=ESMF_GRIDITEM_AREA,               &
                             farrayPtr=ptrA,                            &
                             rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Debug: write size of pointers    
!-----------------------------------------------------------------------
!
      name = GRIDDES(models(Iatmos)%mesh(1)%gtype)
!
      if (debugLevel > 0) then
        write(*,110) localPet, 1, adjustl("PTR/ATM/GRD/"//name),        &
                    lbnd(1), ubnd(1), lbnd(2), ubnd(2), lbnd(3), ubnd(3)
      end if
!
!-----------------------------------------------------------------------
!     Fill the pointers    
!-----------------------------------------------------------------------
!
      if (debugLevel > 0) then
          write(*,110) localPet, 1, adjustl("DAT/ATM/GRD/"//name),      &
                      lbound(head_grid%xlong, dim=1),                   &
                      ubound(head_grid%xlong, dim=1),                   &
                      lbound(head_grid%xlong, dim=2),                   &
                      ubound(head_grid%xlong, dim=2),                   &
                      lbnd(3),                                          &
                      ubnd(3),                                          &
                      head_grid%bdy_mask(1), head_grid%bdy_mask(2),     &
                      head_grid%bdy_mask(3), head_grid%bdy_mask(4)
      end if
!
      do k = 1, kz 
        do i = lbnd(1), ubnd(1)
          do j = lbnd(2), ubnd(2)
            ptrX(i,j,k) = head_grid%xlong(i,j)
            ptrY(i,j,k) = head_grid%xlat(i,j) 
            ptrZ(i,j,k) = models(Iatmos)%levs(k) 
            ptrM(i,j,k) = head_grid%landmask(i,j) 
            ptrA(i,j,k) = head_grid%dx*head_grid%dy
          end do
        end do
      end do
!
      if (head_grid%bdy_mask(2)) then
        do k = 1, kz 
        ptrX(ubnd(1),:,k) = ptrX(ubnd(1)-1,:,k)+(ptrX(ubnd(1)-1,:,k)-ptrX(ubnd(1)-2,:,k))
        ptrY(ubnd(1),:,k) = ptrY(ubnd(1)-1,:,k)+(ptrY(ubnd(1)-1,:,k)-ptrY(ubnd(1)-2,:,k))
        ptrZ(ubnd(1),:,k) = models(Iatmos)%levs(k) 
        end do
      end if
!
      if (head_grid%bdy_mask(4)) then
        do k = 1, kz 
        ptrX(:,ubnd(2),k) = ptrX(:,ubnd(2)-1,k)+(ptrX(:,ubnd(2)-1,k)-ptrX(:,ubnd(2)-2,k))
        ptrY(:,ubnd(2),k) = ptrY(:,ubnd(2)-1,k)+(ptrY(:,ubnd(2)-1,k)-ptrY(:,ubnd(2)-2,k))
        ptrZ(:,ubnd(2),k) = models(Iatmos)%levs(k) 
        end do
      end if
!
!-----------------------------------------------------------------------
!     Nullify pointers 
!-----------------------------------------------------------------------
!
      if (associated(ptrX)) then
        nullify(ptrX)
      end if
      if (associated(ptrY)) then
        nullify(ptrY)
      end if
      if (associated(ptrZ)) then
        nullify(ptrZ)
      end if
      if (associated(ptrM)) then
        nullify(ptrM)
      end if
      if (associated(ptrA)) then
        nullify(ptrA)
      end if
!
!-----------------------------------------------------------------------
!     Deallocate arrays    
!-----------------------------------------------------------------------
!
      if (allocated(ipatchStarts)) then
        deallocate(ipatchStarts)
      end if
      if (allocated(jpatchStarts)) then
        deallocate(jpatchStarts)
      end if
      if (allocated(kpatchStarts)) then
        deallocate(kpatchStarts)
      end if
      if (allocated(ipatchEnds)) then
        deallocate(ipatchEnds)
      end if
      if (allocated(jpatchEnds)) then
        deallocate(jpatchEnds)
      end if
      if (allocated(kpatchEnds)) then
        deallocate(kpatchEnds)
      end if
      if (allocated(deBlockList)) then
        deallocate(deBlockList)
      end if
!
!-----------------------------------------------------------------------
!     Debug: write out component grid in VTK format 
!-----------------------------------------------------------------------
!
      if (debugLevel > 1) then
      call ESMF_GridWriteVTK(models(Iatmos)%grid,                       &
                         filename="atmos_"//                            &
                         trim(GRIDDES(models(Iatmos)%mesh(1)%gtype))//  &
                         "point",&
                         staggerLoc=ESMF_STAGGERLOC_CENTER,             &
                         rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
      end if
!
!-----------------------------------------------------------------------
!     Format definition 
!-----------------------------------------------------------------------
!
 110  format(" PET(",I3.3,") - DE(",I2.2,") - ", A20, " : ",            &
             6I8," ",L," ",L," ",L," ",L)
!
      end subroutine ATM_SetGridArrays3d
!
      subroutine ATM_SetStates2d(gcomp, rc)
      implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
      type(ESMF_GridComp) :: gcomp
      integer, intent(out) :: rc
!
!-----------------------------------------------------------------------
!     Local variable declarations 
!-----------------------------------------------------------------------
!
      integer :: i, j, k, localPet, petCount, itemCount, localDECount
      real*8, dimension(:,:), pointer :: ptr
      character(ESMF_MAXSTR), allocatable :: itemNameList(:)
!
      type(ESMF_VM) :: vm
      type(ESMF_Field) :: field
      type(ESMF_ArraySpec) :: arraySpec
      type(ESMF_StaggerLoc) :: staggerLoc 
      type(ESMF_State) :: importState, exportState
!
      rc = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!     Get gridded component 
!-----------------------------------------------------------------------
!
      call ESMF_GridCompGet(gcomp, importState=importState,             &
                            exportState=exportState, vm=vm, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Set array descriptor
!-----------------------------------------------------------------------
!
      call ESMF_ArraySpecSet(arraySpec, typekind=ESMF_TYPEKIND_R8,      &
                             rank=2, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Get number of local DEs
!-----------------------------------------------------------------------
! 
      call ESMF_GridGet(models(Iatmos)%grid,                            &
                        localDECount=localDECount,                      &
                        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Get list of export fields 
!-----------------------------------------------------------------------
!
      call ESMF_StateGet(exportState, itemCount=itemCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      if (.not. allocated(itemNameList)) then
        allocate(itemNameList(itemCount))
      end if
!
      call ESMF_StateGet(exportState, itemNameList=itemNameList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Create export fields 
!-----------------------------------------------------------------------
!
      do i = 1, itemCount
      k = get_varid(models(Iatmos)%exportField, trim(itemNameList(i)))
!
!-----------------------------------------------------------------------
!     Set staggering type 
!-----------------------------------------------------------------------
!
      if (models(Iatmos)%exportField(k)%gtype == Icross) then
        staggerLoc = ESMF_STAGGERLOC_CENTER
      end if
!
!-----------------------------------------------------------------------
!     Create field 
!-----------------------------------------------------------------------
!
      field = ESMF_FieldCreate(models(Iatmos)%grid,                     &
                               arraySpec,                               &
                               staggerloc=staggerLoc,                   &
                               name=trim(itemNameList(i)),              &
                               rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Put data into state 
!-----------------------------------------------------------------------
! 
      do j = 0, localDECount-1
!
!-----------------------------------------------------------------------
!     Get pointer from field 
!-----------------------------------------------------------------------
!
      call ESMF_FieldGet(field, localDe=j, farrayPtr=ptr, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Initialize pointer 
!-----------------------------------------------------------------------
!
      ptr = MISSING_R8
!
!-----------------------------------------------------------------------
!     Nullify pointer to make sure that it does not point on a random 
!     part in the memory 
!-----------------------------------------------------------------------
!
      if (associated(ptr)) then
        nullify(ptr)
      end if
!
      end do
!
!-----------------------------------------------------------------------
!     Add field export state
!-----------------------------------------------------------------------
!
      call NUOPC_Realize(exportState, field=field, rc=rc) 
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
      end do
!
!-----------------------------------------------------------------------
!     Deallocate arrays    
!-----------------------------------------------------------------------
!
      if (allocated(itemNameList)) deallocate(itemNameList) 
!
!-----------------------------------------------------------------------
!     Get list of import fields 
!-----------------------------------------------------------------------
!
      call ESMF_StateGet(importState, itemCount=itemCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      if (.not. allocated(itemNameList)) then
        allocate(itemNameList(itemCount))
      end if
!
      call ESMF_StateGet(importState, itemNameList=itemNameList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Create import fields 
!-----------------------------------------------------------------------
!
      do i = 1, itemCount
      k = get_varid(models(Iatmos)%importField, trim(itemNameList(i)))
!
!-----------------------------------------------------------------------
!     Set staggering type 
!-----------------------------------------------------------------------
!
      if (models(Iatmos)%importField(k)%gtype == Icross) then
        staggerLoc = ESMF_STAGGERLOC_CENTER
      end if
!
!-----------------------------------------------------------------------
!     Create field 
!-----------------------------------------------------------------------
!
      field = ESMF_FieldCreate(models(Iatmos)%grid,                     &
                               arraySpec,                               &
                               staggerloc=staggerLoc,                   &
                               name=trim(itemNameList(i)),              &
                               rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Put data into state 
!-----------------------------------------------------------------------
! 
      do j = 0, localDECount-1
!
!-----------------------------------------------------------------------
!     Get pointer from field 
!-----------------------------------------------------------------------
!
      call ESMF_FieldGet(field, localDe=j, farrayPtr=ptr, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Initialize pointer 
!-----------------------------------------------------------------------
!
      ptr = MISSING_R8
!
!-----------------------------------------------------------------------
!     Nullify pointer to make sure that it does not point on a random 
!     part in the memory 
!-----------------------------------------------------------------------
!
      if (associated(ptr)) then
        nullify(ptr)
      end if
!
      end do
!
!-----------------------------------------------------------------------
!     Add field import state
!-----------------------------------------------------------------------
!
      call NUOPC_Realize(importState, field=field, rc=rc) 
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
      end do
!
!-----------------------------------------------------------------------
!     Deallocate arrays    
!-----------------------------------------------------------------------
!
      if (allocated(itemNameList)) deallocate(itemNameList)
!
      end subroutine ATM_SetStates2d
!
      subroutine ATM_SetStates3d(gcomp, rc)
      implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
      type(ESMF_GridComp) :: gcomp
      integer, intent(out) :: rc
!
!-----------------------------------------------------------------------
!     Local variable declarations 
!-----------------------------------------------------------------------
!
      integer :: i, j, k, localPet, petCount, itemCount, localDECount
      real*8, dimension(:,:,:), pointer :: ptr
      character(ESMF_MAXSTR), allocatable :: itemNameList(:)
!
      type(ESMF_VM) :: vm
      type(ESMF_Field) :: field
      type(ESMF_ArraySpec) :: arraySpec
      type(ESMF_StaggerLoc) :: staggerLoc 
      type(ESMF_State) :: importState, exportState
!
      rc = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!     Get gridded component 
!-----------------------------------------------------------------------
!
      call ESMF_GridCompGet(gcomp, importState=importState,             &
                            exportState=exportState, vm=vm, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Set array descriptor
!-----------------------------------------------------------------------
!
      call ESMF_ArraySpecSet(arraySpec, typekind=ESMF_TYPEKIND_R8,      &
                             rank=3, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Get number of local DEs
!-----------------------------------------------------------------------
! 
      call ESMF_GridGet(models(Iatmos)%grid3d,                          &
                        localDECount=localDECount,                      &
                        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Get list of export fields 
!-----------------------------------------------------------------------
!
      call ESMF_StateGet(exportState, itemCount=itemCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      if (.not. allocated(itemNameList)) then
        allocate(itemNameList(itemCount))
      end if
!
      call ESMF_StateGet(exportState, itemNameList=itemNameList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Create export fields 
!-----------------------------------------------------------------------
!
      do i = 1, itemCount
      k = get_varid(models(Iatmos)%exportField, trim(itemNameList(i)))
!
!-----------------------------------------------------------------------
!     Check rank of the export field 
!-----------------------------------------------------------------------
!
      if (models(Iatmos)%exportField(k)%rank .eq. 3) then
!
!-----------------------------------------------------------------------
!     Set staggering type 
!-----------------------------------------------------------------------
!
      if (models(Iatmos)%exportField(k)%gtype == Icross) then
        staggerLoc = ESMF_STAGGERLOC_CENTER
      end if
!
!-----------------------------------------------------------------------
!     Create field 
!-----------------------------------------------------------------------
!
      field = ESMF_FieldCreate(models(Iatmos)%grid3d,                   &
                               arraySpec,                               &
                               staggerloc=staggerLoc,                   &
                               name=trim(itemNameList(i)),              &
                               rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Put data into state 
!-----------------------------------------------------------------------
! 
      do j = 0, localDECount-1
!
!-----------------------------------------------------------------------
!     Get pointer from field 
!-----------------------------------------------------------------------
!
      call ESMF_FieldGet(field, localDe=j, farrayPtr=ptr, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Initialize pointer 
!-----------------------------------------------------------------------
!
      ptr = MISSING_R8
!
!-----------------------------------------------------------------------
!     Nullify pointer to make sure that it does not point on a random 
!     part in the memory 
!-----------------------------------------------------------------------
!
      if (associated(ptr)) then
        nullify(ptr)
      end if
!
      end do
!
!-----------------------------------------------------------------------
!     Add field export state
!-----------------------------------------------------------------------
!
      call NUOPC_Realize(exportState, field=field, rc=rc) 
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      end if
!
      end do
!
!-----------------------------------------------------------------------
!     Deallocate arrays    
!-----------------------------------------------------------------------
!
      if (allocated(itemNameList)) deallocate(itemNameList) 
!
!-----------------------------------------------------------------------
!     Get list of import fields 
!-----------------------------------------------------------------------
!
      call ESMF_StateGet(importState, itemCount=itemCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      if (.not. allocated(itemNameList)) then
        allocate(itemNameList(itemCount))
      end if
!
      call ESMF_StateGet(importState, itemNameList=itemNameList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Create import fields 
!-----------------------------------------------------------------------
!
      do i = 1, itemCount
      k = get_varid(models(Iatmos)%importField, trim(itemNameList(i)))
!
!-----------------------------------------------------------------------
!     Check rank of the import field 
!-----------------------------------------------------------------------
!
      if (models(Iatmos)%importField(k)%rank .eq. 3) then
!
!-----------------------------------------------------------------------
!     Set staggering type 
!-----------------------------------------------------------------------
!
      if (models(Iatmos)%importField(k)%gtype == Icross) then
        staggerLoc = ESMF_STAGGERLOC_CENTER
      end if
!
!-----------------------------------------------------------------------
!     Create field 
!-----------------------------------------------------------------------
!
      field = ESMF_FieldCreate(models(Iatmos)%grid3d,                   &
                               arraySpec,                               &
                               staggerloc=staggerLoc,                   &
                               name=trim(itemNameList(i)),              &
                               rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Put data into state 
!-----------------------------------------------------------------------
! 
      do j = 0, localDECount-1
!
!-----------------------------------------------------------------------
!     Get pointer from field 
!-----------------------------------------------------------------------
!
      call ESMF_FieldGet(field, localDe=j, farrayPtr=ptr, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Initialize pointer 
!-----------------------------------------------------------------------
!
      ptr = MISSING_R8
!
!-----------------------------------------------------------------------
!     Nullify pointer to make sure that it does not point on a random 
!     part in the memory 
!-----------------------------------------------------------------------
!
      if (associated(ptr)) then
        nullify(ptr)
      end if
!
      end do
!
!-----------------------------------------------------------------------
!     Add field import state
!-----------------------------------------------------------------------
!
      call NUOPC_Realize(importState, field=field, rc=rc) 
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      end if
!
      end do
!
!-----------------------------------------------------------------------
!     Deallocate arrays    
!-----------------------------------------------------------------------
!
      if (allocated(itemNameList)) deallocate(itemNameList)
!
      end subroutine ATM_SetStates3d
!
      subroutine ATM_DataInit(gcomp, rc)
      implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
      type(ESMF_GridComp) :: gcomp
      integer, intent(out) :: rc
!
!-----------------------------------------------------------------------
!     Local variable declarations 
!-----------------------------------------------------------------------
!
      integer :: localPet, petCount, phase
      real(ESMF_KIND_R8) :: tstr
      character(ESMF_MAXSTR) :: str1, str2
!
      type(ESMF_VM) :: vm
      type(ESMF_Clock) :: clock
      type(ESMF_Time) :: currTime
      type(ESMF_TimeInterval) :: timeStep
!
!-----------------------------------------------------------------------
!     Get gridded component clock
!-----------------------------------------------------------------------
!
      call ESMF_GridCompGet(gcomp, clock=clock, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      call ESMF_ClockGet(clock, currTime=currTime, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Put export fields (only for restart run)
!-----------------------------------------------------------------------
!
      if (cplType == 1 .and. restarted .and. currTime == esmRestartTime) then
        call ATM_Put(gcomp, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
                               line=__LINE__, file=FILENAME)) return
      end if
!
      end subroutine ATM_DataInit
!
      subroutine ATM_SetClock(gcomp, rc)
      implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
      type(ESMF_GridComp) :: gcomp
      integer, intent(out) :: rc
!
!-----------------------------------------------------------------------
!     Local variable declarations 
!-----------------------------------------------------------------------
!
      integer :: fac1, fac2, maxdiv
      integer :: ref_year,   str_year,   end_year
      integer :: ref_month,  str_month,  end_month
      integer :: ref_day,    str_day,    end_day
      integer :: ref_hour,   str_hour,   end_hour
      integer :: ref_minute, str_minute, end_minute
      integer :: ref_second, str_second, end_second
      integer :: run_days, run_hours, run_minutes, run_seconds
!
      type(ESMF_Calendar) :: cal
      type(ESMF_Clock) :: cmpClock 
      type(ESMF_TimeInterval) :: timeStep
      type(ESMF_Time) :: startTime, currTime
      type(ESMF_Time) :: cmpRefTime, cmpStartTime, cmpStopTime
!
      rc = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!     Create gridded component clock 
!-----------------------------------------------------------------------
!
      cal = ESMF_CalendarCreate(ESMF_CALKIND_GREGORIAN,                 &
                                name="gregorian",                       &
                                rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Set reference time
!     place head_grid%id for 1
!-----------------------------------------------------------------------
!
      call nl_get_simulation_start_year(1, ref_year)
      call nl_get_simulation_start_month(1, ref_month)
      call nl_get_simulation_start_day(1, ref_day)
      call nl_get_simulation_start_hour(1, ref_hour)
      call nl_get_simulation_start_minute(1, ref_minute)
      call nl_get_simulation_start_second(1, ref_second)
!
!      write(*, fmt="(A,6I5)") "REF TIME :: ", ref_year, ref_month, ref_day, ref_hour, ref_minute, ref_second
!
      call ESMF_TimeSet(cmpRefTime,                                     &
                        yy=ref_year,                                    &
                        mm=ref_month,                                   &
                        dd=ref_day,                                     &
                        h=ref_hour,                                     &
                        m=ref_minute,                                   &
                        s=ref_second,                                   &
                        calendar=cal,                                   &
                        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,&
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Set start time
!-----------------------------------------------------------------------
!
      call nl_get_start_year(1, str_year)
      call nl_get_start_month(1, str_month)
      call nl_get_start_day(1, str_day)
      call nl_get_start_hour(1, str_hour)
      call nl_get_start_minute(1, str_minute)
      call nl_get_start_second(1, str_second)
!
!      write(*, fmt="(A,6I5)") "STR TIME :: ", str_year, str_month, str_day, str_hour, str_minute, str_second
!
      call ESMF_TimeSet(cmpStartTime,                                   &
                        yy=str_year,                                    &
                        mm=str_month,                                   &
                        dd=str_day,                                     &
                        h=str_hour,                                     &
                        m=str_minute,                                   &
                        s=str_second,                                   &
                        calendar=cal,                                   &
                        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Set stop time
!-----------------------------------------------------------------------
!
      call nl_get_end_year(1, end_year)
      call nl_get_end_month(1, end_month)
      call nl_get_end_day(1, end_day)
      call nl_get_end_hour(1, end_hour)
      call nl_get_end_minute(1, end_minute)
      call nl_get_end_second(1, end_second)
!
!      write(*, fmt="(A,6I5)") "END TIME :: ", end_year, end_month, end_day, end_hour, end_minute, end_second
!
      call ESMF_TimeSet(cmpStopTime,                                    &
                        yy=end_year,                                    &
                        mm=end_month,                                   &
                        dd=end_day,                                     &
                        h=end_hour,                                     &
                        m=end_minute,                                   &
                        s=end_second,                                   &
                        calendar=cal,                                   &
                        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Get component clock
!-----------------------------------------------------------------------
!
      call ESMF_GridCompGet(gcomp, clock=cmpClock, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      call ESMF_ClockGet(cmpClock, timeStep=timeStep,                   &
                         currTime=currTime, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Compare driver time vs. component time
!-----------------------------------------------------------------------
!
!      if (restarted) then
!        startTime = esmRestartTime
!      else
!        startTime = esmStartTime
!      end if
!
!      if (cmpStartTime /= startTime) then
!        call ESMF_TimePrint(cmpStartTime, options="string isofrac", rc=rc)
!        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
!                               line=__LINE__, file=FILENAME)) return
!
!        call ESMF_TimePrint(startTime, options="string isofrac", rc=rc)
!        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
!                               line=__LINE__, file=FILENAME)) return
!
!        call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc,              &
!             msg='ESM and ATM start times do not match: '//             &
!             'please check the config files')
!        return
!      end if
!
!      if (cmpStopTime /= esmStopTime) then
!        call ESMF_TimePrint(cmpStopTime, options="string", rc=rc)
!        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
!                               line=__LINE__, file=FILENAME)) return
!
!        call ESMF_TimePrint(esmStopTime, options="string", rc=rc)
!        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
!                               line=__LINE__, file=FILENAME)) return
!
!        call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc,              &
!             msg='ESM and ATM stop times do not match: '//              &
!             'please check the config files')
!        return
!      end if
!
      if (cal /= esmCal) then
        call ESMF_CalendarPrint(cal, options="calkindflag", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
                               line=__LINE__, file=FILENAME)) return
!
        call ESMF_CalendarPrint(esmCal, options="calkindflag", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
                               line=__LINE__, file=FILENAME)) return
!
        call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc,              &
             msg='ESM and ATM calendars do not match: '//               &
             'please check the config files')
        return
      end if
!
!-----------------------------------------------------------------------
!     Modify component clock time step 
!-----------------------------------------------------------------------
!
      fac1 = maxval(connectors(Iatmos,:)%divDT,mask=models(:)%modActive)
      fac2 = maxval(connectors(:,Iatmos)%divDT,mask=models(:)%modActive)
      maxdiv = max(fac1, fac2)
!
      call ESMF_ClockSet(cmpClock, name='atm_clock',                    &
                         timeStep=timeStep/maxdiv, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      end subroutine ATM_SetClock
!
      subroutine ATM_ModelAdvance(gcomp, rc)
!
!-----------------------------------------------------------------------
!     Used module declarations 
!-----------------------------------------------------------------------
!
      use module_domain, only : head_grid
      use module_wrf_top, only : wrf_run
      use MYESMF_TimeMod, only : MYESMF_Time
      use module_symbols_util, only : WRFU_TimeSet
!
      implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
      type(ESMF_GridComp) :: gcomp
      integer, intent(out) :: rc
!
!-----------------------------------------------------------------------
!     Local variable declarations 
!-----------------------------------------------------------------------
!
      integer :: tstr, tend
      integer :: localPet, petCount, phase
      integer :: yy, mm, dd, h, m, s
      character(ESMF_MAXSTR) :: str1, str2
!
      type(MYESMF_Time) :: timeFrom2, timeTo2
!     
      type(ESMF_VM) :: vm
      type(ESMF_Clock) :: clock
      type(ESMF_TimeInterval) :: timeStep
      type(ESMF_Time) :: timeFrom1, timeTo1
      type(ESMF_Time) :: refTime, startTime, stopTime, currTime
      type(ESMF_State) :: importState, exportState
!
      rc = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!     Get gridded component 
!-----------------------------------------------------------------------
!
      call ESMF_GridCompGet(gcomp, clock=clock, importState=importState,&
                            exportState=exportState, currentPhase=phase,&
                            vm=vm, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Get start, stop and current time and time step
!-----------------------------------------------------------------------
!
      call ESMF_ClockGet(clock, timeStep=timeStep, refTime=refTime,     &
                         startTime=startTime, stopTime=stopTime,        &
                         currTime=currTime, rc=rc) 
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Calculate run time
!-----------------------------------------------------------------------
!
      if (restarted .and. (currTime-esmRestartTime) < timeStep) then
        timeFrom1 = esmRestartTime
      else
        timeFrom1 = currTime
      end if
      timeTo1 = timeFrom1+timeStep
!
!-----------------------------------------------------------------------
!     Debug: write time information 
!-----------------------------------------------------------------------
!
      if (debugLevel >= 0 .and. localPet == 0) then
        call ESMF_TimeGet(currTime,                                     &
                          timeStringISOFrac=str1, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
                               line=__LINE__, file=FILENAME)) return
!
        call ESMF_TimeGet(currTime+timeStep,                            &
                          timeStringISOFrac=str2, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
                               line=__LINE__, file=FILENAME)) return
!
        write(*,50) trim(str1), trim(str2), phase
      end if
!
!-----------------------------------------------------------------------
!     Convert ESMF_Time to MYESMF_Time
!     MYESMF_Time is the old version of ESMF_Time implemented in WRF
!-----------------------------------------------------------------------
!
      call ESMF_TimeGet(timeFrom1,                                      &
                        yy=yy,                                          &
                        mm=mm,                                          &
                        dd=dd,                                          &
                        h=h,                                            &
                        m=m,                                            &
                        s=s,                                            &
                        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      call WRFU_TimeSet(timeFrom2,                                      &
                        yy=yy,                                          &
                        mm=mm,                                          &
                        dd=dd,                                          &
                        h=h,                                            &
                        m=m,                                            &
                        s=s,                                            &
                        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      call ESMF_TimeGet(timeTo1,                                        &
                        yy=yy,                                          &
                        mm=mm,                                          &
                        dd=dd,                                          &
                        h=h,                                            &
                        m=m,                                            &
                        s=s,                                            &
                        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      call WRFU_TimeSet(timeTo2,                                        &
                        yy=yy,                                          &
                        mm=mm,                                          &
                        dd=dd,                                          &
                        h=h,                                            &
                        m=m,                                            &
                        s=s,                                            &
                        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Get import fields 
!-----------------------------------------------------------------------
!
      if (cplType == 1) then
        call ATM_Get(gcomp, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
                               line=__LINE__, file=FILENAME)) return
      else
        if ((currTime-esmRestartTime) >= timeStep) then
          call ATM_Get(gcomp, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,&
                                 line=__LINE__, file=FILENAME)) return
        end if
      end if
!
!-----------------------------------------------------------------------
!     Run ATM component
!-----------------------------------------------------------------------
!
      head_grid%start_subtime = timeFrom2 
      head_grid%stop_subtime = timeTo2
!
      call wrf_run()
!
!-----------------------------------------------------------------------
!     Put export fields 
!-----------------------------------------------------------------------
!
      call ATM_Put(gcomp, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Formats 
!-----------------------------------------------------------------------
!
 50   format(' Running ATM Component: ',A,' --> ',A,' Phase: ',I1)
!
      end subroutine ATM_ModelAdvance
!
      subroutine ATM_SetFinalize(gcomp, importState, exportState,       &
                                 clock, rc)
!
!-----------------------------------------------------------------------
!     Used module declarations 
!-----------------------------------------------------------------------
!
      use module_wrf_top, only : wrf_finalize
!
      implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
      type(ESMF_GridComp) :: gcomp
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      integer, intent(out) :: rc
!
!-----------------------------------------------------------------------
!     Local variable declarations 
!-----------------------------------------------------------------------
!
      rc = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!     Call model finalize routines
!-----------------------------------------------------------------------
!
      call wrf_finalize()
!
      end subroutine ATM_SetFinalize
!
      subroutine ATM_Get(gcomp, rc)
!
!-----------------------------------------------------------------------
!     Used module declarations 
!-----------------------------------------------------------------------
!
      use module_domain, only : head_grid, get_ijk_from_grid
      use module_drv, only : wrf_import
!
      implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
      type(ESMF_GridComp) :: gcomp
      integer, intent(out) :: rc
!
!-----------------------------------------------------------------------
!     Local variable declarations 
!-----------------------------------------------------------------------
!
      integer :: ids, ide, jds, jde, kds, kde,                          &
                 ims, ime, jms, jme, kms, kme,                          &
                 ips, ipe, jps, jpe, kps, kpe
      integer :: i, j, ii, jj, n, m, id, imin, imax, jmin, jmax
      integer :: iyear, iday, imonth, ihour, iminute, isec, iunit
      integer :: localPet, petCount, itemCount, localDECount
      character(ESMF_MAXSTR) :: cname, ofile
      character(ESMF_MAXSTR), allocatable :: itemNameList(:)
      real(ESMF_KIND_R8) :: sfac, addo
      real(ESMF_KIND_R8), pointer :: ptr(:,:)
!
      type(ESMF_VM) :: vm
      type(ESMF_Clock) :: clock
      type(ESMF_Time) :: currTime
      type(ESMF_Field) :: field
      type(ESMF_State) :: importState
      type(ESMF_StateItem_Flag), allocatable :: itemTypeList(:)      
!
      rc = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!     Get gridded component 
!-----------------------------------------------------------------------
!
      call ESMF_GridCompGet(gcomp, name=cname, clock=clock,             &
                            importState=importState, vm=vm, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Get current time 
!-----------------------------------------------------------------------
!
      if (debugLevel > 2) then
      call ESMF_ClockGet(clock, currTime=currTime, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      call ESMF_TimeGet(currTime, yy=iyear, mm=imonth,                  &
                        dd=iday, h=ihour, m=iminute, s=isec, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
      end if
!
!-----------------------------------------------------------------------
!     Get number of local DEs
!-----------------------------------------------------------------------
! 
      call ESMF_GridGet(models(Iatmos)%grid,                            &
                        localDECount=localDECount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Get list of import fields 
!-----------------------------------------------------------------------
!
      call ESMF_StateGet(importState, itemCount=itemCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      if (.not. allocated(itemNameList)) then
        allocate(itemNameList(itemCount))
      end if
      if (.not. allocated(itemTypeList)) then
        allocate(itemTypeList(itemCount))
      end if
      call ESMF_StateGet(importState, itemNameList=itemNameList,        &
                         itemTypeList=itemTypeList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Get variables related with grid partitioning
!     ids, ide, jds, jde, kds, kde => domain extent
!     ims, ime, jms, jme, kms, kme => memory extent
!     ips, ipe, jps, jpe, kps, kpe => patch extent
!-----------------------------------------------------------------------
!
      call get_ijk_from_grid(head_grid, ids, ide, jds, jde, kds, kde,   &
                                        ims, ime, jms, jme, kms, kme,   &
                                        ips, ipe, jps, jpe, kps, kpe)
!
!-----------------------------------------------------------------------
!     Loop over excahange fields 
!-----------------------------------------------------------------------
!
      do i = 1, itemCount
!
      id = get_varid(models(Iatmos)%importField, itemNameList(i))
!
!-----------------------------------------------------------------------
!     Get field
!-----------------------------------------------------------------------
!
      call ESMF_StateGet(importState, trim(itemNameList(i)),            &
                         field, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Loop over decomposition elements (DEs) 
!-----------------------------------------------------------------------
!
      do j = 0, localDECount-1
!
!-----------------------------------------------------------------------
!     Get pointer from field
!-----------------------------------------------------------------------
!
      call ESMF_FieldGet(field, localDE=j, farrayPtr=ptr, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Debug: write size of pointers    
!-----------------------------------------------------------------------
!
      if (debugLevel > 1) then
      write(*,60) localPet, j, adjustl("PTR/ATM/IMP/"//itemNameList(i)),&
                  lbound(ptr, dim=1), ubound(ptr, dim=1),               &
                  lbound(ptr, dim=2), ubound(ptr, dim=2)
      write(*,60) localPet, j, adjustl("IND/ATM/IMP/"//itemNameList(i)),&
                  ips, ipe, jps, jpe 
      end if
!
!-----------------------------------------------------------------------
!     Put data to ATM component variable
!-----------------------------------------------------------------------
!
      sfac = models(Iatmos)%importField(id)%scale_factor
      addo = models(Iatmos)%importField(id)%add_offset
!
      select case (trim(adjustl(itemNameList(i))))
!     Import from OCN 
      case ('sst')
        do m = ips, ipe
          do n = jps, jpe
            if (ptr(m,n) < TOL_R8) then
              wrf_import%sst(m,n) = (ptr(m,n)*sfac)+addo
            else
              wrf_import%sst(m,n) = MISSING_R8
            end if
          end do
        end do
#ifdef OCNICE
      case ('sic')
        do m = ips, ipe
          do n = jps, jpe
            if (ptr(m,n) < TOL_R8) then
              wrf_import%sic(m,n) = (ptr(m,n)*sfac)+addo
            else
              wrf_import%sic(m,n) = MISSING_R8
            end if
          end do
        end do
#endif
      end select

!-----------------------------------------------------------------------
!     Debug: write field in ASCII format   
!-----------------------------------------------------------------------
!
      if (debugLevel == 4) then
        write(ofile,70) 'atm_import', trim(itemNameList(i)),            &
                        iyear, imonth, iday, ihour, localPet, j
        iunit = localPet*10
        open(unit=iunit, file=trim(ofile)//'.txt')
        call print_matrix(ptr, ips, ipe, jps, jpe, 1, 1,                &
                          localPet, iunit, "PTR/ATM/IMP")
        close(unit=iunit)
      end if
!
!-----------------------------------------------------------------------
!     Nullify pointer to make sure that it does not point on a random 
!     part in the memory 
!-----------------------------------------------------------------------
!
      if (associated(ptr)) then
        nullify(ptr)
      end if
!
      end do
!
!-----------------------------------------------------------------------
!     Debug: write field in netCDF format    
!-----------------------------------------------------------------------
!
      if (debugLevel == 3) then
        write(ofile,80) 'atm_import', trim(itemNameList(i)),            &
                        iyear, imonth, iday, ihour, iminute, isec
        call ESMF_FieldWrite(field, trim(ofile)//'.nc',&
                             variableName='data', overwrite=.true.,     &
                             rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
                               line=__LINE__, file=FILENAME)) return
      end if
!
      end do
!
!-----------------------------------------------------------------------
!     Deallocate arrays    
!-----------------------------------------------------------------------
!
      if (allocated(itemNameList)) deallocate(itemNameList)
      if (allocated(itemTypeList)) deallocate(itemTypeList)
!
!-----------------------------------------------------------------------
!     Format definition 
!-----------------------------------------------------------------------
!
 60   format(' PET(',I3,') - DE(',I2,') - ', A20, ' : ', 4I8)
 70   format(A10,'_',A,'_',I4,'-',I2.2,'-',I2.2,'_',I2.2,'_',I2.2,'_',I1)
 80   format(A10,'_',A,'_',                                             &
             I4,'-',I2.2,'-',I2.2,'_',I2.2,'_',I2.2,'_',I2.2)
!
      end subroutine ATM_Get
!
      subroutine ATM_Put(gcomp, rc)
!
!-----------------------------------------------------------------------
!     Used module declarations 
!-----------------------------------------------------------------------
!
      use module_domain, only : head_grid, get_ijk_from_grid
      use module_model_constants, only : stbolt
      use module_state_description, only : p_qv, p_qc, p_qr, p_qi, p_qs
!
      implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
      type(ESMF_GridComp) :: gcomp
      integer, intent(out) :: rc
!
!-----------------------------------------------------------------------
!     Local variable declarations 
!-----------------------------------------------------------------------
!
      integer :: ids, ide, jds, jde, kds, kde,                          &
                 ims, ime, jms, jme, kms, kme,                          &
                 ips, ipe, jps, jpe, kps, kpe
      integer :: i, j, k, m, n, p, nz
      integer :: iyear, iday, imonth, ihour, iminute, isec, iunit
      integer :: petCount, localPet, itemCount, localDECount
      character(ESMF_MAXSTR) :: cname, ofile
      character(ESMF_MAXSTR), allocatable :: itemNameList(:)
      real(ESMF_KIND_R8) :: u_uo, v_vo, wspd, taut
      real(ESMF_KIND_R8) :: cff, cff1, cff2, cff3 
      real(ESMF_KIND_R8), parameter :: eps=1.0e-10
      real(ESMF_KIND_R8), pointer :: ptr2d(:,:)
      real(ESMF_KIND_R8), pointer :: ptr3d(:,:,:)
      real(ESMF_KIND_R8), allocatable :: varout(:,:,:)
      integer(ESMF_KIND_I8) :: tstep
      real(ESMF_KIND_R8), save, allocatable :: total_precip_stored(:,:)
      real(ESMF_KIND_R8), allocatable       :: total_precip_tmp(:,:)
      logical, save :: firsttime = .true.
!
      type(ESMF_VM) :: vm
      type(ESMF_Clock) :: clock
      type(ESMF_Time) :: currTime
      type(ESMF_Field) :: field
      type(ESMF_State) :: exportState
      type(ESMF_StateItem_Flag), allocatable :: itemTypeList(:)
!
      rc = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!     Get gridded component 
!-----------------------------------------------------------------------
!
      call ESMF_GridCompGet(gcomp, name=cname, clock=clock,             &
                            exportState=exportState, vm=vm, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Get current time 
!-----------------------------------------------------------------------
!
      if (debugLevel > 2) then
      call ESMF_ClockGet(clock, currTime=currTime,                      &
                         advanceCount=tstep, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      call ESMF_TimeGet(currTime, yy=iyear, mm=imonth,                  &
                        dd=iday, h=ihour, m=iminute, s=isec, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
      end if
!
!-----------------------------------------------------------------------
!     Get list of export fields 
!-----------------------------------------------------------------------
!
      call ESMF_StateGet(exportState, itemCount=itemCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return 
!
      if (.not. allocated(itemNameList)) then
        allocate(itemNameList(itemCount))
      end if
      if (.not. allocated(itemTypeList)) then
        allocate(itemTypeList(itemCount))
      end if
      call ESMF_StateGet(exportState, itemNameList=itemNameList,        &
                         itemTypeList=itemTypeList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Get variables related with grid partitioning
!     ids, ide, jds, jde, kds, kde => domain extent
!     ims, ime, jms, jme, kms, kme => memory extent
!     ips, ipe, jps, jpe, kps, kpe => patch extent
!-----------------------------------------------------------------------
!
      call get_ijk_from_grid(head_grid, ids, ide, jds, jde, kds, kde,   &
                                        ims, ime, jms, jme, kms, kme,   &
                                        ips, ipe, jps, jpe, kps, kpe)
!
!-----------------------------------------------------------------------
!     Rotate wind on a rectangular north-south east-west oriented grid 
!-----------------------------------------------------------------------
!
!     ??????????????????????
!
!-----------------------------------------------------------------------
!     Loop over export fields 
!-----------------------------------------------------------------------
!
      do i = 1, itemCount
!
      k = get_varid(models(Iatmos)%exportField, trim(itemNameList(i)))
!
!-----------------------------------------------------------------------
!     Check rank of the export field
!-----------------------------------------------------------------------
!
      if (models(Iatmos)%exportField(k)%rank .eq. 2) then
!
!-----------------------------------------------------------------------
!     Get number of local DEs
!-----------------------------------------------------------------------
! 
      call ESMF_GridGet(models(Iatmos)%grid,                            &
                        localDECount=localDECount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Get field from export state 
!-----------------------------------------------------------------------
!
      call ESMF_StateGet(exportState, trim(itemNameList(i)),            &
                         field, rc=rc) 
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Loop over decomposition elements (DEs) 
!-----------------------------------------------------------------------
!
      do j = 0, localDECount-1
!
!-----------------------------------------------------------------------
!     Get pointer from field 
!-----------------------------------------------------------------------
!
      call ESMF_FieldGet(field, localDE=j, farrayPtr=ptr2d, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Set initial value to missing 
!-----------------------------------------------------------------------
!
      ptr2d = MISSING_R8
!
!-----------------------------------------------------------------------
!     Put data to export field 
!-----------------------------------------------------------------------
!
      select case (trim(adjustl(itemNameList(i)))) 
      case ('psfc')
        ! following code inherited from COAWST modelling system
        ! for more information please refer to Warner et al., 2010 
        do m = ips, ipe 
          do n = jps, jpe
            ptr2d(m,n) = head_grid%PSFC(m,n)*                           &
                         exp((9.81*head_grid%ht(m,n))/                  &
                         (287.0*head_grid%T2(m,n)*                      &
                         (1.0+0.61*head_grid%Q2(m,n))))
          end do
        end do
      case ('tsfc')
        do m = ips, ipe 
          do n = jps, jpe
            ptr2d(m,n) = head_grid%T2(m,n)
          end do
        end do
      case ('qsfc')
        do m = ips, ipe
          do n = jps, jpe
            ptr2d(m,n) = head_grid%Q2(m,n)
          end do
        end do
      case ('lwrd')
        do m = ips, ipe
          do n = jps, jpe
            ptr2d(m,n) = head_grid%GLW(m,n)-                            &
                     (STBOLT*head_grid%EMISS(m,n)*head_grid%SST(m,n)**4)
          end do
        end do
      case ('dlwr')
        do m = ips, ipe
          do n = jps, jpe
            ptr2d(m,n) = head_grid%GLW(m,n)
          end do
        end do
      case ('lhfx')
        do m = ips, ipe
          do n = jps, jpe
            ptr2d(m,n) = head_grid%LH(m,n)
          end do
        end do
      case ('shfx')
        do m = ips, ipe
          do n = jps, jpe
            ptr2d(m,n) = head_grid%HFX(m,n)
          end do
        end do
      case ('evap')
        do m = ips, ipe 
          do n = jps, jpe
             ptr2d(m,n) = head_grid%QFX(m,n)
          end do
        end do
      case ('prec')
       do m = ips, ipe
         do n = jps, jpe
           ptr2d(m,n) = head_grid%RAINC(m,n)+head_grid%RAINNC(m,n)+head_grid%RAINSH(m,n)
         end do
       end do
       ! store initial data of accumulated prec.
       if (firsttime .and. .not. restarted) then
          if (.not. allocated(total_precip_stored)) allocate(total_precip_stored(ips:ipe,jps:jpe))
          total_precip_stored(ips:ipe,jps:jpe) = ZERO_R8
          ptr2d(ips:ipe,jps:jpe) = (ptr2d(ips:ipe,jps:jpe)-total_precip_stored(ips:ipe,jps:jpe)) / ((24./connectors(Iatmos,Iocean)%divDT) * 60 * 60) ! Convert mm to mm/s
          write(*,fmt="(A,I5.5,F15.8)") "Initialize rain,firsttime and not restart->",localPet,maxval(total_precip_stored(ips:ipe,jps:jpe))*3600.
          firsttime = .false.
       else if (firsttime .and. restarted) then
          if (.not. allocated(total_precip_stored)) allocate(total_precip_stored(ips:ipe,jps:jpe))
          total_precip_stored(ips:ipe,jps:jpe) = ptr2d(ips:ipe,jps:jpe)
          ! First guess...there is no rain at first time step after restart
          ptr2d(ips:ipe,jps:jpe) = (ptr2d(ips:ipe,jps:jpe)-total_precip_stored(ips:ipe,jps:jpe)) / ((24./connectors(Iatmos,Iocean)%divDT) * 60 * 60) ! Convert mm to mm/s
          write(*,fmt="(A,I5.5,F15.8)") "Initialize rain,firsttime and restart->",localPet,maxval(total_precip_stored(ips:ipe,jps:jpe))
          firsttime = .false.
       else
          if (.not. allocated(total_precip_tmp)) allocate(total_precip_tmp(ips:ipe,jps:jpe))
          total_precip_tmp(ips:ipe,jps:jpe) = ptr2d(ips:ipe,jps:jpe)
          ptr2d(ips:ipe,jps:jpe) = (ptr2d(ips:ipe,jps:jpe)-total_precip_stored(ips:ipe,jps:jpe)) / ((24./connectors(Iatmos,Iocean)%divDT) * 60 * 60) ! Convert mm to mm/s
          total_precip_stored(ips:ipe,jps:jpe) = total_precip_tmp(ips:ipe,jps:jpe)
          write(*,fmt="(A,I5.5,3F15.8)") "Precipitation Decumulated (mm/h) =  ", &
             localPet,minval(ptr2d(ips:ipe,jps:jpe))*3600.,maxval(ptr2d(ips:ipe,jps:jpe))*3600.,maxval(total_precip_stored(ips:ipe,jps:jpe))
       end if
      case ('wndu')
        do m = ips, ipe 
          do n = jps, jpe
             ptr2d(m,n) = head_grid%U10(m,n)*head_grid%cosa(m,n)-        &
                          head_grid%V10(m,n)*head_grid%sina(m,n)
          end do
        end do
      case ('wndv') 
        do m = ips, ipe 
          do n = jps, jpe
             ptr2d(m,n) = head_grid%V10(m,n)*head_grid%cosa(m,n)+        &
                          head_grid%U10(m,n)*head_grid%sina(m,n)
          end do
        end do
      case ('swrd') 
        do m = ips, ipe
          do n = jps, jpe
            ptr2d(m,n) = head_grid%GSW(m,n)
          end do
        end do
      case ('dswr') ! W/m2
        do m = ips, ipe
          do n = jps, jpe
            ptr2d(m,n) = head_grid%SWDOWN(m,n)
          end do
        end do
      case ('rnof') ! mm/s
        do m = ips, ipe
          do n = jps, jpe
            ptr2d(m,n) = head_grid%runsfxy(m,n)
          end do
        end do
      case ('snof') ! mm/s
        do m = ips, ipe
          do n = jps, jpe
            ptr2d(m,n) = head_grid%runsbxy(m,n)
          end do
        end do
      case ('taux')
        ! following code inherited from COAWST modelling system
        ! for more information please refer to Warner et al., 2010   
        do m = ips, ipe
          do n = jps, jpe
            cff1 = 1.0/(head_grid%alt(m,1,n)+eps)
            cff2 = 2.0/(((head_grid%u_2(m,1,n)+head_grid%u_2(m+1,1,n))**2+&
                   (head_grid%v_2(m,1,n)+head_grid%v_2(m,1,n+1))**2)**0.5+eps)
            cff3 = 0.5*(head_grid%u_2(m,1,n)+head_grid%u_2(m+1,1,n))*head_grid%cosa(m,n)-&
                   0.5*(head_grid%v_2(m,1,n)+head_grid%v_2(m,1,n+1))*head_grid%sina(m,n)
            cff = cff1*cff2*(head_grid%UST(m,n)**2)*cff3
            ptr2d(m,n) = cff
          end do
        end do
      case ('tauy')
        ! following code inherited from COAWST modelling system
        ! for more information please refer to Warner et al., 2010   
        do m = ips, ipe
          do n = jps, jpe
            cff1 = 1.0/(head_grid%alt(m,1,n)+eps)
            cff2 = 2.0/(((head_grid%u_2(m,1,n)+head_grid%u_2(m+1,1,n))**2+&
                   (head_grid%v_2(m,1,n)+head_grid%v_2(m,1,n+1))**2)**0.5+eps)
            cff3 = 0.5*(head_grid%v_2(m,1,n)+head_grid%v_2(m,1,n+1))*head_grid%cosa(m,n)+&
                   0.5*(head_grid%u_2(m,1,n)+head_grid%u_2(m+1,1,n))*head_grid%sina(m,n)
            cff = cff1*cff2*(head_grid%UST(m,n)**2)*cff3
            ptr2d(m,n) = cff
          end do
        end do
      case ('wspd') ! m/s
        do m = ips, ipe
          do n = jps, jpe
            cff1 = 0.5*(head_grid%U10(m,n)+head_grid%U10(m+1,n))*head_grid%cosa(m,n)-&
                   0.5*(head_grid%V10(m,n)+head_grid%V10(m,n+1))*head_grid%sina(m,n)
            cff2 = 0.5*(head_grid%V10(m,n)+head_grid%V10(m,n+1))*head_grid%cosa(m,n)+&
                   0.5*(head_grid%U10(m,n)+head_grid%U10(m+1,n))*head_grid%sina(m,n) 
            ptr2d(m,n) = (cff1*cff1+cff2*cff2)**0.5
          end do
        end do
      case ('wdir')
        do m = ips, ipe
          do n = jps, jpe
            cff1 = 0.5*(head_grid%U10(m,n)+head_grid%U10(m+1,n))*head_grid%cosa(m,n)-&
                   0.5*(head_grid%V10(m,n)+head_grid%V10(m,n+1))*head_grid%sina(m,n)
            cff2 = 0.5*(head_grid%V10(m,n)+head_grid%V10(m,n+1))*head_grid%cosa(m,n)+&
                   0.5*(head_grid%U10(m,n)+head_grid%U10(m+1,n))*head_grid%sina(m,n) 
            cff = atan2(cff1, cff2)
            if (cff < ZERO_R8) cff = cff+pi2
            ptr2d(m,n) = cff
          end do
        end do
      case ('ustr')
        do m = ips, ipe
          do n = jps, jpe
            ptr2d(m,n) = head_grid%UST(m,n)
          end do
        end do
      case ('nflx') 
        do m = ips, ipe
          do n = jps, jpe
            ptr2d(m,n) = head_grid%GSW(m,n)+                            &
                         (head_grid%EMISS(m,n)*head_grid%GLW(m,n)-(STBOLT*head_grid%EMISS(m,n)*head_grid%SST(m,n)**4))-&
                         head_grid%LH(m,n)-                             &
                         head_grid%HFX(m,n) 
          end do
        end do
      case ('nflz')
        do m = ips, ipe
          do n = jps, jpe
            ptr2d(m,n) = head_grid%HFX(m,n)+head_grid%LH(m,n)+                            &
                         (head_grid%GLW(m,n)-(STBOLT*head_grid%EMISS(m,n)*head_grid%SST(m,n)**4))
          end do
        end do
      case ('sflx') ! This works fine when no adaptive time step is used in WRF, otherwise is better using E-P
        do m = ips, ipe 
          do n = jps, jpe
            ! http://forum.wrfforum.com/viewtopic.php?f=32&t=5580
            ptr2d(m,n) = head_grid%QFX(m,n)-                            &
                        (head_grid%RAINCV(m,n)+head_grid%RAINNCV(m,n))/ &
                         head_grid%DT
          end do
        end do
      case ('snow') ! mm
        do m = ips, ipe 
          do n = jps, jpe
            ptr2d(m,n) = head_grid%SNOWNCV(m,n)
          end do
        end do
      case ('grau') ! mm
        do m = ips, ipe 
          do n = jps, jpe
            ptr2d(m,n) = head_grid%GRAUPELNCV(m,n)
          end do
        end do
      case ('hail') ! mm
        do m = ips, ipe 
          do n = jps, jpe
            ptr2d(m,n) = head_grid%HAILNCV(m,n)
          end do
        end do
      case ('topo') ! m
        do m = ips, ipe 
          do n = jps, jpe
            ptr2d(m,n) = head_grid%ht(m,n)
          end do
        end do
      case ('mask') ! unitless (0 land, 1 water)
        do m = ips, ipe 
          do n = jps, jpe
            ptr2d(m,n) = head_grid%LU_MASK(m,n)
          end do
        end do
      end select
!
!-----------------------------------------------------------------------
!     Nullify pointer to make sure that it does not point on a random 
!     part in the memory 
!-----------------------------------------------------------------------
!
      if (associated(ptr2d)) then
        nullify(ptr2d)
      end if
!
      end do
!
!-----------------------------------------------------------------------
!     Check rank of the export field
!-----------------------------------------------------------------------
!
      else if (models(Iatmos)%exportField(k)%rank .eq. 3) then
!
!-----------------------------------------------------------------------
!     Get number of local DEs
!-----------------------------------------------------------------------
! 
      call ESMF_GridGet(models(Iatmos)%grid3d,                          &
                        localDECount=localDECount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Get field from export state 
!-----------------------------------------------------------------------
!
      call ESMF_StateGet(exportState, trim(itemNameList(i)),            &
                         field, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Loop over decomposition elements (DEs) 
!-----------------------------------------------------------------------
!
      do j = 0, localDECount-1
!
!-----------------------------------------------------------------------
!     Get pointer from field 
!-----------------------------------------------------------------------
!
      call ESMF_FieldGet(field, localDE=j, farrayPtr=ptr3d, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Set initial value to missing 
!-----------------------------------------------------------------------
!
      ptr3d = MISSING_R8
!
!-----------------------------------------------------------------------
!     Put data to export field 
!-----------------------------------------------------------------------
!
      nz = models(Iatmos)%nLevs
!
      select case (trim(adjustl(itemNameList(i))))
      case ('qv')
        call wrf_vintrp(ips, ipe, jps, jpe, kps, kpe,                   &
                        head_grid%bdy_mask,                             &
                        head_grid%z_at_w(ips:ipe,kps:kpe,jps:jpe),      &
                        head_grid%moist(ips:ipe,kps:kpe,jps:jpe,p_qv),  &
                        varout)
      case ('qc')
        call wrf_vintrp(ips, ipe, jps, jpe, kps, kpe,                   &
                        head_grid%bdy_mask,                             &
                        head_grid%z_at_w(ips:ipe,kps:kpe,jps:jpe),      &
                        head_grid%moist(ips:ipe,kps:kpe,jps:jpe,p_qc),  &
                        varout)
      case ('qr')
        call wrf_vintrp(ips, ipe, jps, jpe, kps, kpe,                   &
                        head_grid%bdy_mask,                             &
                        head_grid%z_at_w(ips:ipe,kps:kpe,jps:jpe),      &
                        head_grid%moist(ips:ipe,kps:kpe,jps:jpe,p_qr),  &
                        varout)
      case ('qi')
        call wrf_vintrp(ips, ipe, jps, jpe, kps, kpe,                   &
                        head_grid%bdy_mask,                             &
                        head_grid%z_at_w(ips:ipe,kps:kpe,jps:jpe),      &
                        head_grid%moist(ips:ipe,kps:kpe,jps:jpe,p_qi),  &
                        varout)
      case ('qs')
        call wrf_vintrp(ips, ipe, jps, jpe, kps, kpe,                   &
                        head_grid%bdy_mask,                             &
                        head_grid%z_at_w(ips:ipe,kps:kpe,jps:jpe),      &
                        head_grid%moist(ips:ipe,kps:kpe,jps:jpe,p_qs),  &
                        varout)
         
      end select
!
      if (allocated(varout)) then
        do p = 1, nz
          do m = ips, ipe
            do n = jps, jpe
              ptr3d(m,n,p) = varout(m,n,p)
            end do
          end do
        end do
        deallocate(varout)
      end if
!
!-----------------------------------------------------------------------
!     Nullify pointer to make sure that it does not point on a random 
!     part in the memory 
!-----------------------------------------------------------------------
!
      if (associated(ptr3d)) then
        nullify(ptr3d)
      end if
!
      end do
!
      end if
!
!-----------------------------------------------------------------------
!     Debug: write field in netCDF format    
!-----------------------------------------------------------------------
!
      if (debugLevel == 3) then
        write(ofile,100) 'atm_export', trim(itemNameList(i)),           &
                         iyear, imonth, iday, ihour, iminute, isec
        call ESMF_FieldWrite(field, trim(ofile)//'.nc',&
                             variableName='data', overwrite=.true.,     &
                             rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
                               line=__LINE__, file=FILENAME)) return
      end if
!
      end do
!
!-----------------------------------------------------------------------
!     Deallocate arrays    
!-----------------------------------------------------------------------
!
      if (allocated(itemNameList)) deallocate(itemNameList)
      if (allocated(itemTypeList)) deallocate(itemTypeList)
!
!-----------------------------------------------------------------------
!     Format definition 
!-----------------------------------------------------------------------
!
 90   format(A10,'_',A,'_',                                             &
             I4,'-',I2.2,'-',I2.2,'_',I2.2,'_',I2.2,'_',I1)
 100  format(A10,'_',A,'_',                                             &
             I4,'-',I2.2,'-',I2.2,'_',I2.2,'_',I2.2,'_',I2.2)
!
      end subroutine ATM_Put
!
      subroutine wrf_vintrp(ips, ipe, jps, jpe, kps, kpe, bdy_mask, hgt, varin, varout)
      implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
      integer, intent(in) :: ips, ipe, jps, jpe, kps, kpe
      logical, intent(in) :: bdy_mask(4)
      real, intent(in) :: hgt(ips:ipe,kps:kpe,jps:jpe)
      real, intent(in) :: varin(ips:ipe,kps:kpe,jps:jpe)      
      real*8, allocatable, intent(inout) :: varout(:,:,:)      
!
!-----------------------------------------------------------------------
!     Local variable declarations 
!-----------------------------------------------------------------------
!
      integer :: i, j, k, nz, ipos
!
!-----------------------------------------------------------------------
!     Get vertical coordinate array 
!-----------------------------------------------------------------------
!
      nz = models(Iatmos)%nLevs
      if (.not. allocated(varout)) allocate(varout(ips:ipe,jps:jpe,1:nz))
!
      do i = ips, ipe
        do j = jps, jpe
          do k = 1, nz
            ipos = locate(hgt(i,:,j), real(models(Iatmos)%levs(k)))
            if (ipos == -1) then
              varout(i,j,k) = MISSING_R8
            else if(ipos < kpe .and. ipos > 0) then
              varout(i,j,k) = varin(i,ipos,j)+                          &
                              (models(Iatmos)%levs(k)-hgt(i,ipos,j))*   &
                              (varin(i,ipos+1,j)-varin(i,ipos,j))/      &
                              (hgt(i,ipos+1,j)-hgt(i,ipos,j))
            else if(ipos == kpe) then
              varout(i,j,k) = varin(i,kpe,j)
            else if(ipos == 0) then
              varout(i,j,k) = varin(i,1,j)
            else
              write(6,fmt='("ERROR: Unexpected value of ipos : ",I0)') ipos
            end if
          end do
        end do
      end do
!
!-----------------------------------------------------------------------
!     Fill north and east edges
!-----------------------------------------------------------------------
!
      if (bdy_mask(2)) then
        do k = 1, nz
          varout(ipe,:,k) = varout(ipe-1,:,k)
        end do
      end if
!
      if (bdy_mask(4)) then
        do k = 1, nz
          varout(:,jpe,k) = varout(:,jpe-1,k)
        end do
      end if
!
      end subroutine wrf_vintrp
!
      integer function locate(xx,x)
!
!-----------------------------------------------------------------------
!     Locate a value in a sorted array
!     https://github.com/astrofrog/fortranlib
!-----------------------------------------------------------------------
!
      implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
      real, dimension(:), intent(in) :: xx
      real, intent(in) :: x
!
!-----------------------------------------------------------------------
!     Local variable declarations 
!-----------------------------------------------------------------------
!
      integer :: n,jl,jm,ju
      logical :: ascnd
!
      n=size(xx)
      ascnd = (xx(n) >= xx(1))
      jl=0
      ju=n+1
      do
        if (ju-jl <= 1) exit
        jm=(ju+jl)/2
        if (ascnd .eqv. (x >= xx(jm))) then
          jl=jm
        else
          ju=jm
        end if
      end do
!
      if (x == xx(1)) then
        locate = 1
      else if (x == xx(n)) then
        locate = n-1
      else if(ascnd.and. (x > xx(n) .or. x < xx(1))) then
        locate = -1
      else if(.not.ascnd.and. (x < xx(n) .or. x > xx(1))) then
        locate = -1
      else
        locate = jl
      end if
!
      end function locate
!
      end module mod_esmf_atm
