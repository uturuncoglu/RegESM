!-----------------------------------------------------------------------
!
!     This file is part of ITU RegESM.
!
!     ITU RegESM is free software: you can redistribute it and/or modify
!     it under the terms of the GNU General Public License as published by
!     the Free Software Foundation, either version 3 of the License, or
!     (at your option) any later version.
!
!     ITU RegESM is distributed in the hope that it will be useful,
!     but WITHOUT ANY WARRANTY; without even the implied warranty of
!     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!     GNU General Public License for more details.
!
!     You should have received a copy of the GNU General Public License
!     along with ITU RegESM.  If not, see <http://www.gnu.org/licenses/>.
!
!-----------------------------------------------------------------------
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
      integer, intent(inout) :: rc
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
      call ATM_SetGridArrays(gcomp, rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Set-up fields and register to import/export states
!-----------------------------------------------------------------------
!
      call ATM_SetStates(gcomp, rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      end subroutine ATM_SetInitializeP2
!
      subroutine ATM_SetGridArrays(gcomp, rc)
!
!-----------------------------------------------------------------------
!     Used module declarations 
!-----------------------------------------------------------------------
!
      use module_domain, only : head_grid, get_ijk_from_grid
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
      numprocsX = 0
      numprocsY = 0
      do pe = 0, petCount-1
        if (ips == ipatchStarts(pe)) then
          numprocsY = numprocsY+1
        end if
        if (jps == jpatchStarts(pe)) then
          numprocsX = numprocsX+1
        end if        
      end do
!
      if (.not. allocated(ipatchEnds)) then
        allocate(ipatchEnds(0:petCount-1))
      end if
!
      if (.not. allocated(jpatchEnds)) then
        allocate(jpatchEnds(0:petCount-1))
      end if
!
      call ESMF_VMAllGatherV(vm, sendData=(/ min(ide-1,ipe) /),         &
                             sendCount=1, recvData=ipatchEnds,          &
                             recvCounts=(/ (1, k = 0, petCount-1) /),   &
                             recvOffsets=(/ (k, k = 0, petCount-1) /),  &
                             rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      call ESMF_VMAllGatherV(vm, sendData=(/ min(jde-1,jpe) /),         &
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
      minIndex = (/ 1, 1 /)
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
                                            name="atm_grid",            &
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
                      ubound(head_grid%xlong, dim=2)
      end if
!
      do i = lbnd(1), ubnd(1)
        do j = lbnd(2), ubnd(2)
          ptrX(i,j) = head_grid%xlong(i,j)
          ptrY(i,j) = head_grid%xlat(i,j) 
          ptrM(i,j) = head_grid%landmask(i,j) 
        end do
      end do
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
 30   format(" PET(",I3.3,") - DE(",I2.2,") - ", A20, " : ", 4I8)
!
      end subroutine ATM_SetGridArrays
!
      subroutine ATM_SetStates(gcomp, rc)
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
      end subroutine ATM_SetStates
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
!     Put export fields (only for initial and restart run)
!-----------------------------------------------------------------------
!
      if (restarted .and. currTime == esmRestartTime) then
!
!-----------------------------------------------------------------------
!     Put export fields
!-----------------------------------------------------------------------
!
!        call ATM_Put(gcomp, rc=rc)
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
      if (restarted) then
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
        if (debugLevel == 0) then
          write(*,40) trim(str1), trim(str2), phase
        end if
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
!      if ((currTime /= refTime) .or. restarted) then
!        call ATM_Get(gcomp, rc=rc)
!      end if
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
!      call ATM_Put(gcomp, rc=rc)
!
!-----------------------------------------------------------------------
!     Formats 
!-----------------------------------------------------------------------
!
 40   format(' Running ATM Component: ',A,' --> ',A,' Phase: ',I1)
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
      end module mod_esmf_atm

