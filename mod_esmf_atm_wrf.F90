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
          NUOPC_Label_SetClock       => label_SetClock
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
!      call ATM_SetStates(gcomp, rc)
!      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
!                             line=__LINE__, file=FILENAME)) return
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
      integer :: its, ite, jts, ite
      integer :: localPet, petCount 
      integer :: numprocsX, numprocsY
      integer :: lbnd(3), ubnd(3)
      integer :: minIndex(2), maxIndex(2)
      integer, allocatable :: ipatchStarts(:), jpatchStarts(:)
      integer, allocatable :: ipatchEnds(:), jpatchEnds(:)
      integer, allocatable :: allXStart(:), allYStart(:)
      integer, allocatable :: allXCount(:), allYCount(:)
      integer, allocatable :: dimXCount(:), dimYCount(:)
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
      call ESMF_VMAllGatherV(vm, sendData=(/ ipe /), sendCount=1,       &
                             recvData=ipatchEnds,                       &
                             recvCounts=(/ (1, k = 0, petCount-1) /),   &
                             recvOffsets=(/ (k, k = 0, petCount-1) /),  &
                             rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      call ESMF_VMAllGatherV(vm, sendData=(/ jpe /), sendCount=1,       &
                             recvData=jpatchEnds,                       &
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
!        write(*,fmt="(6I5)") localPet, pe, deBlockList(1,1,pe), deBlockList(1,2,pe), deBlockList(2,1,pe), deBlockList(2,2,pe)
      end do
!
      minIndex = (/ 1, 1 /)
      maxIndex = (/ maxval(ipatchEnds), maxval(jpatchEnds) /)
!      write(*,fmt="(A,3I5)") "maxIndex = ", localPet, maxIndex
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
                    lbound(ptrX, dim=1), ubound(ptrX, dim=1),           &
                    lbound(ptrX, dim=2), ubound(ptrX, dim=2)
      end if
!
!-----------------------------------------------------------------------
!     Fill the pointers    
!-----------------------------------------------------------------------
!
!        if (debugLevel > 0) then
!          write(*,30) localPet, 1, adjustl("DAT/ATM/GRD/"//name),       &
!                      lbound(head_grid%xlong, dim=1), ubound(head_grid%xlong, dim=1), &
!                      lbound(head_grid%xlong, dim=2), ubound(head_grid%xlong, dim=2)
!                      lbnd(1), ubnd(1), lbnd(2), ubnd(2)   
!      end if
      do i = lbnd(1), ubnd(1)
        do j = lbnd(2), ubnd(2)
          ptrX(i,j) = head_grid%xlong(i,j)
          ptrY(i,j) = head_grid%xlat(i,j) 
          ptrM(i,j) = head_grid%landmask(i,j) 
!          if (localPet == 29) then
!            write(*,fmt="(3I5,2F8.4,I5)"), localPet, i, j, ptrX(i,j), ptrY(i,j), ptrM(i,j)
!          end if
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
!      if (debugLevel > 1) then
      call ESMF_GridWriteVTK(models(Iatmos)%grid,&
                         filename="atmos_"//&
                         trim(GRIDDES(models(Iatmos)%mesh(1)%gtype))//&
                         "point",&
                         staggerLoc=ESMF_STAGGERLOC_CENTER,&
                         rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,&
                             line=__LINE__, file=FILENAME)) return
!      end if
!
!
!-----------------------------------------------------------------------
!     Create DELayout
!-----------------------------------------------------------------------
!
!      pe = 0
!      do j = 0, numprocsY-1
!        do i = 0, numprocsX-1
!          permuteTasks(pe) = pe
!          pe = pe+1
!        end do
!      end do
!
!      call ESMF_DELayoutCreate( vm, numprocsXY, petList=permuteTasks, rc=rc )
!
!
!      print*, localPet, numprocsX, numprocsY
!
!      is_min = minval(allXStart)
!      js_min = minval(allYStart)
!
!      do pe = 0, numprocs-1
!        if (allXStart(pe) == is_min) then
!          dimYCount(j) = allYCount(pe)
!          j = j+1
!        end if 
!        if (allYStart(pe) == js_min) then
!          dimXCount(i) = allXCount(pe)
!          i = i+1
!        end if
!      end do
!
!      print*, PatchStart(1), PatchStart(2)
!      if (.not. allocated(dimXCount)) allocate(dimXCount(
!
!-----------------------------------------------------------------------
!     Create ESMF Grid
!-----------------------------------------------------------------------
!
!      models(Iatmos)%grid = ESMF_GridCreate(  &
!                 countsPerDEDim1=dimXCount , &
!                 countsPerDEDim2=dimYcount , &
!                 coordDep1=(/1/) , &
!                 coordDep2=(/2/) , &
!                 indexflag=ESMF_INDEX_GLOBAL, & ! use global indices
!                 name=TRIM(gridname), &
!                 rc = rc )
!
!-----------------------------------------------------------------------
!     Format definition 
!-----------------------------------------------------------------------
!
 30   format(" PET(",I3.3,") - DE(",I2.2,") - ", A20, " : ", 4I8)
!
      end subroutine ATM_SetGridArrays
!
      end module mod_esmf_atm

