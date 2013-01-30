!-----------------------------------------------------------------------
!
!     This file is part of ICTP RegESM.
!
!     ICTP RegESM is free software: you can redistribute it and/or modify
!     it under the terms of the GNU General Public License as published by
!     the Free Software Foundation, either version 3 of the License, or
!     (at your option) any later version.
!
!     ICTP RegESM is distributed in the hope that it will be useful,
!     but WITHOUT ANY WARRANTY; without even the implied warranty of
!     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!     GNU General Public License for more details.
!
!     You should have received a copy of the GNU General Public License
!     along with ICTP RegESM.  If not, see <http://www.gnu.org/licenses/>.
!
!-----------------------------------------------------------------------
#define FILENAME "mod_esmf_ocn.F90"
!
!-----------------------------------------------------------------------
!     OCN gridded component code 
!-----------------------------------------------------------------------
!
      module mod_esmf_ocn
!
!-----------------------------------------------------------------------
!     Used module declarations 
!-----------------------------------------------------------------------
!
      use ESMF
      use NUOPC
      use NUOPC_Model, only :                                           &
          NUOPC_SetServices   => routine_SetServices,                   &
          NUOPC_Label_Advance => label_Advance
!
      use mod_types
!
      use ocean_control_mod, only :                                     &
          OCN_Initialize => ROMS_initialize,                            &
          OCN_Run        => ROMS_run,                                   &
          OCN_Finalize   => ROMS_finalize
      use mod_forces, only : FORCES
!
      implicit none
      private
!
!-----------------------------------------------------------------------
!     Public subroutines 
!-----------------------------------------------------------------------
!
      public :: OCN_SetServices
!
      contains
!
      subroutine OCN_SetServices(gcomp, rc)
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
      call NUOPC_SetServices(gcomp, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Register initialize routine (P 1/2) for specific implementation   
!-----------------------------------------------------------------------
!
      call ESMF_GridCompSetEntryPoint(gcomp,                            &
                                      methodflag=ESMF_METHOD_INITIALIZE,&
                                      userRoutine=OCN_SetInitializeP1,  &
                                      phase=1,                          &
                                      rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      call ESMF_GridCompSetEntryPoint(gcomp,                            &
                                      methodflag=ESMF_METHOD_INITIALIZE,&
                                      userRoutine=OCN_SetInitializeP2,  &
                                      phase=2,                          &
                                      rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Attach specializing method(s)   
!-----------------------------------------------------------------------
!
      call ESMF_MethodAdd(gcomp, label=NUOPC_Label_Advance,             &
                          userRoutine=OCN_ModelAdvance, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      end subroutine OCN_SetServices
!
      subroutine OCN_SetInitializeP1(gcomp, importState, exportState,   &
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
      do i = 1, ubound(models(Iocean)%importField, dim=1)
        call NUOPC_StateAdvertiseField(importState,                     &
             StandardName=trim(models(Iocean)%importField(i)%long_name),&
             name=trim(models(Iocean)%importField(i)%short_name), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
                               line=__LINE__, file=FILENAME)) return
      end do 
!
!-----------------------------------------------------------------------
!     Set export fields 
!-----------------------------------------------------------------------
!
      do i = 1, ubound(models(Iocean)%exportField, dim=1)
        call NUOPC_StateAdvertiseField(exportState,                     &
             StandardName=trim(models(Iocean)%exportField(i)%long_name),&
             name=trim(models(Iocean)%exportField(i)%short_name), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
                               line=__LINE__, file=FILENAME)) return
      end do
!
      end subroutine OCN_SetInitializeP1
!
      subroutine OCN_SetInitializeP2(gcomp, importState, exportState,   &
                                     clock, rc)
!
!-----------------------------------------------------------------------
!     Used module declarations 
!-----------------------------------------------------------------------
!
      use mod_scalars, only : exit_flag, NoError
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
      logical :: flag
      integer :: comm, localPet, petCount
      character(ESMF_MAXSTR) :: gname, msgString
!
      type(ESMF_VM) :: vm
!
      rc = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!     Get gridded component 
!-----------------------------------------------------------------------
!
      call ESMF_GridCompGet(gcomp, name=gname, vm=vm, rc=rc)
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
      flag = .true.
      call OCN_Initialize(flag, mpiCOMM=comm)
!
!-----------------------------------------------------------------------
!     Check for error 
!-----------------------------------------------------------------------
!
!      if (exit_flag /= NoError) then
!        write(msgString,'(A,I5)') trim(gname)//                         &
!              ': exit with flag = ', exit_flag
!        call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_ERROR)
!        call OCN_Finalize()
!      end if 
!
!-----------------------------------------------------------------------
!     Set-up internal clock for gridded component
!-----------------------------------------------------------------------
!
      call OCN_SetClock(gcomp, rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Set-up grid and load coordinate data 
!-----------------------------------------------------------------------
!
      call OCN_SetGridArrays(gcomp, localPet, rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Set-up fields and register to import/export states
!-----------------------------------------------------------------------
!
      call OCN_SetStates(gcomp, importState, exportState, rc)
!
      end subroutine OCN_SetInitializeP2
!
      subroutine OCN_SetClock(gcomp, rc)
!
!-----------------------------------------------------------------------
!     Used module declarations 
!-----------------------------------------------------------------------
!
      use mod_param
      use mod_scalars
!
      implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
      type(ESMF_GridComp) :: gcomp
      integer :: rc
!
!-----------------------------------------------------------------------
!     Local variable declarations 
!-----------------------------------------------------------------------
!
      integer :: ng
      integer :: ref_year,   str_year,   end_year
      integer :: ref_month,  str_month,  end_month
      integer :: ref_day,    str_day,    end_day
      integer :: ref_hour,   str_hour,   end_hour
      integer :: ref_minute, str_minute, end_minute
      integer :: ref_second, str_second, end_second
      real*8 :: stime, etime, hour, minute, yday
      character (len=80) :: name, calendar
!
      type(ESMF_Clock) :: clock
      type(ESMF_TimeInterval) :: timeStep
      type(ESMF_Time) :: refTime, startTime, stopTime
      type(ESMF_Calendar) :: cal   
!
      rc = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!     Create gridded component clock 
!-----------------------------------------------------------------------
!
      if (int(time_ref) == -2) then
        ref_year=1968
        ref_month=5
        ref_day=23
        ref_hour=0
        ref_minute=0
        ref_second=0
        calendar='gregorian'
        cal = ESMF_CalendarCreate(ESMF_CALKIND_GREGORIAN,               &
                                  name=trim(calendar), rc=rc)
      else if (int(time_ref) == -1) then
        ref_year=0
        ref_month=1
        ref_day=1
        ref_hour=0
        ref_minute=0
        ref_second=0
        calendar='360_day'
        cal = ESMF_CalendarCreate(ESMF_CALKIND_360DAY,                  &
                                  name=trim(calendar), rc=rc)
      else if (int(time_ref) == 0) then
        ref_year=1
        ref_month=1
        ref_day=1
        ref_hour=0
        ref_minute=0
        ref_second=0
        calendar='julian'
        cal = ESMF_CalendarCreate(ESMF_CALKIND_JULIAN,                  &
                                  name=trim(calendar), rc=rc)
      else if (time_ref > 0.0_r8) then
        ref_year=int(r_date(2))
        ref_month=int(r_date(4))
        ref_day=int(r_date(5))
        ref_hour=int(r_date(6))
        ref_minute=int(r_date(7))
        ref_second=int(r_date(8))
        calendar='gregorian'
        cal = ESMF_CalendarCreate(ESMF_CALKIND_GREGORIAN,               &
                                  name=trim(calendar), rc=rc)
      end if
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Set reference time
!-----------------------------------------------------------------------
!
      call ESMF_TimeSet(refTime,                                        &
                        yy=ref_year,                                    &
                        mm=ref_month,                                   &
                        dd=ref_day,                                     &
                        h=ref_hour,                                     &
                        m=ref_minute,                                   &
                        s=ref_second,                                   &
                        calendar=cal,                                   &
                        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Set start time
!-----------------------------------------------------------------------
!
      stime = minval(tdays)
      call caldate(r_date, stime, str_year, yday, str_month,            &
                   str_day, hour)
      minute = (hour-aint(hour))*60.0_r8
      str_hour = int(hour)
      str_minute = int(minute)
      str_second = int((minute-aint(minute))*60.0_r8)
!
      call ESMF_TimeSet(startTime,                                      &
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
      etime = 0.0_r8
      do ng = 1, 1 !Ngrids
        etime = max(etime, (real(ntimes(ng),r8)*dt(ng))*sec2day)
      end do
      etime = etime+stime
      call caldate(r_date, etime, end_year, yday, end_month,            &
                   end_day, hour)
      minute = (hour-aint(hour))*60.0_r8
      end_hour = int(hour)
      end_minute = int(minute)
      end_second = int((minute-aint(minute))*60.0_r8)
!
      call ESMF_TimeSet(stopTime,                                       &
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
!     Set time interval
!-----------------------------------------------------------------------
!
      call ESMF_TimeIntervalSet(timeStep, s=int(minval(dt)), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Create component clock
!-----------------------------------------------------------------------
!
      clock = ESMF_ClockCreate(name='ocn_clock', refTime=refTime,       &
                               timeStep=timeStep, startTime=startTime,  &
                               stopTime=stopTime, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Add clock to component
!-----------------------------------------------------------------------
!
!      call NUOPC_GridCompSetClock(gcomp, clock, timeStep, rc=rc)
!      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
!                             line=__LINE__, file=FILENAME)) return
!
!
      end subroutine OCN_SetClock
!
      subroutine OCN_SetGridArrays(gcomp, localPet, rc)
!
!-----------------------------------------------------------------------
!     Used module declarations 
!-----------------------------------------------------------------------
!
      use mod_grid , only : GRID
      use mod_param, only : NtileI, NtileJ, BOUNDS, Lm, Mm, Ngrids
!
      implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
      type(ESMF_GridComp), intent(inout) :: gcomp
      integer :: localPet 
      integer :: rc
!
!-----------------------------------------------------------------------
!     Local variable declarations 
!-----------------------------------------------------------------------
!
      integer :: i, j, ii, jj, ng, tile, localDECount
      integer :: IstrR, IendR, JstrR, JendR
      integer :: IstrU, IendU, JstrU, JendU     
      integer :: IstrV, IendV, JstrV, JendV
      integer :: staggerEdgeLWidth(2)
      integer :: staggerEdgeUWidth(2)
      integer, allocatable :: deBlockList(:,:,:)
      real(ESMF_KIND_R8), pointer :: ptrX(:,:), ptrY(:,:)
      integer, pointer :: ptrM(:,:)
      character(ESMF_MAXSTR) :: cname, name, msgString
!
      type(ESMF_ArraySpec) :: arraySpec
      type(ESMF_StaggerLoc) :: staggerLoc
      type(ESMF_DistGrid) :: distGrid
!
      rc = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!     Check number of nested grids 
!-----------------------------------------------------------------------
!
      call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      if (Ngrids > 1) then
        write(msgString,'(A,I3)') trim(cname)//                         &
              ': number of nested grid is', Ngrids,                     &
              'coupled model only interactes with outermost one!'
        call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_ERROR)        
        ng = 1
      else
        ng = Ngrids
      end if
!
!-----------------------------------------------------------------------
!     Get limits of the grid arrays (based on PET and nest level)
!-----------------------------------------------------------------------
!
      IstrR = BOUNDS(ng)%IstrR(localPet)
      IendR = BOUNDS(ng)%IendR(localPet)
      JstrR = BOUNDS(ng)%JstrR(localPet)
      JendR = BOUNDS(ng)%JendR(localPet)
!
      IstrU = BOUNDS(ng)%Istr(localPet)
      IendU = BOUNDS(ng)%IendR(localPet)
      JstrU = BOUNDS(ng)%JstrR(localPet)
      JendU = BOUNDS(ng)%JendR(localPet)
!
      IstrV = BOUNDS(ng)%IstrR(localPet)
      IendV = BOUNDS(ng)%IendR(localPet)
      JstrV = BOUNDS(ng)%Jstr(localPet)
      JendV = BOUNDS(ng)%JendR(localPet)
!
      if (.not.allocated(deBlockList)) then
        allocate(deBlockList(2,2,NtileI(ng)*NtileJ(ng)))
      end if
      do tile=0,NtileI(ng)*NtileJ(ng)-1
        deBlockList(1,1,tile+1)=BOUNDS(ng)%Istr(tile)
        deBlockList(1,2,tile+1)=BOUNDS(ng)%Iend(tile)
        deBlockList(2,1,tile+1)=BOUNDS(ng)%Jstr(tile)
        deBlockList(2,2,tile+1)=BOUNDS(ng)%Jend(tile)
      end do
!
!-----------------------------------------------------------------------
!     Create ESMF DistGrid based on model domain decomposition
!-----------------------------------------------------------------------
!
      distGrid = ESMF_DistGridCreate(minIndex=(/ 1, 1 /),               &
                                     maxIndex=(/ Lm(ng), Mm(ng) /),     &
                                     deBlockList=deBlockList,           &
                                     rc=rc)
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
!     Define component grid (dot and cross points)
!-----------------------------------------------------------------------
!
      if (.not. allocated(models(Iocean)%mesh)) then
        allocate(models(Iocean)%mesh(4))
        models(Iocean)%mesh(1)%gtype = Icross
        models(Iocean)%mesh(2)%gtype = Idot
        models(Iocean)%mesh(3)%gtype = Iupoint
        models(Iocean)%mesh(4)%gtype = Ivpoint
      end if
!
!-----------------------------------------------------------------------
!     Set staggering type 
!-----------------------------------------------------------------------
!
      do i = 1, 4 
      if (models(Iocean)%mesh(i)%gtype == Iupoint) then
        staggerLoc = ESMF_STAGGERLOC_EDGE1
        staggerEdgeLWidth = (/0,1/)
        staggerEdgeUWidth = (/1,1/)
      else if (models(Iocean)%mesh(i)%gtype == Ivpoint) then
        staggerLoc = ESMF_STAGGERLOC_EDGE2
        staggerEdgeLWidth = (/1,0/)
        staggerEdgeUWidth = (/1,1/)
      else if (models(Iocean)%mesh(i)%gtype == Icross) then
        staggerLoc = ESMF_STAGGERLOC_CENTER
        staggerEdgeLWidth = (/1,1/)
        staggerEdgeUWidth = (/1,1/)
      else if (models(Iocean)%mesh(i)%gtype == Idot) then
        staggerLoc = ESMF_STAGGERLOC_CORNER
        staggerEdgeLWidth = (/0,0/)
        staggerEdgeUWidth = (/1,1/)
      end if
!
!-----------------------------------------------------------------------
!     Create ESMF Grid
!-----------------------------------------------------------------------
!
      if (i == 1) then
      models(Iocean)%grid = ESMF_GridCreate(distgrid=distGrid,          &
                                            gridEdgeLWidth=(/1,1/),     &
                                            gridEdgeUWidth=(/1,1/),     &
                                            indexflag=ESMF_INDEX_GLOBAL,&
                                            name="ocn_grid",            &
                                            rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
      end if
!
!-----------------------------------------------------------------------
!     Allocate coordinates 
!-----------------------------------------------------------------------
!
      call ESMF_GridAddCoord(models(Iocean)%grid,                       &
                             staggerLoc=staggerLoc,                     &
                             staggerEdgeLWidth=staggerEdgeLWidth,       &
                             staggerEdgeUWidth=staggerEdgeUWidth,       &
                             rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Allocate items for masking
!-----------------------------------------------------------------------
!
      call ESMF_GridAddItem(models(Iocean)%grid,                        &
                            staggerLoc=staggerLoc,                      &
                            itemflag=ESMF_GRIDITEM_MASK,                &
                            rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!
!-----------------------------------------------------------------------
!     Get number of local DEs
!-----------------------------------------------------------------------
! 
      call ESMF_GridGet(models(Iocean)%grid,                            &
                        localDECount=localDECount,                      &
                        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Get pointers and set coordinates for the grid 
!-----------------------------------------------------------------------
! 
      do j = 0, localDECount-1
      call ESMF_GridGetCoord(models(Iocean)%grid,                       &
                             localDE=j,                                 &
                             staggerLoc=staggerLoc,                     &
                             coordDim=1,                                &
                             farrayPtr=ptrX,                            &
                             rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      call ESMF_GridGetCoord(models(Iocean)%grid,                       &
                             localDE=j,                                 &
                             staggerLoc=staggerLoc,                     &
                             coordDim=2,                                &
                             farrayPtr=ptrY,                            &
                             rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      call ESMF_GridGetItem (models(Iocean)%grid,                       &
                             localDE=j,                                 &
                             staggerLoc=staggerLoc,                     &
                             itemflag=ESMF_GRIDITEM_MASK,               &
                             farrayPtr=ptrM,                            &
                             rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Debug: write size of pointers    
!-----------------------------------------------------------------------
!
      name = GRIDDES(models(Iocean)%mesh(i)%gtype)
!
      if (debugLevel > 0) then
        write(*,30) localPet, j, adjustl("PTR/OCN/GRD/"//name),         &
                    lbound(ptrX, dim=1), ubound(ptrX, dim=1),           &
                    lbound(ptrX, dim=2), ubound(ptrX, dim=2)
      end if
!
!-----------------------------------------------------------------------
!     Fill the pointers    
!-----------------------------------------------------------------------
!
      if (models(Iocean)%mesh(i)%gtype == Idot) then
        if (debugLevel > 0) then
        write(*,30) localPet, j, adjustl("DAT/OCN/GRD/"//name),         &
        lbound(GRID(ng)%lonp, dim=1), ubound(GRID(ng)%lonp, dim=1),     &
        lbound(GRID(ng)%lonp, dim=2), ubound(GRID(ng)%lonp, dim=2)
        end if
!
        do jj = JstrV, JendR
          do ii = IstrU, IendR
            ptrX(ii,jj) = GRID(ng)%lonp(ii,jj)
            ptrY(ii,jj) = GRID(ng)%latp(ii,jj)
            ptrM(ii,jj) = int(GRID(ng)%pmask(ii,jj))
          end do
        end do
      else if (models(Iocean)%mesh(i)%gtype == Icross) then
        if (debugLevel > 0) then
        write(*,30) localPet, j, adjustl("DAT/OCN/GRD/"//name),         &
        lbound(GRID(ng)%lonr, dim=1), ubound(GRID(ng)%lonr, dim=1),     &
        lbound(GRID(ng)%lonr, dim=2), ubound(GRID(ng)%lonr, dim=2)
        end if
!
        do jj = JstrR, JendR
          do ii = IstrR, IendR
            ptrX(ii,jj) = GRID(ng)%lonr(ii,jj)
            ptrY(ii,jj) = GRID(ng)%latr(ii,jj)
            ptrM(ii,jj) = int(GRID(ng)%rmask(ii,jj))
          end do
        end do
      else if (models(Iocean)%mesh(i)%gtype == Iupoint) then
        if (debugLevel > 0) then
        write(*,30) localPet, j, adjustl("DAT/OCN/GRD/"//name),         &
        lbound(GRID(ng)%lonu, dim=1), ubound(GRID(ng)%lonu, dim=1),     &
        lbound(GRID(ng)%lonu, dim=2), ubound(GRID(ng)%lonu, dim=2)
        end if
!
        do jj = JstrU, JendU
          do ii = IstrU, IendU
            ptrX(ii,jj) = GRID(ng)%lonu(ii,jj)
            ptrY(ii,jj) = GRID(ng)%latu(ii,jj)
            ptrM(ii,jj) = int(GRID(ng)%umask(ii,jj))
          end do
        end do
      else if (models(Iocean)%mesh(i)%gtype == Ivpoint) then
        if (debugLevel > 0) then
        write(*,30) localPet, j, adjustl("DAT/OCN/GRD/"//name),         &
        lbound(GRID(ng)%lonv, dim=1), ubound(GRID(ng)%lonv, dim=1),     &
        lbound(GRID(ng)%lonv, dim=2), ubound(GRID(ng)%lonv, dim=2)
        end if
!
        do jj = JstrV, JendV
          do ii = IstrV, IendV
            ptrX(ii,jj) = GRID(ng)%lonv(ii,jj)
            ptrY(ii,jj) = GRID(ng)%latv(ii,jj)
            ptrM(ii,jj) = int(GRID(ng)%vmask(ii,jj))
          end do
        end do
      end if
!
!-----------------------------------------------------------------------
!     Nullify pointers 
!-----------------------------------------------------------------------
!
      if (associated(ptrY)) then
        nullify(ptrY)
      end if
      if (associated(ptrX)) then
        nullify(ptrX)
      end if
      if (associated(ptrM)) then
        nullify(ptrM)
      end if
      end do
!
!-----------------------------------------------------------------------
!     Assign grid to gridded component 
!-----------------------------------------------------------------------
!
      call ESMF_GridCompSet(gcomp, grid=models(Iocean)%grid, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Debug: write out component grid in VTK format 
!-----------------------------------------------------------------------
!
      if (debugLevel > 1) then
      call ESMF_GridWriteVTK(models(Iocean)%grid,                       &
                         filename="ocean_"//                            &
                         trim(GRIDDES(models(Iocean)%mesh(i)%gtype))//  &
                         "point",                                       &
                         staggerLoc=staggerLoc,                         &
                         rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
      end if
      end do 

!-----------------------------------------------------------------------
!     Format definition 
!-----------------------------------------------------------------------
!
 30   format(" PET(",I3,") - DE(",I2,") - ", A20, " : ", 4I8)
!
      end subroutine OCN_SetGridArrays
!
      subroutine OCN_SetStates(gcomp, importState, exportState, rc)
!
!-----------------------------------------------------------------------
!     Used module declarations 
!-----------------------------------------------------------------------
!
      use mod_param, only : NtileI, NtileJ, BOUNDS, N, Lm, Mm, Ngrids
!
      implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
      type(ESMF_GridComp), intent(in) :: gcomp
      type(ESMF_State), intent(inout) :: importState
      type(ESMF_State), intent(inout) :: exportState
      integer, intent(out) :: rc
!
!-----------------------------------------------------------------------
!     Local variable declarations 
!-----------------------------------------------------------------------
!
      integer :: i, j, k, ng, itemCount, localDECount, localPet
      integer :: IstrR, IendR, JstrR, JendR
      integer :: IstrU, IendU, JstrU, JendU     
      integer :: IstrV, IendV, JstrV, JendV
      integer :: staggerEdgeLWidth(2)
      integer :: staggerEdgeUWidth(2)
      integer :: TLW(2), TUW(2)
      integer, allocatable :: TLWidth(:,:), TUWidth(:,:)
      character(ESMF_MAXSTR) :: cname, msgString
      character(ESMF_MAXSTR), allocatable :: itemNameList(:)
      real*8, dimension(:,:), pointer :: ptr
!
      type(ESMF_VM) :: vm
      type(ESMF_Field) :: field
      type(ESMF_ArraySpec) :: arraySpec
      type(ESMF_StaggerLoc) :: staggerLoc 
!
      rc = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!     Check number of nested grids 
!-----------------------------------------------------------------------
!
      call ESMF_GridCompGet(gcomp, name=cname, vm=vm, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      call ESMF_VMGet(vm, localPet=localPet, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      if (Ngrids > 1) then
        write(msgString,'(A,I3)') trim(cname)//                         &
              ': number of nested grid is', Ngrids,                     &
              'coupled model only interactes with outermost one!'
        call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_ERROR)
        ng = 1
      else
        ng = Ngrids
      end if
!
!-----------------------------------------------------------------------
!     Allocate arrays 
!-----------------------------------------------------------------------
!
      if (.not. allocated(TLWidth)) then
        allocate(TLWidth(2,0:NtileI(ng)*NtileJ(ng)-1))
        allocate(TUWidth(2,0:NtileI(ng)*NtileJ(ng)-1))
      end if
!
!-----------------------------------------------------------------------
!     Get limits of the arrays (based on PET)
!-----------------------------------------------------------------------
!
      IstrR = BOUNDS(ng)%IstrR(localPet)
      IendR = BOUNDS(ng)%IendR(localPet)
      JstrR = BOUNDS(ng)%JstrR(localPet)
      JendR = BOUNDS(ng)%JendR(localPet)
!
      IstrU = BOUNDS(ng)%Istr(localPet)
      IendU = BOUNDS(ng)%IendR(localPet)
      JstrU = BOUNDS(ng)%JstrR(localPet)
      JendU = BOUNDS(ng)%JendR(localPet)
!
      IstrV = BOUNDS(ng)%IstrR(localPet)
      IendV = BOUNDS(ng)%IendR(localPet)
      JstrV = BOUNDS(ng)%Jstr(localPet)
      JendV = BOUNDS(ng)%JendR(localPet)
!
      TLW(1)=BOUNDS(ng)%Istr(localPet)-BOUNDS(ng)%LBi(localPet)
      TLW(2)=BOUNDS(ng)%Jstr(localPet)-BOUNDS(ng)%LBj(localPet)
      TUW(1)=BOUNDS(ng)%UBi(localPet)-BOUNDS(ng)%Iend(localPet)
      TUW(2)=BOUNDS(ng)%UBj(localPet)-BOUNDS(ng)%Jend(localPet) 
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
      call ESMF_StateGet(exportState, itemNameList=itemNameList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Create export fields 
!-----------------------------------------------------------------------
!
      do i = 1, itemCount
      k = get_varid(models(Iocean)%exportField, trim(itemNameList(i)))
!
!-----------------------------------------------------------------------
!     Set staggering type 
!-----------------------------------------------------------------------
!
      if (models(Iocean)%exportField(k)%gtype == Iupoint) then
        staggerLoc = ESMF_STAGGERLOC_EDGE1
        staggerEdgeLWidth = (/0,1/)
        staggerEdgeUWidth = (/1,1/)
      else if (models(Iocean)%exportField(k)%gtype == Ivpoint) then
        staggerLoc = ESMF_STAGGERLOC_EDGE2
        staggerEdgeLWidth = (/1,0/)
        staggerEdgeUWidth = (/1,1/)
      else if (models(Iocean)%exportField(k)%gtype == Icross) then
        staggerLoc = ESMF_STAGGERLOC_CENTER
        staggerEdgeLWidth = (/1,1/)
        staggerEdgeUWidth = (/1,1/)
      else if (models(Iocean)%exportField(k)%gtype == Idot) then
        staggerLoc = ESMF_STAGGERLOC_CORNER
        staggerEdgeLWidth = (/0,0/)
        staggerEdgeUWidth = (/1,1/)
      end if
!
!-----------------------------------------------------------------------
!     Create field 
!-----------------------------------------------------------------------
!
      field = ESMF_FieldCreate(models(Iocean)%grid,                     &
                               arraySpec,                               &
                               staggerloc=staggerLoc,                   &
                               name=trim(itemNameList(i)),              &
                               rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Get number of local DEs
!-----------------------------------------------------------------------
! 
      call ESMF_GridGet(models(Iocean)%grid,                            &
                        localDECount=localDECount,                      &
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
      if (associated(ptr)) nullify(ptr)
!
      end do
!
!-----------------------------------------------------------------------
!     Add field export state
!-----------------------------------------------------------------------
!
      call NUOPC_StateRealizeField(exportState, field=field, rc=rc) 
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
      end do
!
!-----------------------------------------------------------------------
!     Get list of import fields 
!-----------------------------------------------------------------------
!
      call ESMF_StateGet(importState, itemCount=itemCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      if (allocated(itemNameList)) deallocate(itemNameList)
      if (.not. allocated(itemNameList)) then
        allocate(itemNameList(itemCount))
      end if
      call ESMF_StateGet(importState, itemNameList=itemNameList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Create import fields 
!-----------------------------------------------------------------------
!
      do i = 1, itemCount
      k = get_varid(models(Iocean)%importField, trim(itemNameList(i)))
!
!-----------------------------------------------------------------------
!     Set staggering type 
!-----------------------------------------------------------------------
!
      if (models(Iocean)%importField(k)%gtype == Iupoint) then
        staggerLoc = ESMF_STAGGERLOC_EDGE1
        staggerEdgeLWidth = (/0,1/)
        staggerEdgeUWidth = (/1,1/)
      else if (models(Iocean)%importField(k)%gtype == Ivpoint) then
        staggerLoc = ESMF_STAGGERLOC_EDGE2
        staggerEdgeLWidth = (/1,0/)
        staggerEdgeUWidth = (/1,1/)
      else if (models(Iocean)%importField(k)%gtype == Icross) then
        staggerLoc = ESMF_STAGGERLOC_CENTER
        staggerEdgeLWidth = (/1,1/)
        staggerEdgeUWidth = (/1,1/)
      else if (models(Iocean)%importField(k)%gtype == Idot) then
        staggerLoc = ESMF_STAGGERLOC_CORNER
        staggerEdgeLWidth = (/0,0/)
        staggerEdgeUWidth = (/1,1/)
      end if
!
!-----------------------------------------------------------------------
!     Create field 
!-----------------------------------------------------------------------
!
      field = ESMF_FieldCreate(models(Iocean)%grid,                     &
                               arraySpec,                               &
                               totalLWidth=TLW,                         &
                               totalUWidth=TUW,                         &
                               staggerloc=staggerLoc,                   &
                               indexflag=ESMF_INDEX_GLOBAL,             &
                               name=trim(itemNameList(i)),              &
                               rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Get number of local DEs
!-----------------------------------------------------------------------
! 
      call ESMF_GridGet(models(Iocean)%grid,                            &
                        localDECount=localDECount,                      &
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
      end do
!
!-----------------------------------------------------------------------
!     Add field import state
!-----------------------------------------------------------------------
!
      call NUOPC_StateRealizeField(importState, field=field, rc=rc) 
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
      end do
!
      end subroutine OCN_SetStates
!
      subroutine OCN_ModelAdvance(gcomp, rc)
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
      type(ESMF_Clock) :: clock
      type(ESMF_State) :: importState, exportState
!
      rc = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!     Retrieve component information 
!-----------------------------------------------------------------------
!
      call ESMF_GridCompGet(gcomp, clock=clock, importState=importState,&
                            exportState=exportState, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Debug: write current time
!-----------------------------------------------------------------------
!
      call NUOPC_ClockPrintCurrTime(clock,                              &
                                    '--> Running OCN component from: ', &
                                    rc=rc)
!
      end subroutine OCN_ModelAdvance
!
      end module mod_esmf_ocn
