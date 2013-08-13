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
#define FILENAME "mod_esmf_ocn_mit.F90"
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
          NUOPC_SetServices          => routine_SetServices,            &
          NUOPC_Label_Advance        => label_Advance,                  &
          NUOPC_Label_DataInitialize => label_DataInitialize,           &
          NUOPC_Model_Type_IS        => type_InternalState,             &
          NUOPC_Model_Label_IS       => label_InternalState,            &
          NUOPC_Label_SetClock       => label_SetClock,                 &
          NUOPC_Label_CheckImport    => label_CheckImport
!
      use mod_types
      use mod_utils
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
      integer, intent(out) :: rc
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
!     Attach phase independent specializing methods
!     Setting the slow and fast model clocks 
!-----------------------------------------------------------------------
!
      call ESMF_MethodAdd(gcomp, label=NUOPC_Label_DataInitialize,      &
                          userRoutine=OCN_DataInit, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      call ESMF_MethodAdd(gcomp, label=NUOPC_Label_SetClock,            &
                          userRoutine=OCN_SetClock, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      call ESMF_MethodAdd(gcomp, label=NUOPC_Label_CheckImport,         &
                          userRoutine=OCN_CheckImport, index=1, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      call ESMF_MethodAdd(gcomp, label=NUOPC_Label_Advance,             &
                          userRoutine=OCN_ModelAdvance, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Register finalize routine    
!-----------------------------------------------------------------------
! 
!      call ESMF_GridCompSetEntryPoint(gcomp,                            &
!                                      methodflag=ESMF_METHOD_FINALIZE,  &
!                                      userRoutine=OCN_SetFinalize,      &
!                                      rc=rc)
!      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
!                             line=__LINE__, file=FILENAME)) return
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
      real*8 :: myTime
      integer :: comm, localPet, petCount
      integer :: iLoop, myIter, mythid
      character(ESMF_MAXSTR) :: gname
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
      iLoop = 0
      myTime = 0.
      myIter = 0
      myThid = 1
!
      call MIT_INIT(comm, iLoop, myTime, myIter, myThid) 
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
      call OCN_SetStates(gcomp, rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      end subroutine OCN_SetInitializeP2
!
      subroutine OCN_DataInit(gcomp, rc)
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
      type(ESMF_Time) :: currTime
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
      if (restarted .and. currTime == esmRestartTime) then
        call OCN_Put(gcomp, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
                               line=__LINE__, file=FILENAME)) return
      end if
!
      end subroutine OCN_DataInit
!
      subroutine OCN_SetClock(gcomp, rc)
!
!-----------------------------------------------------------------------
!     Used module declarations 
!-----------------------------------------------------------------------
!
      use mod_mit_gcm, only : TheCalendar, startdate_1, startdate_2 
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
      integer :: localPet, petCount, fac1, fac2, maxdiv
      integer :: ref_year,   str_year,   end_year
      integer :: ref_month,  str_month,  end_month
      integer :: ref_day,    str_day,    end_day
      integer :: ref_hour,   str_hour,   end_hour
      integer :: ref_minute, str_minute, end_minute
      integer :: ref_second, str_second, end_second
      character (len=80) :: calendar
!
      type(ESMF_VM) :: vm
      type(ESMF_Clock) :: cmpClock
      type(ESMF_TimeInterval) :: timeStep
      type(ESMF_Time) :: cmpRefTime, cmpStartTime, cmpStopTime
      type(ESMF_Time) :: startTime, currTime
      type(ESMF_Calendar) :: cal 
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
!     Create gridded component clock 
!-----------------------------------------------------------------------
!
      if (trim(TheCalendar) == 'gregorian') then
        ref_year=1582
        ref_month=10
        ref_day=15
        ref_hour=0
        ref_minute=1
        ref_second=1
        calendar='gregorian'
        cal = ESMF_CalendarCreate(ESMF_CALKIND_GREGORIAN,               &
                                  name=trim(calendar), rc=rc)
      else if (trim(TheCalendar) == 'model') then
        ref_year=0
        ref_month=1
        ref_day=1
        ref_hour=0
        ref_minute=1
        ref_second=1
        calendar='360_day'
        cal = ESMF_CalendarCreate(ESMF_CALKIND_360DAY,                  &
                                  name=trim(calendar), rc=rc)
      end if
!
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Set reference time
!-----------------------------------------------------------------------
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
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Set start time
!-----------------------------------------------------------------------
!
      str_year = startdate_1/10000
      str_month = mod(startdate_1/100,100)
      str_day = mod(startdate_1,100)
      str_hour = 0
      str_minute = 0
      str_second = 0
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
      end_year = startdate_2/10000
      if (end_year == 0) then
        end_year = 2100
        end_month = 1
        end_day = 1
      else
        end_month = mod(startdate_2/100,100)
        end_day = mod(startdate_2,100)
      end if
      end_hour = 0
      end_minute = 0
      end_second = 0
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
      if (restarted) then
        startTime = esmRestartTime
      else
        startTime = esmStartTime
      end if
!
      if (cmpStartTime /= startTime) then
        call ESMF_TimePrint(cmpStartTime, options="string", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
                               line=__LINE__, file=FILENAME)) return
!
        call ESMF_TimePrint(startTime, options="string", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
                               line=__LINE__, file=FILENAME)) return
!
        call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc,              &
             msg='ESM and OCN start times do not match: '//             &
             'please check the config files')
        return
      end if
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
             msg='ESM and OCN calendars do not match: '//               &
             'please check the config files')
        return
      end if
!
!-----------------------------------------------------------------------
!     Modify component clock time step 
!-----------------------------------------------------------------------
!
      fac1 = maxval(connectors(Iocean,:)%divDT,mask=models(:)%modActive)
      fac2 = maxval(connectors(:,Iocean)%divDT,mask=models(:)%modActive)
      maxdiv = max(fac1, fac2)
!
      call ESMF_ClockSet(cmpClock, name='ocn_clock',                    &
                         refTime=cmpRefTime, timeStep=timeStep/maxdiv,  &
                         startTime=cmpStartTime, stopTime=cmpStopTime,  &
                         rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      end subroutine OCN_SetClock 
!
      subroutine OCN_CheckImport(gcomp, rc)
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
      integer :: itemCount
      logical :: atCorrectTime
      character(ESMF_MAXSTR), allocatable :: itemNameList(:)
!
      type(NUOPC_Model_Type_IS) :: is
      type(ESMF_Time) :: startTime, currTime
      type(ESMF_Clock) :: clock
      type(ESMF_Field) :: field
      type(ESMF_State) :: importState
!
      rc = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!     Get component for its internal state 
!-----------------------------------------------------------------------
!
      nullify(is%wrap)
!
      call ESMF_UserCompGetInternalState(gcomp, NUOPC_Model_Label_IS,   &
                                         is, rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Get the start time and current time out of the clock
!-----------------------------------------------------------------------
!
      call ESMF_ClockGet(is%wrap%driverClock, startTime=startTime,      &
                         currTime=currTime, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      call ESMF_GridCompGet(gcomp, importState=importState, rc=rc)
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
      call ESMF_StateGet(importState, itemNameList=itemNameList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Check fields in the importState (fast time step) 
!-----------------------------------------------------------------------
!
      if (itemCount > 0) then
      call ESMF_StateGet(importState, itemName=trim(itemNameList(1)),   &
                         field=field, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      atCorrectTime = NUOPC_FieldIsAtTime(field, currTime, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      if (.not. atCorrectTime) then
        call ESMF_LogSetError(ESMF_RC_ARG_BAD,                          &
                              msg="NUOPC INCOMPATIBILITY DETECTED: "//  &
                              "Import Fields not at correct time",      &
                              line=__LINE__, file=FILENAME,             &
                              rcToReturn=rc)
        return
      end if
!
      end if
!
      end subroutine OCN_CheckImport
!
      subroutine OCN_SetGridArrays(gcomp, localPet, rc)
!
!-----------------------------------------------------------------------
!     Used module declarations 
!-----------------------------------------------------------------------
!
      use mod_mit_gcm, only : sNx, sNy, OLx, OLy, nPx, nPy, Nx, Ny,     &
                              xC, yC, xG, yG, maskC, maskW, maskS, rA
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
      integer :: i, j, localDECount, p 
      character(ESMF_MAXSTR) :: name
!
      integer(ESMF_KIND_I4), pointer :: ptrM(:,:)
      real(ESMF_KIND_R8), pointer :: ptrX(:,:), ptrY(:,:), ptrA(:,:)
      type(ESMF_VM) :: vm
      type(ESMF_StaggerLoc) :: staggerLoc
      type(ESMF_DistGrid) :: distGrid
!
      rc = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!     Create ESMF DistGrid based on model domain decomposition
!-----------------------------------------------------------------------
!
      distGrid = ESMF_DistGridCreate(minIndex=(/ 1, 1 /),               &
                                     maxIndex=(/ Ny, Nx /),             &
                                     regDecomp=(/ nPy, nPx /),          &
                                     rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Define data structure for component grid
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
      do p = 1, 4 
!
      if (models(Iocean)%mesh(p)%gtype == Iupoint) then
        staggerLoc = ESMF_STAGGERLOC_EDGE1
      else if (models(Iocean)%mesh(p)%gtype == Ivpoint) then
        staggerLoc = ESMF_STAGGERLOC_EDGE2
      else if (models(Iocean)%mesh(p)%gtype == Icross) then
        staggerLoc = ESMF_STAGGERLOC_CENTER
      else if (models(Iocean)%mesh(p)%gtype == Idot) then
        staggerLoc = ESMF_STAGGERLOC_CORNER
      end if
!
!-----------------------------------------------------------------------
!     Create ESMF Grid
!-----------------------------------------------------------------------
!
      if (p == 1) then
      models(Iocean)%grid = ESMF_GridCreate(distgrid=distGrid,          &
                                            gridEdgeLWidth=(/0,0/),     &
                                            gridEdgeUWidth=(/0,0/),     &
                                            indexflag=ESMF_INDEX_DELOCAL,&
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
                             rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Allocate items for masking (not for cell corners)
!-----------------------------------------------------------------------
!
      if (models(Iocean)%mesh(p)%gtype /= Idot) then
      call ESMF_GridAddItem(models(Iocean)%grid,                        &
                            staggerLoc=staggerLoc,                      &
                            itemflag=ESMF_GRIDITEM_MASK,                &
                            rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      end if
!
!-----------------------------------------------------------------------
!     Allocate items for grid area (only for cell center)
!-----------------------------------------------------------------------
!
      if (models(Iocean)%mesh(p)%gtype == Icross) then
      call ESMF_GridAddItem(models(Iocean)%grid,                        &
                            staggerLoc=staggerLoc,                      &
                            itemflag=ESMF_GRIDITEM_AREA,                &
                            rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      end if
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
      if (models(Iocean)%mesh(p)%gtype /= Idot) then
      call ESMF_GridGetItem (models(Iocean)%grid,                       &
                             localDE=j,                                 &
                             staggerLoc=staggerLoc,                     &
                             itemflag=ESMF_GRIDITEM_MASK,               &
                             farrayPtr=ptrM,                            &
                             rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
      end if
!
      if (models(Iocean)%mesh(p)%gtype == Icross) then
      call ESMF_GridGetItem (models(Iocean)%grid,                       &
                             localDE=j,                                 &
                             staggerLoc=staggerLoc,                     &
                             itemflag=ESMF_GRIDITEM_AREA,               &
                             farrayPtr=ptrA,                            &
                             rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
      end if
!
!-----------------------------------------------------------------------
!     Debug: write size of pointers    
!-----------------------------------------------------------------------
!
      name = GRIDDES(models(Iocean)%mesh(p)%gtype)
!
      if (debugLevel > 0) then
        write(*,30) localPet, 0, adjustl("PTR/OCN/GRD/"//name),         &
                    lbound(ptrX, dim=2), ubound(ptrX, dim=2),           &
                    lbound(ptrX, dim=1), ubound(ptrX, dim=1)
      end if
!
!-----------------------------------------------------------------------
!     Fill the pointers    
!-----------------------------------------------------------------------
!
      if (models(Iocean)%mesh(p)%gtype == Idot) then
         ptrX = transpose(xG(1:sNx,1:sNy,1,1))
         ptrY = transpose(yG(1:sNx,1:sNy,1,1))
      else if (models(Iocean)%mesh(p)%gtype == Icross) then
         ptrX = transpose(xC(1:sNx,1:sNy,1,1))
         ptrY = transpose(yC(1:sNx,1:sNy,1,1))
         ptrM = int(transpose(maskC(1:sNx,1:sNy,1,1,1)))
         ptrA = transpose(rA(1:sNx,1:sNy,1,1))
      else if (models(Iocean)%mesh(p)%gtype == Iupoint) then
         ptrX = transpose(xG(1:sNx,1:sNy,1,1))
         ptrY = transpose(yC(1:sNx,1:sNy,1,1))
         ptrM = int(transpose(maskW(1:sNx,1:sNy,1,1,1)))
      else if (models(Iocean)%mesh(p)%gtype == Ivpoint) then
         ptrX = transpose(xC(1:sNx,1:sNy,1,1))
         ptrY = transpose(yG(1:sNx,1:sNy,1,1))
         ptrM = int(transpose(maskS(1:sNx,1:sNy,1,1,1)))
      end if
!
      if (debugLevel > 0) then
        write(*,30) localPet, j, adjustl("DAT/OCN/GRD/"//name),         &
                    1, sNx,1 ,sNy
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
      if (associated(ptrA)) then
        nullify(ptrA)
      end if
!
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
                         trim(GRIDDES(models(Iocean)%mesh(p)%gtype))//  &
                         "point",                                       &
                         staggerLoc=staggerLoc,                         &
                         rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
      end if
      end do     
!
!-----------------------------------------------------------------------
!     Format definition 
!-----------------------------------------------------------------------
!
 20   format(" RIVER(",I2.2,") - ",I3,2F6.2," [",I3.3,":",I3.3,"] - ",I2)
 30   format(" PET(",I3.3,") - DE(",I2.2,") - ", A20, " : ", 4I8)
!
      end subroutine OCN_SetGridArrays
!
      subroutine OCN_SetStates(gcomp, rc)
!
!-----------------------------------------------------------------------
!     Used module declarations 
!-----------------------------------------------------------------------
!
      use mod_mit_gcm, only : OLx, OLy
!
      implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
      type(ESMF_GridComp), intent(in) :: gcomp
      integer, intent(out) :: rc
!
!-----------------------------------------------------------------------
!     Local variable declarations 
!-----------------------------------------------------------------------
!
      integer :: i, j, k, itemCount, localDECount, localPet, petCount
      integer :: staggerEdgeLWidth(2)
      integer :: staggerEdgeUWidth(2)
      character(ESMF_MAXSTR), allocatable :: itemNameList(:)
      real*8, dimension(:,:), pointer :: ptr2d
!
      type(ESMF_VM) :: vm
      type(ESMF_Field) :: field
      type(ESMF_ArraySpec) :: arraySpec
      type(ESMF_StaggerLoc) :: staggerLoc 
      type(ESMF_State) :: importState, exportState
!
!-----------------------------------------------------------------------
!     Get information about gridded component 
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
      call ESMF_GridGet(models(Iocean)%grid,                            &
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
      else if (models(Iocean)%exportField(k)%gtype == Ivpoint) then
        staggerLoc = ESMF_STAGGERLOC_EDGE2
      else if (models(Iocean)%exportField(k)%gtype == Icross) then
        staggerLoc = ESMF_STAGGERLOC_CENTER
      else if (models(Iocean)%exportField(k)%gtype == Idot) then
        staggerLoc = ESMF_STAGGERLOC_CORNER
      end if
!
      staggerEdgeLWidth = (/0,0/)
      staggerEdgeUWidth = (/0,0/)
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
!     Put initial data into state 
!-----------------------------------------------------------------------
! 
      do j = 0, localDECount-1
!
!-----------------------------------------------------------------------
!     Get pointer from field 
!-----------------------------------------------------------------------
!
      call ESMF_FieldGet(field, localDe=j, farrayPtr=ptr2d, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Initialize pointer 
!-----------------------------------------------------------------------
!
      ptr2d = MISSING_R8
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
!     Add field export state
!-----------------------------------------------------------------------
!
      call NUOPC_StateRealizeField(exportState, field=field, rc=rc) 
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
      else if (models(Iocean)%importField(k)%gtype == Ivpoint) then
        staggerLoc = ESMF_STAGGERLOC_EDGE2
      else if (models(Iocean)%importField(k)%gtype == Icross) then
        staggerLoc = ESMF_STAGGERLOC_CENTER
      else if (models(Iocean)%importField(k)%gtype == Idot) then
        staggerLoc = ESMF_STAGGERLOC_CORNER
      end if
!
      staggerEdgeLWidth = (/0,0/)
      staggerEdgeUWidth = (/0,0/)
!
!-----------------------------------------------------------------------
!     Create field
!-----------------------------------------------------------------------
!
      field = ESMF_FieldCreate(models(Iocean)%grid,                     &
                               arraySpec,                               &
                               totalLWidth=(/OLy,OLx/),                 &
                               totalUWidth=(/OLy,OLx/),                 &
                               staggerloc=staggerLoc,                   &
                               name=trim(itemNameList(i)),              &
                               rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Store routehandle to exchage halo region data 
!     *Only for first import field becuase it is same for the others
!-----------------------------------------------------------------------
!
!      if (k == 1) then
!      call ESMF_FieldHaloStore(field,                                   &
!              routehandle=models(Iocean)%importField(k)%rhandle, rc=rc)
!      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
!                             line=__LINE__, file=FILENAME)) return
!      end if
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
      call ESMF_FieldGet(field, localDe=j, farrayPtr=ptr2d, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Initialize pointer 
!-----------------------------------------------------------------------
!
      ptr2d = MISSING_R8
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
!     Add field import state
!-----------------------------------------------------------------------
!
      call NUOPC_StateRealizeField(importState, field=field, rc=rc) 
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
      end subroutine OCN_SetStates 
!
      subroutine OCN_ModelAdvance(gcomp, rc)
!
!-----------------------------------------------------------------------
!     Used module declarations 
!-----------------------------------------------------------------------
!
      use mod_mit_gcm, only : deltaT
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
      real*8 :: trun, myTime
      integer :: localPet, petCount, phase, iter
      integer :: iLoop, myIter, myThid 
      character(ESMF_MAXSTR) :: str1, str2
!     
      type(ESMF_VM) :: vm
      type(ESMF_Clock) :: clock
      type(ESMF_TimeInterval) :: timeStep
      type(ESMF_Time) :: refTime, stopTime, currTime
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
      call ESMF_ClockGet(clock, timeStep=timeStep, refTime=refTime, &
                         stopTime=stopTime, currTime=currTime, rc=rc) 
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Get time interval 
!-----------------------------------------------------------------------
!
      call ESMF_TimeIntervalGet(timeStep, s_r8=trun, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Debug: write time information 
!-----------------------------------------------------------------------
!
      iter = int(trun/deltaT)
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
        else
          write(*,50) trim(str1), trim(str2), phase, dble(iter)
        end if
      end if
!
!-----------------------------------------------------------------------
!     Get import fields 
!-----------------------------------------------------------------------
!
      if ((currTime /= refTime) .or. restarted) then
        call OCN_Get(gcomp, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
                               line=__LINE__, file=FILENAME)) return
      end if
!
!-----------------------------------------------------------------------
!     Run OCN component
!-----------------------------------------------------------------------
!
      iLoop = 0
      myTime = 0.
      myIter = 0
      myThid = 1
!
      call MIT_RUN(iter, iLoop, myTime, myIter, myThid)
!
!-----------------------------------------------------------------------
!     Put export fields 
!-----------------------------------------------------------------------
!
      call OCN_Put(gcomp, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Formats 
!-----------------------------------------------------------------------
!
 40   format(' Running OCN Component: ',A,' --> ',A,' Phase: ',I1)
 50   format(' Running OCN Component: ',A,' --> ',A,' Phase: ',I1,      &
             ' [', E12.2, ']')
!
      end subroutine OCN_ModelAdvance
!
      subroutine OCN_Get(gcomp, rc)
!
!-----------------------------------------------------------------------
!     Used module declarations 
!-----------------------------------------------------------------------
!
      use mod_mit_gcm, only : sNx, sNy
      use mod_mit_gcm, only : ustress, vstress, hflux, sflux, swflux,   &
                              uwind, vwind, atemp, aqh, lwflux, evap,   &
                              precip, runoff, swdown, lwdown, apressure,&
                              wspeed, snowprecip 
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
      integer :: i, j, ii, jj
      integer :: id, iyear, iday, imonth, ihour, iunit
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
                        dd=iday, h=ihour, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
      end if
!
!-----------------------------------------------------------------------
!     Get number of local DEs
!-----------------------------------------------------------------------
! 
      call ESMF_GridGet(models(Iocean)%grid,                            &
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
!     Loop over excahange fields 
!-----------------------------------------------------------------------
!
      do i = 1, itemCount
!
      id = get_varid(models(Iocean)%importField, itemNameList(i)) 
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
      write(*,60) localPet, j, adjustl("PTR/OCN/IMP/"//itemNameList(i)),&
                  lbound(ptr, dim=1), ubound(ptr, dim=1),               &
                  lbound(ptr, dim=2), ubound(ptr, dim=2)
      write(*,60) localPet, j, adjustl("IND/OCN/IMP/"//itemNameList(i)),&
                  1, sNx, 1, sNy 
      end if 
!
!-----------------------------------------------------------------------
!     Put data to OCN component variable
!-----------------------------------------------------------------------
!
      sfac = models(Iocean)%importField(id)%scale_factor
      addo = models(Iocean)%importField(id)%add_offset
!
      select case (trim(adjustl(itemNameList(i))))
      case ('taux')
        where (transpose(ptr) < TOL_R8)
          ustress(1:sNx,1:sNy,1,1) = (transpose(ptr)*sfac)+addo
        end where
      case ('tauy')
        vstress(1:sNx,1:sNy,1,1) = (transpose(ptr)*sfac)+addo
      case ('nflx')
        hflux(1:sNx,1:sNy,1,1) = (transpose(ptr)*sfac)+addo
      case ('sflx')
        sflux(1:sNx,1:sNy,1,1) = (transpose(ptr)*sfac)+addo
      case ('swrd')
        swflux(1:sNx,1:sNy,1,1) = (transpose(ptr)*sfac)+addo
      case ('wndu')
        uwind(1:sNx,1:sNy,1,1) = (transpose(ptr)*sfac)+addo 
      case ('wndv')
        vwind(1:sNx,1:sNy,1,1) = (transpose(ptr)*sfac)+addo 
      case ('wspd')
        wspeed(1:sNx,1:sNy,1,1) = (transpose(ptr)*sfac)+addo
      case ('tsfc')
        atemp(1:sNx,1:sNy,1,1) = (transpose(ptr)*sfac)+addo
      case ('qsfc')
        aqh(1:sNx,1:sNy,1,1) = (transpose(ptr)*sfac)+addo
      case ('lwrd')
        lwflux(1:sNx,1:sNy,1,1) = (transpose(ptr)*sfac)+addo
      case ('evap')
        evap(1:sNx,1:sNy,1,1) = (transpose(ptr)*sfac)+addo
      case ('prec')
        precip(1:sNx,1:sNy,1,1) = (transpose(ptr)*sfac)+addo
      case ('snow')
        snowprecip(1:sNx,1:sNy,1,1) = (transpose(ptr)*sfac)+addo
      case ('rnof')
        print*, "runoff not implemented yet !!!"
      case ('dswr')
        swdown(1:sNx,1:sNy,1,1) = (transpose(ptr)*sfac)+addo
      case ('dlwr')
        lwdown(1:sNx,1:sNy,1,1) = (transpose(ptr)*sfac)+addo
      case ('psfc')
        apressure(1:sNx,1:sNy,1,1) = (transpose(ptr)*sfac)+addo
      end select
!
!-----------------------------------------------------------------------
!     Debug: write field in ASCII format   
!-----------------------------------------------------------------------
!
      if (debugLevel == 4) then
        write(ofile,70) 'ocn_import', trim(itemNameList(i)),            &
                        iyear, imonth, iday, ihour, localPet, j
        iunit = localPet*10
        open(unit=iunit, file=trim(ofile)//'.txt')
        call print_matrix_r8(ptr, 1, sNx, 1, sNy, 1, 1,                 &
                             localPet, iunit, "PTR/OCN/IMP")
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
        write(ofile,80) 'ocn_import', trim(itemNameList(i)),            &
                        iyear, imonth, iday, ihour, localPet
        call ESMF_FieldWrite(field, trim(ofile)//'.nc', rc=rc)
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
 80   format(A10,'_',A,'_',I4,'-',I2.2,'-',I2.2,'_',I2.2,'_',I2.2)
!
      end subroutine OCN_Get
!
      subroutine OCN_Put(gcomp, rc)
!
!-----------------------------------------------------------------------
!     Used module declarations 
!-----------------------------------------------------------------------
!
      use mod_mit_gcm, only : theta
      use mod_mit_gcm, only : sNx, sNy 
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
      integer :: i, j, ii, jj, iunit, iyear, iday, imonth, ihour
      integer :: petCount, localPet, itemCount, localDECount
      character(ESMF_MAXSTR) :: cname, ofile
      character(ESMF_MAXSTR), allocatable :: itemNameList(:)
      real(ESMF_KIND_R8), pointer :: ptr(:,:)
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
      call ESMF_ClockGet(clock, currTime=currTime, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      call ESMF_TimeGet(currTime, yy=iyear, mm=imonth,                  &
                        dd=iday, h=ihour, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
      end if
!
!-----------------------------------------------------------------------
!     Get number of local DEs
!-----------------------------------------------------------------------
! 
      call ESMF_GridGet(models(Iocean)%grid,                            &
                        localDECount=localDECount, rc=rc)
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
      if (.not. allocated(itemTypeList)) then
        allocate(itemTypeList(itemCount))
      end if
      call ESMF_StateGet(exportState, itemNameList=itemNameList,        &
                         itemTypeList=itemTypeList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Loop over export fields 
!-----------------------------------------------------------------------
!
      do i = 1, itemCount
!
!-----------------------------------------------------------------------
!     Get export field 
!-----------------------------------------------------------------------
!
      call ESMF_StateGet(exportState, trim(itemNameList(i)),            &
                         field, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      do j = 0, localDECount-1
!
!-----------------------------------------------------------------------
!     Get pointer 
!-----------------------------------------------------------------------
!
      call ESMF_FieldGet(field, localDE=j, farrayPtr=ptr, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Set initial value to missing 
!-----------------------------------------------------------------------
!
      ptr = MISSING_R8
!
!-----------------------------------------------------------------------
!     Put data to export field 
!-----------------------------------------------------------------------
!
      select case (trim(adjustl(itemNameList(i))))
      case ('sst')
        ptr = transpose(theta(1:sNx,1:sNy,1,1,1))
      end select
!
!-----------------------------------------------------------------------
!     Debug: write field in ASCII format   
!-----------------------------------------------------------------------
!
      if (debugLevel == 4) then
        iunit = localPet
        write(ofile,90) 'ocn_export', trim(itemNameList(i)),            &
                        iyear, imonth, iday, ihour, localPet, j
        open(unit=iunit, file=trim(ofile)//'.txt') 
        call print_matrix_r8(ptr, 1, sNx, 1, sNy, 1, 1,     &
                             localPet, iunit, "PTR/OCN/EXP")
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
        write(ofile,100) 'ocn_export', trim(itemNameList(i)),           &
                        iyear, imonth, iday, ihour, localPet
        call ESMF_FieldWrite(field, trim(ofile)//'.nc', rc=rc)
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
 90   format(A10,'_',A,'_',I4,'-',I2.2,'-',I2.2,'_',I2.2,'_',I2.2,'_',I1)
 100  format(A10,'_',A,'_',I4,'-',I2.2,'-',I2.2,'_',I2.2,'_',I2.2)
!
      end subroutine OCN_Put
!
      end module mod_esmf_ocn

