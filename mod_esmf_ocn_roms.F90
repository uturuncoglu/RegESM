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
          NUOPC_SetServices          => routine_SetServices,            &
          NUOPC_Label_Advance        => label_Advance,                  &
          NUOPC_Label_DataInitialize => label_DataInitialize,           &
          NUOPC_Model_Type_IS        => type_InternalState,             &
          NUOPC_Model_Label_IS       => label_InternalState,            &
          NUOPC_Label_SetClock       => label_SetClock,                 &
          NUOPC_Label_CheckImport    => label_CheckImport
!
      use mod_types
      use mod_shared
!
      use ocean_control_mod, only :                                     &
          OCN_Initialize => ROMS_initialize,                            &
          OCN_Run        => ROMS_run,                                   &
          OCN_Finalize   => ROMS_finalize
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
      call ESMF_GridCompSetEntryPoint(gcomp,                            &
                                      methodflag=ESMF_METHOD_FINALIZE,  &
                                      userRoutine=OCN_SetFinalize,      &
                                      rc=rc)
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
      use mod_param, only : BOUNDS, Ngrids
      use ocean_coupler_mod, only : allocate_atm2ocn,                   &
                                    initialize_atm2ocn
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
      integer :: LBi, UBi, LBj, UBj
      integer :: ng, comm, localPet, petCount
      character(ESMF_MAXSTR) :: gname
!
      type(ESMF_VM) :: vm
      type(ESMF_Time) :: startTime, currTime 
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
!     Allocate exchange arrays 
!-----------------------------------------------------------------------
!
      do ng = 1, Ngrids
        LBi = BOUNDS(ng)%LBi(localPet)
        UBi = BOUNDS(ng)%UBi(localPet)
        LBj = BOUNDS(ng)%LBj(localPet)
        UBj = BOUNDS(ng)%UBj(localPet)
        call allocate_atm2ocn(ng, LBi, UBi, LBj, UBj)      
        call initialize_atm2ocn(ng, localPet)
      end do
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
      integer :: localPet, petCount
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
!     Put export fields (only for initial and restart run)
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
      integer :: ng, fac1, fac2, maxdiv
      integer :: ref_year,   str_year,   end_year
      integer :: ref_month,  str_month,  end_month
      integer :: ref_day,    str_day,    end_day
      integer :: ref_hour,   str_hour,   end_hour
      integer :: ref_minute, str_minute, end_minute
      integer :: ref_second, str_second, end_second
      integer :: localPet, petCount
      real*8 :: stime, etime, hour, minute, yday
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
      stime = minval(tdays)
      call caldate(r_date, stime, str_year, yday, str_month,            &
                   str_day, hour)
      minute = (hour-aint(hour))*60.0_r8
      str_hour = int(hour)
      str_minute = int(minute)
      str_second = int((minute-aint(minute))*60.0_r8)
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
      etime = 0.0_r8
      do ng = 1, Ngrids
        if (iic(ng) /= ntstart(ng)) then
          etime = max(etime, (real(ntimes(ng),r8)*dt(ng))*sec2day)
        else 
          etime = max(etime, tdays(ng)+(real(ntimes(ng),r8)*dt(ng))*sec2day)
        end if
      end do
      call caldate(r_date, etime, end_year, yday, end_month,            &
                   end_day, hour)
      minute = (hour-aint(hour))*60.0_r8
      end_hour = int(hour)
      end_minute = int(minute)
      end_second = int((minute-aint(minute))*60.0_r8)
      if (localPet == 0) then
      print*, etime, r_date, end_year, end_month, end_day, end_hour, end_minute, end_second 
      end if
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
!             msg='ESM and OCN stop times do not match: '//              &
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
!-----------------------------------------------------------------------
!     Check fields in the importState (slow time step) 
!-----------------------------------------------------------------------
!
      if (models(Iriver)%modActive) then
!
      call ESMF_StateGet(importState, itemName="rdis",                  &
                         field=field, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      atCorrectTime = NUOPC_FieldIsAtTime(field, startTime, rc=rc)
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
      integer :: i, j, ii, jj, ng, nr, tile, localDECount
      integer :: IstrR, IendR, JstrR, JendR
      integer :: IstrU, IendU, JstrU, JendU     
      integer :: IstrV, IendV, JstrV, JendV
      integer :: LBi, UBi, LBj, UBj
      integer :: staggerEdgeLWidth(2)
      integer :: staggerEdgeUWidth(2)
      integer, allocatable :: deBlockList(:,:,:)
      real(ESMF_KIND_R8), pointer :: ptrX(:,:), ptrY(:,:), ptrA(:,:)
      integer(ESMF_KIND_I4), pointer :: ptrM(:,:)
      character(ESMF_MAXSTR) :: cname, name, msgString
!
      type(ESMF_Array) :: arrX, arrY, arrM, arrA
      type(ESMF_StaggerLoc) :: staggerLoc
      type(ESMF_DistGrid) :: distGrid
      type(ESMF_VM) :: vm
!
      rc = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!     Check number of nested grids 
!-----------------------------------------------------------------------
!
      call ESMF_GridCompGet(gcomp, vm=vm, name=cname, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      if (Ngrids > 1) then
        write(msgString,'(A,I3)') trim(cname)//                         &
              ': number of nested grid is', Ngrids,                     &
              'coupled model only interacts with outermost one!'
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
      LBi = BOUNDS(ng)%LBi(localPet)
      UBi = BOUNDS(ng)%UBi(localPet)
      LBj = BOUNDS(ng)%LBj(localPet)
      UBj = BOUNDS(ng)%UBj(localPet)
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
!     Deallocate arrays    
!-----------------------------------------------------------------------
!
      if (allocated(deBlockList)) deallocate(deBlockList) 
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
!     Set mask value for land and ocean 
!-----------------------------------------------------------------------
!
      models(Iocean)%isLand = 0
      models(Iocean)%isOcean = 1
!
!-----------------------------------------------------------------------
!     Allocate items for grid area 
!-----------------------------------------------------------------------
!
      call ESMF_GridAddItem(models(Iocean)%grid,                        &
                            staggerLoc=staggerLoc,                      &
                            itemflag=ESMF_GRIDITEM_AREA,                &
                            rc=rc)
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
      call ESMF_GridGetItem (models(Iocean)%grid,                       &
                             localDE=j,                                 &
                             staggerLoc=staggerLoc,                     &
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
            ptrA(ii,jj) = GRID(ng)%om_p(ii,jj)*GRID(ng)%on_p(ii,jj)
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
            ptrA(ii,jj) = GRID(ng)%om_r(ii,jj)*GRID(ng)%on_r(ii,jj)
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
            ptrA(ii,jj) = GRID(ng)%om_u(ii,jj)*GRID(ng)%on_u(ii,jj)
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
            ptrA(ii,jj) = GRID(ng)%om_v(ii,jj)*GRID(ng)%on_v(ii,jj)
          end do
        end do
      end if
!
!-----------------------------------------------------------------------
!     Create temporary arrays.
!-----------------------------------------------------------------------
!
      if (models(Iocean)%mesh(i)%gtype == Icross) then
        arrX = ESMF_ArrayCreate(distGrid, ptrX,                         &
                                indexflag=ESMF_INDEX_DELOCAL, rc=rc) 
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
                             line=__LINE__, file=FILENAME)) return
!
        arrY = ESMF_ArrayCreate(distGrid, ptrY,                         &
                                indexflag=ESMF_INDEX_DELOCAL, rc=rc) 
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
                             line=__LINE__, file=FILENAME)) return
!
        arrM = ESMF_ArrayCreate(distGrid, ptrM,                         &
                                indexflag=ESMF_INDEX_DELOCAL, rc=rc) 
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
                             line=__LINE__, file=FILENAME)) return
!
        arrA = ESMF_ArrayCreate(distGrid, ptrA,                         &
                                indexflag=ESMF_INDEX_DELOCAL, rc=rc) 
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
                             line=__LINE__, file=FILENAME)) return
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
!
!-----------------------------------------------------------------------
!     Collect data from arrays to first PET of the component
!-----------------------------------------------------------------------
!
      if (models(Iriver)%modActive) then
!
      i = minloc(models(Iocean)%mesh(:)%gtype, dim=1,                   &
                 mask=(models(Iocean)%mesh(:)%gtype == Icross))
!
      if (localPet == 0) then
        allocate(models(Iocean)%mesh(i)%glon(Lm(ng),Mm(ng)))
        allocate(models(Iocean)%mesh(i)%glat(Lm(ng),Mm(ng)))
        allocate(models(Iocean)%mesh(i)%gmsk(Lm(ng),Mm(ng)))
        allocate(models(Iocean)%mesh(i)%gare(Lm(ng),Mm(ng)))
      else
        allocate(models(Iocean)%mesh(i)%glon(0,0))
        allocate(models(Iocean)%mesh(i)%glat(0,0))
        allocate(models(Iocean)%mesh(i)%gmsk(0,0))
        allocate(models(Iocean)%mesh(i)%gare(0,0))
      end if
!
      call ESMF_ArrayGather(arrX, farray=models(Iocean)%mesh(i)%glon,   &
                            rootPet=0, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      call ESMF_ArrayGather(arrY, farray=models(Iocean)%mesh(i)%glat,   &
                            rootPet=0, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      call ESMF_ArrayGather(arrM, farray=models(Iocean)%mesh(i)%gmsk,   &
                            rootPet=0, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      call ESMF_ArrayGather(arrA, farray=models(Iocean)%mesh(i)%gare,   &
                            rootPet=0, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Destroy temporary arrays 
!-----------------------------------------------------------------------
!
      call ESMF_ArrayDestroy(arrX, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      call ESMF_ArrayDestroy(arrY, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      call ESMF_ArrayDestroy(arrM, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      call ESMF_ArrayDestroy(arrA, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Find location of the rivers 
!-----------------------------------------------------------------------
!
      nr = size(rivers, dim=1)
      do i = 1, nr
        if (rivers(i)%isActive > 0) then
          if (.not. rivers(i)%asIndex) then
            call get_ij(vm, rivers(i)%lon, rivers(i)%lat,               &
                        rivers(i)%iindex, rivers(i)%jindex, rc)
          else
            call get_ll(vm, rivers(i)%iindex, rivers(i)%jindex,         &
                        rivers(i)%lon, rivers(i)%lat, rc)
          end if
          if (ESMF_LogFoundError(rcToCheck=rc,                          &
                                 msg=ESMF_LOGERR_PASSTHRU,              &
                                 line=__LINE__, file=FILENAME)) return
!
          rivers(i)%rootPet = findPet(vm, rivers(i)%iindex,             &
                                      rivers(i)%jindex, rc)
!
          if (localPet == 0) then
            write(*,20) i, rivers(i)%dir, rivers(i)%eRadius,            &
                        rivers(i)%lon, rivers(i)%lat,                   &
                        rivers(i)%iindex, rivers(i)%jindex,             &
                        rivers(i)%rootPet, 'ACTIVE'
          end if
        else
          if (.not. rivers(i)%asIndex) then
            rivers(i)%iindex = ZERO_I4
            rivers(i)%jindex = ZERO_I4
            rivers(i)%rootPet = ZERO_I4
          else
            rivers(i)%lon = ZERO_R8 
            rivers(i)%lat = ZERO_R8
            rivers(i)%rootPet = ZERO_I4
          end if 
!
          if (localPet == 0) then
            write(*,20) i, rivers(i)%dir, rivers(i)%eRadius,            &
                        rivers(i)%lon, rivers(i)%lat,                   &
                        rivers(i)%iindex, rivers(i)%jindex,             &
                        rivers(i)%rootPet, 'NOT ACTIVE!'
          end if
        end if
      end do
!
!-----------------------------------------------------------------------
!     Map ocean grid points to rivers defined by RTM component 
!-----------------------------------------------------------------------
!
      call map_rivers(vm, rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      end if
!
!-----------------------------------------------------------------------
!     Format definition 
!-----------------------------------------------------------------------
!
 20   format(" RIVER(",I2.2,") - ",I4,3F6.2," [",I3.3,":",I3.3,"] - ",I2," ",A)
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
      use mod_param, only : NtileI, NtileJ, BOUNDS, N, Lm, Mm, Ngrids
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
      integer :: i, j, k, ng, itemCount, localDECount,localPet, petCount
      integer :: IstrR, IendR, JstrR, JendR
      integer :: IstrU, IendU, JstrU, JendU     
      integer :: IstrV, IendV, JstrV, JendV
      integer :: staggerEdgeLWidth(2)
      integer :: staggerEdgeUWidth(2)
      integer :: TLW(2), TUW(2)
      character(ESMF_MAXSTR) :: cname, msgString
      character(ESMF_MAXSTR), allocatable :: itemNameList(:)
      real*8, dimension(:,:), pointer :: ptr2d
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
!     Check number of nested grids 
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
!     Store routehandle to exchage halo region data 
!-----------------------------------------------------------------------
!
      if (models(Iriver)%modActive) then
      call ESMF_FieldHaloStore(field,                                   &
              routehandle=models(Iocean)%importField(k)%rhandle, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
      end if
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
      if (trim(itemNameList(i)) == 'rdis') then
        ptr2d = ZERO_R8
      else
        ptr2d = MISSING_R8
      end if
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
      use mod_param, only : Ngrids
      use mod_scalars, only : dt, exit_flag, NoError
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
      real*8 :: trun
      integer :: localPet, petCount, phase
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
          write(*,50) trim(str1), trim(str2), phase, trun-minval(dt)
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
      call OCN_Run(trun-minval(dt))
!
!-----------------------------------------------------------------------
!     Check for error 
!-----------------------------------------------------------------------
!
      if (exit_flag /= NoError) then
        if (localPet == 0) then
          write(*,*) 'OCN component exit with flag = ', exit_flag
        end if
        call OCN_Finalize()
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      end if 
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
             ' [', F15.2, ']')
!
      end subroutine OCN_ModelAdvance
!
      subroutine OCN_SetFinalize(gcomp, importState, exportState,       &
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
      rc = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!     Call model finalize routines
!-----------------------------------------------------------------------
!
      call OCN_Finalize()
!
!-----------------------------------------------------------------------
!     Destroy ESMF objects 
!-----------------------------------------------------------------------
!
      call ESMF_ClockDestroy(clock, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      call ESMF_StateDestroy(importState, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      call ESMF_StateDestroy(exportState, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      end subroutine OCN_SetFinalize
!
      subroutine OCN_Get(gcomp, rc)
!
!-----------------------------------------------------------------------
!     Used module declarations 
!-----------------------------------------------------------------------
!
      use mod_param, only : BOUNDS, Ngrids
      use ocean_coupler_mod, only : rdata
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
      integer :: ng, i, j, ii, jj
      integer :: id, iyear, iday, imonth, ihour
      integer :: LBi, UBi, LBj, UBj, iunit
      integer :: IstrR, IendR, JstrR, JendR
      integer :: IstrU, IendU, JstrU, JendU     
      integer :: IstrV, IendV, JstrV, JendV
      integer :: localPet, petCount, itemCount, localDECount
      character(ESMF_MAXSTR) :: cname, msgString, ofile
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
      if (Ngrids > 1) then
        write(msgString,'(A,I3)') trim(cname)//                         &
              ': number of nested grid is', Ngrids,                     &
              'coupled model only interacts with outermost one!'
        call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_ERROR)
        ng = 1
      else
        ng = Ngrids
      end if
!
!-----------------------------------------------------------------------
!     Get limits of the arrays (based on PET id)
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
      LBi = BOUNDS(ng)%LBi(localPet)
      UBi = BOUNDS(ng)%UBi(localPet)
      LBj = BOUNDS(ng)%LBj(localPet)
      UBj = BOUNDS(ng)%UBj(localPet)
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
!     Perform halo region update 
!-----------------------------------------------------------------------
!
      if (models(Iriver)%modActive) then
      call ESMF_FieldHalo(field,                                        &
                routehandle=models(Iocean)%importField(id)%rhandle,     &
                checkflag=.true., rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
      end if
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
                  LBi, UBi, LBj, UBj
      end if 
!
!-----------------------------------------------------------------------
!     Put data to OCN component variable
!     Caution: wspd, snow and dswr is not mapped in ROMS side!
!-----------------------------------------------------------------------
!
      sfac = models(Iocean)%importField(id)%scale_factor
      addo = models(Iocean)%importField(id)%add_offset
!
      select case (trim(adjustl(itemNameList(i))))
      case ('psfc')
        do jj = LBj, UBj
          do ii = LBi, UBi
            rdata(ng)%Pair(ii,jj) = (ptr(ii,jj)*sfac)+addo
          end do
        end do
      case ('tsfc')
        do jj = LBj, UBj
          do ii = LBi, UBi
            rdata(ng)%Tair(ii,jj) = (ptr(ii,jj)*sfac)+addo
          end do
       end do
      case ('qsfc')
        do jj = LBj, UBj
          do ii = LBi, UBi
            rdata(ng)%Qair(ii,jj) = (ptr(ii,jj)*sfac)+addo
          end do
        end do
      case ('lwrd')
        do jj = LBj, UBj
          do ii = LBi, UBi
            rdata(ng)%Lwrad(ii,jj) = (ptr(ii,jj)*sfac)+addo
          end do
        end do
      case ('dlwr')
        do jj = LBj, UBj
          do ii = LBi, UBi
            rdata(ng)%Lwrad_down(ii,jj) = (ptr(ii,jj)*sfac)+addo
          end do
        end do
      case ('lhfx')
        do jj = LBj, UBj
          do ii = LBi, UBi
            rdata(ng)%Lhflx(ii,jj) = (ptr(ii,jj)*sfac)+addo
          end do
        end do
      case ('shfx')
        do jj = LBj, UBj
          do ii = LBi, UBi
            rdata(ng)%Shflx(ii,jj) = (ptr(ii,jj)*sfac)+addo
          end do
        end do
      case ('prec')
        do jj = LBj, UBj
          do ii = LBi, UBi
            rdata(ng)%rain(ii,jj) = (ptr(ii,jj)*sfac)+addo
          end do
        end do
      case ('wndu')
        do jj = LBj, UBj
          do ii = LBi, UBi
          rdata(ng)%Uwind(ii,jj) = (ptr(ii,jj)*sfac)+addo
          end do
        end do
      case ('wndv')
        do jj = LBj, UBj
          do ii = LBi, UBi 
          rdata(ng)%Vwind(ii,jj) = (ptr(ii,jj)*sfac)+addo
          end do
        end do
      case ('swrd')
        do jj = LBj, UBj
          do ii = LBi, UBi
            rdata(ng)%Swrad(ii,jj) = (ptr(ii,jj)*sfac)+addo
          end do
        end do
      case ('nflx')
        do jj = LBj, UBj
          do ii = LBi, UBi
            rdata(ng)%nhflx(ii,jj) = (ptr(ii,jj)*sfac)+addo
          end do
        end do
      case ('sflx')
        do jj = LBj, UBj
          do ii = LBi, UBi
            rdata(ng)%EminP(ii,jj) = (ptr(ii,jj)*sfac)+addo
          end do
        end do
      case ('taux')
        do jj = LBj, UBj
          do ii = LBi, UBi
            rdata(ng)%Ustr(ii,jj) = (ptr(ii,jj)*sfac)+addo
          end do
        end do
      case ('tauy')
        do jj = LBj, UBj
          do ii = LBi, UBi
            rdata(ng)%Vstr(ii,jj) = (ptr(ii,jj)*sfac)+addo
          end do
        end do
      case ('rdis')
        call put_river(vm, clock, LBi, UBi, LBj, UBj,                   &
                       ptr, sfac, addo, rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
                               line=__LINE__, file=FILENAME)) return  
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
        call print_matrix(ptr, LBi, UBi, LBj, UBj, 1, 1,                &
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
      use mod_param, only : BOUNDS, N, Ngrids
      use mod_ocean, only : OCEAN
      use mod_grid , only : GRID
      use mod_scalars, only : itemp
      use mod_stepping, only : nstp
#ifdef OCNICE
      use mod_ice, only : ICE
      use mod_stepping, only : linew
#endif
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
      integer :: ng, i, j, ii, jj, iunit, iyear, iday, imonth, ihour
      integer :: petCount, localPet, itemCount, localDECount
      integer :: IstrR, IendR, JstrR, JendR
      integer :: LBi, UBi, LBj, UBj
      character(ESMF_MAXSTR) :: cname, msgString, ofile
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
      if (Ngrids > 1) then
        write(msgString,'(A,I3)') trim(cname)//                         &
              ': number of nested grid is', Ngrids,                     &
              'coupled model only interacts with outermost one!'
        call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_ERROR)
        ng = 1
      else
        ng = Ngrids
      end if
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
!     Get limits of the arrays (based on PET id)
!-----------------------------------------------------------------------
!
      IstrR=BOUNDS(ng)%IstrR(localPet)
      IendR=BOUNDS(ng)%IendR(localPet)
      JstrR=BOUNDS(ng)%JstrR(localPet)
      JendR=BOUNDS(ng)%JendR(localPet)
!
      LBi = BOUNDS(ng)%LBi(localPet)
      UBi = BOUNDS(ng)%UBi(localPet)
      LBj = BOUNDS(ng)%LBj(localPet)
      UBj = BOUNDS(ng)%UBj(localPet)
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
        do jj = JstrR, JendR
          do ii= IstrR, IendR
            ptr(ii,jj) = OCEAN(ng)%t(ii,jj,N(ng),nstp(ng),itemp)
          end do
        end do
#ifdef OCNICE
      case ('sit')
        do jj = JstrR, JendR
          do ii = IstrR, IendR
            ptr(ii,jj) = ICE(ng)%ai(ii,jj,linew(ng))
          end do
        end do
#endif
#ifdef OCNWETDRY
      case ('msk')
        do jj = JstrR, JendR
          do ii = IstrR, IendR
            ! update only sea grids
            if (GRID(ng)%rmask(ii,jj) > 0.0d0) then
              ! if wet-dry mask differs from static mask
              if (GRID(ng)%rmask(ii,jj) /= GRID(ng)%rmask_wet(ii,jj)) then
                ptr(ii,jj) = GRID(ng)%rmask_wet(ii,jj)
              else
                ptr(ii,jj) = GRID(ng)%rmask(ii,jj)
              end if
            end if
          end do
        end do
#endif
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
        call print_matrix(ptr, IstrR, IendR, JstrR, JendR, 1, 1,        &
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
      subroutine put_river(vm, clock, LBi, UBi, LBj, UBj,               &
                           ptr, sfac, addo, rc)
!
!-----------------------------------------------------------------------
!     Used module declarations 
!-----------------------------------------------------------------------
!
      use mod_grid, only : GRID
      use mod_forces, only : FORCES
      use mod_param, only : BOUNDS, Ngrids
      use ocean_coupler_mod, only : rdata
!
      implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
      type(ESMF_VM), intent(in) :: vm
      type(ESMF_Clock), intent(in) :: clock
      integer, intent(in) :: LBi, UBi, LBj, UBj
      real(ESMF_KIND_R8), intent(in) :: ptr(LBi:UBi,LBj:UBj)
      real(ESMF_KIND_R8), intent(in) :: sfac, addo
      integer, intent(inout) :: rc
!
!-----------------------------------------------------------------------
!     Local variable declarations 
!-----------------------------------------------------------------------
!
      real*8 :: rdis(1)
      integer :: i, j, k, r, ii, jj, mm, ng, np, nr, localPet, petCount 
      character(ESMF_MAXSTR) :: str
!
      type(ESMF_Time) :: currTime
! 
      rc = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!     Query VM
!-----------------------------------------------------------------------
!
      call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Query clock and time 
!-----------------------------------------------------------------------
!
      call ESMF_ClockGet(clock, currTime=currTime, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      call ESMF_TimeGet(currTime, mm=mm, timeStringISOFrac=str, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!     
!-----------------------------------------------------------------------
!     Fill array with river discharge data
!-----------------------------------------------------------------------
!
      nr = size(rivers, dim=1)
!
      do ng = 1, Ngrids
        k = 0
        do r = 1, nr
          if (rivers(r)%isActive > 0) then
            ! get river discharge
            if (localPet == rivers(r)%rootPet) then
              rdis = ptr(rivers(r)%iindex,rivers(r)%jindex)
              if (rdis(1) < TOL_R8) then
                rdis = (rdis*sfac)+addo
              end if
            end if
!
            ! broadcast data across the PETs
            call ESMF_VMBroadcast(vm, bcstData=rdis, count=1,           &
                                  rootPet=rivers(r)%rootPet, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc,                        &
                                   msg=ESMF_LOGERR_PASSTHRU,            &
                                   line=__LINE__, file=FILENAME)) return
!
            ! apply monthly correction factor
            rdis = rdis*rivers(r)%monfac(mm)
!
            ! if river data is provided as monthly values (m^3/s), then
            ! overwrite river discharge data
            if (rivers(r)%isActive == 2) then
              rdis = rivers(r)%monfac(mm)
            end if
!
            ! apply as point source
            if (riverOpt == 1) then
              ! set direction
              if (rivers(r)%dir < 0) then
                rdis = -rdis
              end if               
              ! fill data arrays
              np = rivers(r)%npoints
              do j = 1, np
                k = k+1
                rdata(ng)%R1dis(k) = (rdis(1)*sfac)+addo
              end do 
!
            ! apply as surface boundary condition
            else if (riverOpt == 2) then
              ! distribute data to the mapped ocean grid points
              np = rivers(r)%mapSize 
              do k = 1, np
                i = int(rivers(r)%mapTable(1,k))
                j = int(rivers(r)%mapTable(2,k))                
!
                do jj = LBj, UBj
                  do ii = LBi, UBi
                    if (ii == i .and. jj ==j .and. rdis(1) < TOL_R8) then
                      rdata(ng)%R2dis(ii,jj) = rdata(ng)%R2dis(ii,jj)+  &
                                             (rdis(1)/rivers(r)%mapArea)
                    end if 
                  end do
                end do  
              end do
            end if
!
            ! print debug info
            if (localPet == 0) then
              if (rdis(1) < TOL_R8) then
                write(*,110) r, trim(str), rdis(1)
              else
                write(*,110) r, trim(str), ZERO_R8
              end if 
            end if
          else
            ! print debug info
            if (localPet == 0) then
              write(*,110) r, trim(str), ZERO_R8
            end if
          end if
        end do
      end do
!
!-----------------------------------------------------------------------
!     Formats 
!-----------------------------------------------------------------------
!
 110  format(' River (',I2.2,') Discharge [',A,'] : ',F15.6)
!
      end subroutine put_river
!
      function findPet(vm, i, j, rc)
!
!-----------------------------------------------------------------------
!     Used module declarations 
!-----------------------------------------------------------------------
!
      use mod_param, only : BOUNDS, Ngrids
!
      implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!      
      integer :: findPet
!
      type(ESMF_VM), intent(in) :: vm
      integer, intent(in) :: i, j
      integer, intent(inout) :: rc
!
!-----------------------------------------------------------------------
!     Local variable declarations 
!-----------------------------------------------------------------------
!
      integer :: ii, jj, k, ng, petCount, localPet, rootPet, sendData(1)
      integer :: LBi, UBi, LBj, UBj
!
      rc = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!     Query VM 
!-----------------------------------------------------------------------
!
      call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Find rootPET that has river discharge data 
!-----------------------------------------------------------------------
!
      do ng = 1, Ngrids 
        do k = 0, petCount-1
          LBi = BOUNDS(ng)%LBi(k)
          UBi = BOUNDS(ng)%UBi(k)
          LBj = BOUNDS(ng)%LBj(k)
          UBj = BOUNDS(ng)%UBj(k)
!
          do jj = LBj, UBj
            do ii = LBi, UBi
              if (ii == i .and. jj == j) then
                rootPet = k
                exit
              end if
            end do
          end do
        end do
      end do
!
!-----------------------------------------------------------------------
!     Broadcast rootPET data to PETs 
!-----------------------------------------------------------------------
!
      sendData(1) = rootPet
      call ESMF_VMBroadcast(vm, bcstData=sendData, count=1,             &
                            rootPet=rootPet, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
      findPet = sendData(1)
!
      end function findPet
!
      end module mod_esmf_ocn
