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
#define FILENAME "mod_esmf_atm.F90"
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
      use NUOPC_Model,                                                  &
          NUOPC_SetServices          => SetServices,                    &
          NUOPC_Label_Advance        => label_Advance,                  &
          NUOPC_Label_DataInitialize => label_DataInitialize,           &
          NUOPC_Label_SetClock       => label_SetClock,                 &
          NUOPC_Label_CheckImport    => label_CheckImport
!
      use mod_types
      use mod_shared
!
      use mod_regcm_interface, only :                                   &
          ATM_Initialize => RCM_initialize,                             &
          ATM_Run        => RCM_run,                                    &
          ATM_Finalize   => RCM_finalize
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
      call NUOPC_CompSpecialize(gcomp,                                  &
                                specLabel=NUOPC_Label_CheckImport,      &
                                specPhaseLabel="RunPhase1",             &
                                specRoutine=ATM_CheckImport, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
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
      use mod_update, only : ATM_Allocate => RCM_Allocate
      use mod_runparams, only : dtsrf
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
      call ATM_Initialize(mpiCommunicator=comm)
      call ATM_Allocate()
!
!-----------------------------------------------------------------------
!     Set-up grid and load coordinate data 
!-----------------------------------------------------------------------
!
      call ATM_SetGridArrays2d(gcomp, localPet, rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      if (models(Icopro)%modActive) then
      call ATM_SetGridArrays3d(gcomp, localPet, rc) 
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
      if (models(Icopro)%modActive) then
      call ATM_SetStates3d(gcomp, rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
      end if
!
      end subroutine ATM_SetInitializeP2
!
      subroutine ATM_DataInit(gcomp, rc)
!
!-----------------------------------------------------------------------
!     Used module declarations 
!-----------------------------------------------------------------------
!
      use mod_runparams, only : dtsrf
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
      call ESMF_GridCompGet(gcomp, vm=vm, clock=clock,                  &
                            currentPhase=phase, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      call ESMF_ClockGet(clock, currTime=currTime,                      &
                         timeStep=timeStep, rc=rc)
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
          write(*,10) trim(str1), trim(str2), phase
        else
          write(*,20) trim(str1), trim(str2), phase, 0.0d0, dtsrf
        end if
      end if
!
!-----------------------------------------------------------------------
!     Run ATM component (run only one time step to fill variables)
!-----------------------------------------------------------------------
!
      call ATM_Run(0.0d0, dtsrf)
!
!-----------------------------------------------------------------------
!     Put export fields
!-----------------------------------------------------------------------
!
      call ATM_Put(gcomp, rc=rc)
!
      end if
!
!-----------------------------------------------------------------------
!     Format definition 
!-----------------------------------------------------------------------
!
 10   format(' Running ATM Component: ',A,' --> ',A,' Phase: ',I1,' +')
 20   format(' Running ATM Component: ',A,' --> ',A,' Phase: ',I1,      &
             ' [',F15.2, '-', F15.2, '] +')
!
      end subroutine ATM_DataInit
!
      subroutine ATM_SetClock(gcomp, rc)
!
!-----------------------------------------------------------------------
!     Used module declarations 
!-----------------------------------------------------------------------
!
      use mod_dynparam , only : calendar
      use mod_runparams, only : idate0, idate1, idate2, dtsec
      use mod_date, only : split_idate
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
      integer :: fac1, fac2, maxdiv
      integer :: ref_year,   str_year,   end_year
      integer :: ref_month,  str_month,  end_month
      integer :: ref_day,    str_day,    end_day
      integer :: ref_hour,   str_hour,   end_hour
      integer :: ref_minute, str_minute, end_minute
      integer :: ref_second, str_second, end_second
      real*8 :: tstr
!
      type(ESMF_Clock) :: cmpClock 
      type(ESMF_TimeInterval) :: timeStep
      type(ESMF_Time) :: currTime, startTime
      type(ESMF_Time) :: cmpRefTime, cmpStartTime, cmpStopTime
      type(ESMF_Calendar) :: cal
!
      rc = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!     Create gridded component clock 
!-----------------------------------------------------------------------
!
      if (calendar == 'gregorian') then
        cal = ESMF_CalendarCreate(ESMF_CALKIND_GREGORIAN,               &
                                  name=trim(calendar),                  &
                                  rc=rc)
      else if (calendar == 'noleap' .or. calendar == '365_day') then
        cal = ESMF_CalendarCreate(ESMF_CALKIND_NOLEAP,                  &
                                  name=trim(calendar),                  &
                                  rc=rc)
      else if (calendar == '360_day') then
        cal = ESMF_CalendarCreate(ESMF_CALKIND_360DAY,                  &
                                  name=trim(calendar),                  &
                                  rc=rc)
      else
        cal = ESMF_CalendarCreate(ESMF_CALKIND_GREGORIAN,               &
                                  name=trim(calendar),                  &
                                  rc=rc)
      end if
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Set reference time
!-----------------------------------------------------------------------
!
      call split_idate(idate0, ref_year, ref_month, ref_day, ref_hour)
      ref_minute = 0
      ref_second = 0
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
      call split_idate(idate1, str_year, str_month, str_day, str_hour)
      str_minute = 0
      str_second = 0
!
      call ESMF_TimeSet (cmpStartTime,                                  &
                         yy=str_year,                                   &
                         mm=str_month,                                  &
                         dd=str_day,                                    &
                         h=str_hour,                                    &
                         m=str_minute,                                  &
                         s=str_second,                                  &
                         calendar=cal,                                  &
                         rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Set stop time
!-----------------------------------------------------------------------
!
      call split_idate(idate2, end_year, end_month, end_day, end_hour)
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
             msg='ESM and ATM start times do not match: '//             &
             'please check the config files')
        return
      end if
!
      if (cmpStopTime /= esmStopTime) then
        call ESMF_TimePrint(cmpStopTime, options="string", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
                               line=__LINE__, file=FILENAME)) return
!
        call ESMF_TimePrint(esmStopTime, options="string", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
                               line=__LINE__, file=FILENAME)) return
!
        call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc,              &
             msg='ESM and ATM stop times do not match: '//              &
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
             msg='ESM and ATM calendars do not match: '//               &
             'please check the config files')
        return
      end if
!
!-----------------------------------------------------------------------
!     Modify component clock time step 
!-----------------------------------------------------------------------
!
      fac1 = maxval(connectors(Iatmos,:)%divDT,                         &
                    mask=models(:)%modActive)
      fac2 = maxval(connectors(:,Iatmos)%divDT,                         &
                    mask=models(:)%modActive)
      maxdiv = max(fac1, fac2)
!
      call ESMF_ClockSet(cmpClock, name='atm_clock',                    &
                         timeStep=timeStep/maxdiv, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      end subroutine ATM_SetClock
!
      subroutine ATM_CheckImport(gcomp, rc)
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
      integer :: itemCount, localPet, ddt
      logical :: atCorrectTime
      character(ESMF_MAXSTR), allocatable :: itemNameList(:)
!
      type(ESMF_VM) :: vm
      type(ESMF_Time)  :: startTime, currTime
      type(ESMF_TimeInterval) :: timeStep
      type(ESMF_Clock) :: driverClock
      type(ESMF_Field) :: field
      type(ESMF_State) :: importState
!
      rc = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!     Query component for the driverClock
!-----------------------------------------------------------------------
!
      call NUOPC_ModelGet(gcomp, driverClock=driverClock, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      call ESMF_GridCompGet(gcomp, vm=vm, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      call ESMF_VMGet(vm, localPet=localPet, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Get the start time and current time out of the clock
!-----------------------------------------------------------------------
!
      call ESMF_ClockGet(driverClock, startTime=startTime,              &
                         currTime=currTime, timeStep=timeStep, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Query component for its clock and importState
!-----------------------------------------------------------------------
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
!
      call ESMF_StateGet(importState, itemNameList=itemNameList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Check fields in the importState
!-----------------------------------------------------------------------
!
      if (itemCount > 0) then
      call ESMF_StateGet(importState, itemName=trim(itemNameList(1)),   &
                         field=field, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
!      ddt = maxval(connectors(Iatmos,:)%divDT, mask=models(:)%modActive)
!
      if (cplType == 1) then
        atCorrectTime = NUOPC_IsAtTime(field, currTime, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
            line=__LINE__, file=FILENAME)) return
!
        call print_timestamp(field, currTime, localPet, "ATM", rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
            line=__LINE__, file=FILENAME)) return
      else
        atCorrectTime = NUOPC_IsAtTime(field, currTime+timeStep, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
            line=__LINE__, file=FILENAME)) return
!
        call print_timestamp(field, currTime+timeStep,                  &
                             localPet, "ATM", rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
            line=__LINE__, file=FILENAME)) return
      end if
!
!      if (.not. atCorrectTime) then
!        call ESMF_LogSetError(ESMF_RC_ARG_BAD,                          &
!                              msg="NUOPC INCOMPATIBILITY DETECTED: "//  &
!                              "Import Fields not at correct time",      &
!                              line=__LINE__, file=FILENAME,             &
!                              rcToReturn=rc)
!        return
!      end if
      end if
!
      end subroutine ATM_CheckImport
!
      subroutine ATM_SetGridArrays2d(gcomp, localPet, rc)
!
!-----------------------------------------------------------------------
!     Used module declarations 
!-----------------------------------------------------------------------
!
      use mod_mppparam, only : ma
      use mod_runparams, only : dxsq
      use mod_atm_interface, only : mddom
      use mod_dynparam, only : iy, jx, nproc, ide1, ide2, jde1, jde2,   &
                               idi1, idi2, jdi1, jdi2,                  &
                               ice1, ice2, jce1, jce2
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
      integer :: i, j, ii, jj, i0, j0, localDECount
      integer :: cpus_per_dim(2)
!
      type(ESMF_DistGrid) :: distGrid
      type(ESMF_StaggerLoc) :: staggerLoc
      real(ESMF_KIND_R8), pointer :: ptrX(:,:), ptrY(:,:), ptrA(:,:)
      integer, pointer :: ptrM(:,:)
      character (len=40) :: name
!
      rc = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!     Calculate number of CPUs in each direction 
!-----------------------------------------------------------------------
!
      if ( nproc < 4 ) then
        cpus_per_dim(2) = 1
        cpus_per_dim(1) = nproc
      else if ( nproc >= 4 ) then
        cpus_per_dim(2) = (nint(sqrt(dble(nproc)))/2)*2
        if ( iy > int(1.5*dble(jx)) ) then
          cpus_per_dim(2) = cpus_per_dim(2) - 1
          do while ( mod(nproc,cpus_per_dim(2)) /= 0 )
            cpus_per_dim(2) = cpus_per_dim(2) - 1
          end do
        else if ( jx > int(1.5*dble(iy)) ) then
          cpus_per_dim(2) = cpus_per_dim(2) + 1
          do while ( mod(nproc,cpus_per_dim(2)) /= 0 )
            cpus_per_dim(2) = cpus_per_dim(2) + 1
          end do
        else
          do while ( mod(nproc,cpus_per_dim(2)) /= 0 )
            cpus_per_dim(2) = cpus_per_dim(2) + 1
          end do
        end if
        cpus_per_dim(1) = nproc/cpus_per_dim(2)
      end if
!
!-----------------------------------------------------------------------
!     Create DistGrid based on model domain decomposition
!
!     ESMF is basically using a right handed coordinate system, and 
!     using the Fortran way of using the smallest stride to the first 
!     dimension but RegCM not. The order of dimension is reversed
!     because of this limitation. 
!-----------------------------------------------------------------------
!
      distGrid = ESMF_DistGridCreate(minIndex=(/ 1, 1 /),               &
                                     maxIndex=(/ iy, jx /),             &
                                     regDecomp=cpus_per_dim,            &
                                     rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Define type of stenciles used by grid (dot and cross points)
!-----------------------------------------------------------------------
!
      if (.not. allocated(models(Iatmos)%mesh)) then
        allocate(models(Iatmos)%mesh(2))
        models(Iatmos)%mesh(1)%gtype = Icross
        models(Iatmos)%mesh(2)%gtype = Idot
      end if
!
      do i = 1, 2 
!
!-----------------------------------------------------------------------
!     Set staggering type 
!-----------------------------------------------------------------------
!
      if (models(Iatmos)%mesh(i)%gtype == Icross) then
        staggerLoc = ESMF_STAGGERLOC_CENTER
      else if (models(Iatmos)%mesh(i)%gtype == Idot) then
        staggerLoc = ESMF_STAGGERLOC_CORNER
      end if
!
!-----------------------------------------------------------------------
!     Create grid (2d)
!-----------------------------------------------------------------------
!
      if (i == 1) then
      models(Iatmos)%grid = ESMF_GridCreate(distgrid=distGrid,          &
                                            indexflag=ESMF_INDEX_GLOBAL,&
                                            name="atm_grid2d",          &
                                            rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
      end if
!
!-----------------------------------------------------------------------
!     Allocate coordinates 
!-----------------------------------------------------------------------
!
      call ESMF_GridAddCoord(models(Iatmos)%grid,                       &
                             staggerLoc=staggerLoc,                     &
                             rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Allocate items for masking
!-----------------------------------------------------------------------
!
      call ESMF_GridAddItem(models(Iatmos)%grid,                        &
                            staggerLoc=staggerLoc,                      &
                            itemflag=ESMF_GRIDITEM_MASK,                &
                            rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Set mask value for land and ocean 
!-----------------------------------------------------------------------
!
      models(Iatmos)%isLand = 2
      models(Iatmos)%isOcean = 0
!
!-----------------------------------------------------------------------
!     Allocate items for grid area 
!-----------------------------------------------------------------------
!
      call ESMF_GridAddItem(models(Iatmos)%grid,                        &
                            staggerLoc=staggerLoc,                      &
                            itemflag=ESMF_GRIDITEM_AREA,                &
                            rc=rc)
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
!     Get pointers and set coordinates for the grid 
!-----------------------------------------------------------------------
! 
      do j = 0, localDECount-1
      call ESMF_GridGetCoord(models(Iatmos)%grid,                       &
                             localDE=j,                                 &
                             staggerLoc=staggerLoc,                     &
                             coordDim=1,                                &
                             farrayPtr=ptrX,                            &
                             rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      call ESMF_GridGetCoord(models(Iatmos)%grid,                       &
                             localDE=j,                                 &
                             staggerLoc=staggerLoc,                     &
                             coordDim=2,                                &
                             farrayPtr=ptrY,                            &
                             rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      call ESMF_GridGetItem (models(Iatmos)%grid,                       &
                             localDE=j,                                 &
                             staggerLoc=staggerLoc,                     &
                             itemflag=ESMF_GRIDITEM_MASK,               &
                             farrayPtr=ptrM,                            &
                             rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      call ESMF_GridGetItem (models(Iatmos)%grid,                       &
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
      name = GRIDDES(models(Iatmos)%mesh(i)%gtype)
!
      if (debugLevel > 0) then
        write(*,30) localPet, j, adjustl("PTR/ATM/GRD/"//name),         &
                    lbound(ptrX, dim=1), ubound(ptrX, dim=1),           &
                    lbound(ptrX, dim=2), ubound(ptrX, dim=2),           &
                    ma%has_bdybottom, ma%has_bdyright,                  &
                    ma%has_bdytop, ma%has_bdyleft
      end if
!
!-----------------------------------------------------------------------
!     Fill the pointers    
!-----------------------------------------------------------------------
!
      if (models(Iatmos)%mesh(i)%gtype == Idot) then
        if (debugLevel > 0) then
          write(*,30) localPet, j, adjustl("DAT/ATM/GRD/"//name),       &
                      ide1, ide2, jde1, jde2,                           &
                      ma%has_bdybottom, ma%has_bdyright,                &
                      ma%has_bdytop, ma%has_bdyleft
        end if
!
        do i0 = ide1, ide2
          do j0 = jde1, jde2
            ptrX(i0,j0) = mddom%dlon(j0,i0)
            ptrY(i0,j0) = mddom%dlat(j0,i0)
          end do
        end do
!
        if (ma%has_bdyright) then
           ptrX(:,jde2+1) = ptrX(:,jde2)+(ptrX(:,jde2)-ptrX(:,jde2-1))
           ptrY(:,jde2+1) = ptrY(:,jde2)+(ptrY(:,jde2)-ptrY(:,jde2-1))
        end if
!
        if (ma%has_bdytop) then
          ptrX(ide2+1,:) = ptrX(ide2,:)+(ptrX(ide2,:)-ptrX(ide2-1,:))
          ptrY(ide2+1,:) = ptrY(ide2,:)+(ptrY(ide2,:)-ptrY(ide2-1,:))
        end if
!
        ptrA = dxsq
      else if (models(Iatmos)%mesh(i)%gtype == Icross) then
        if (debugLevel > 0) then
          write(*,30) localPet, j, adjustl("DAT/ATM/GRD/"//name),       &
                      ice1, ice2, jce1, jce2,                           &
                      ma%has_bdybottom, ma%has_bdyright,                &
                      ma%has_bdytop, ma%has_bdyleft
        end if
!
        do i0 = ice1, ice2
          do j0 = jce1, jce2
            ptrX(i0,j0) = mddom%xlon(j0,i0)
            ptrY(i0,j0) = mddom%xlat(j0,i0)
            ptrM(i0,j0) = int(mddom%mask(j0,i0))
          end do
        end do
!
        ptrA = dxsq
!
        if (ma%has_bdyright) then
           ptrX(:,jce2+1) = ptrX(:,jce2)+(ptrX(:,jce2)-ptrX(:,jce2-1))
           ptrY(:,jce2+1) = ptrY(:,jce2)+(ptrY(:,jce2)-ptrY(:,jce2-1))
        end if
!
        if (ma%has_bdytop) then
          ptrX(ice2+1,:) = ptrX(ice2,:)+(ptrX(ice2,:)-ptrX(ice2-1,:))
          ptrY(ice2+1,:) = ptrY(ice2,:)+(ptrY(ice2,:)-ptrY(ice2-1,:))
        end if
      end if
!
!-----------------------------------------------------------------------
!     Nullify pointer to make sure that it does not point on a random 
!     part in the memory 
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
      call ESMF_GridCompSet(gcomp, grid=models(Iatmos)%grid, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
      if (models(Icopro)%modActive) then    
      end if

!
!-----------------------------------------------------------------------
!     Debug: write out component grid in VTK format 
!-----------------------------------------------------------------------
!
      if (debugLevel > 1) then
      call ESMF_GridWriteVTK(models(Iatmos)%grid,                       &
                         filename="atmos_"//                            &
                         trim(GRIDDES(models(Iatmos)%mesh(i)%gtype))//  &
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
 30   format(" PET(",I3.3,") - DE(",I2.2,") - ",A20," : ",              &
             4I8," ",L," ",L," ",L," ",L)
!
      end subroutine ATM_SetGridArrays2d
!
      subroutine ATM_SetGridArrays3d(gcomp, localPet, rc)
!
!-----------------------------------------------------------------------
!     Used module declarations 
!-----------------------------------------------------------------------
!
      use mod_mppparam, only : ma
      use mod_atm_interface, only : mddom
      use mod_dynparam, only : iy, jx, nproc
      use mod_dynparam, only : ide1, ide2, jde1, jde2
      use mod_dynparam, only : ice1, ice2, jce1, jce2
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
      integer :: i, j, ii, jj, i0, j0, k0, kz, localDECount
      integer :: cpus_per_dim(3)
!
      type(ESMF_DistGrid) :: distGrid
      type(ESMF_StaggerLoc) :: staggerLoc
      real(ESMF_KIND_R8), pointer :: ptrX(:,:,:)
      real(ESMF_KIND_R8), pointer :: ptrY(:,:,:)
      real(ESMF_KIND_R8), pointer :: ptrZ(:,:,:)
      character (len=40) :: name
!
      rc = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!     Calculate number of CPUs in each direction 
!-----------------------------------------------------------------------
!
      if ( nproc < 4 ) then
        cpus_per_dim(2) = 1
        cpus_per_dim(1) = nproc
      else if ( nproc >= 4 ) then
        cpus_per_dim(2) = (nint(sqrt(dble(nproc)))/2)*2
        if ( iy > int(1.5*dble(jx)) ) then
          cpus_per_dim(2) = cpus_per_dim(2) - 1
          do while ( mod(nproc,cpus_per_dim(2)) /= 0 )
            cpus_per_dim(2) = cpus_per_dim(2) - 1
          end do
        else if ( jx > int(1.5*dble(iy)) ) then
          cpus_per_dim(2) = cpus_per_dim(2) + 1
          do while ( mod(nproc,cpus_per_dim(2)) /= 0 )
            cpus_per_dim(2) = cpus_per_dim(2) + 1
          end do
        else
          do while ( mod(nproc,cpus_per_dim(2)) /= 0 )
            cpus_per_dim(2) = cpus_per_dim(2) + 1
          end do
        end if
        cpus_per_dim(1) = nproc/cpus_per_dim(2)
      end if
!
!-----------------------------------------------------------------------
!     Create DistGrid based on model domain decomposition
!
!     ESMF is basically using a right handed coordinate system, and 
!     using the Fortran way of using the smallest stride to the first 
!     dimension but RegCM not. The order of dimension is reversed
!     because of this limitation. 
!-----------------------------------------------------------------------
!
      kz = models(Iatmos)%nLevs
      cpus_per_dim(3) = 1
!
      distGrid = ESMF_DistGridCreate(minIndex=(/ 1, 1, 1 /),            &
                                     maxIndex=(/ iy, jx, kz /),         &
                                     regDecomp=cpus_per_dim,            &
                                     rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Define component grid (dot and cross points)
!-----------------------------------------------------------------------
!
      do i = 1, 2 
!
!-----------------------------------------------------------------------
!     Set staggering type 
!-----------------------------------------------------------------------
!
      if (models(Iatmos)%mesh(i)%gtype == Icross) then
        staggerLoc = ESMF_STAGGERLOC_CENTER
      else if (models(Iatmos)%mesh(i)%gtype == Idot) then
        staggerLoc = ESMF_STAGGERLOC_CORNER
      end if
!
!-----------------------------------------------------------------------
!     Create ESMF Grid
!-----------------------------------------------------------------------
!
      if (i == 1) then
      models(Iatmos)%grid3d = ESMF_GridCreate(distgrid=distGrid,        &
                                            indexflag=ESMF_INDEX_GLOBAL,&
                                            name="atm_grid3d",          &
                                            rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
      end if
!
!-----------------------------------------------------------------------
!     Allocate coordinates 
!-----------------------------------------------------------------------
!
      call ESMF_GridAddCoord(models(Iatmos)%grid3d,                     &
                             staggerLoc=staggerLoc,                     &
                             rc=rc)
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
!     Get pointers and set coordinates for the grid 
!-----------------------------------------------------------------------
! 
      do j = 0, localDECount-1
      call ESMF_GridGetCoord(models(Iatmos)%grid3d,                     &
                             localDE=j,                                 &
                             staggerLoc=staggerLoc,                     &
                             coordDim=1,                                &
                             farrayPtr=ptrX,                            &
                             rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      call ESMF_GridGetCoord(models(Iatmos)%grid3d,                     &
                             localDE=j,                                 &
                             staggerLoc=staggerLoc,                     &
                             coordDim=2,                                &
                             farrayPtr=ptrY,                            &
                             rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      call ESMF_GridGetCoord(models(Iatmos)%grid3d,                     &
                             localDE=j,                                 &
                             staggerLoc=staggerLoc,                     &
                             coordDim=3,                                &
                             farrayPtr=ptrZ,                            &
                             rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Debug: write size of pointers    
!-----------------------------------------------------------------------
!
      name = GRIDDES(models(Iatmos)%mesh(i)%gtype)
!
      if (debugLevel > 0) then
        write(*,110) localPet, j, adjustl("PTR/ATM/GRD3D/"//name),      &
                    lbound(ptrX, dim=1), ubound(ptrX, dim=1),           &
                    lbound(ptrX, dim=2), ubound(ptrX, dim=2),           &
                    lbound(ptrX, dim=3), ubound(ptrX, dim=3),           &
                    ma%has_bdybottom, ma%has_bdyright,                  &
                    ma%has_bdytop, ma%has_bdyleft
      end if
!
!-----------------------------------------------------------------------
!     Fill the pointers    
!-----------------------------------------------------------------------
!
      if (models(Iatmos)%mesh(i)%gtype == Idot) then
        if (debugLevel > 0) then
          write(*,110) localPet, j, adjustl("DAT/ATM/GRD3D/"//name),    &
                      ide1, ide2, jde1, jde2, 1, kz,                    &
                      ma%has_bdybottom, ma%has_bdyright,                &
                      ma%has_bdytop, ma%has_bdyleft
        end if
!
        do k0 = 1 , kz
          do i0 = ide1, ide2
            do j0 = jde1, jde2
              ptrX(i0,j0,k0) = mddom%dlon(j0,i0)
              ptrY(i0,j0,k0) = mddom%dlat(j0,i0)
              ptrZ(i0,j0,k0) = models(Iatmos)%levs(k0)
            end do
          end do
        end do
!
        if (ma%has_bdyright) then
          do k0 = 1 , kz
            ptrX(:,jde2+1,k0) = ptrX(:,jde2,k0)+(ptrX(:,jde2,k0)-       &
                                ptrX(:,jde2-1,k0))
            ptrY(:,jde2+1,k0) = ptrY(:,jde2,k0)+(ptrY(:,jde2,k0)-       &
                                ptrY(:,jde2-1,k0))
            ptrZ(:,jde2+1,k0) = models(Iatmos)%levs(k0)
          end do
        end if
!
        if (ma%has_bdytop) then
          do k0 = 1 , kz
            ptrX(ide2+1,:,k0) = ptrX(ide2,:,k0)+(ptrX(ide2,:,k0)-       &
                                ptrX(ide2-1,:,k0))
            ptrY(ide2+1,:,k0) = ptrY(ide2,:,k0)+(ptrY(ide2,:,k0)-       &
                                ptrY(ide2-1,:,k0))
            ptrZ(ide2+1,:,k0) = models(Iatmos)%levs(k0)
          end do
        end if
      else if (models(Iatmos)%mesh(i)%gtype == Icross) then
        if (debugLevel > 0) then
          write(*,110) localPet, j, adjustl("DAT/ATM/GRD3D/"//name),    &
                      ice1, ice2, jce1, jce2, 1, kz,                    &
                      ma%has_bdybottom, ma%has_bdyright,                &
                      ma%has_bdytop, ma%has_bdyleft
        end if
!
        do k0 = 1 , kz
          do i0 = ice1, ice2
            do j0 = jce1, jce2
              ptrX(i0,j0,k0) = mddom%xlon(j0,i0)
              ptrY(i0,j0,k0) = mddom%xlat(j0,i0)
              ptrZ(i0,j0,k0) = models(Iatmos)%levs(k0) 
            end do
          end do
        end do
!
        if (ma%has_bdyright) then
          do k0 = 1 , kz
            ptrX(:,jce2+1,k0) = ptrX(:,jce2,k0)+(ptrX(:,jce2,k0)-       &
                                ptrX(:,jce2-1,k0))
            ptrY(:,jce2+1,k0) = ptrY(:,jce2,k0)+(ptrY(:,jce2,k0)-       &
                                ptrY(:,jce2-1,k0))
            ptrZ(:,jce2+1,k0) = models(Iatmos)%levs(k0) 
          end do
        end if
!
        if (ma%has_bdytop) then
          do k0 = 1 , kz
            ptrX(ice2+1,:,k0) = ptrX(ice2,:,k0)+(ptrX(ice2,:,k0)-       &
                                ptrX(ice2-1,:,k0))
            ptrY(ice2+1,:,k0) = ptrY(ice2,:,k0)+(ptrY(ice2,:,k0)-       &
                                ptrY(ice2-1,:,k0))
            ptrZ(ice2+1,:,k0) = models(Iatmos)%levs(k0) 
          end do
        end if
      end if
!
!-----------------------------------------------------------------------
!     Nullify pointer to make sure that it does not point on a random 
!     part in the memory 
!-----------------------------------------------------------------------
!
      if (associated(ptrY)) then
        nullify(ptrY)
      end if
      if (associated(ptrX)) then
        nullify(ptrX)
      end if
      if (associated(ptrZ)) then
        nullify(ptrZ)
      end if
!
      end do
!
!-----------------------------------------------------------------------
!     Debug: write out component grid in VTK format 
!-----------------------------------------------------------------------
!
      if (debugLevel > 1) then
      call ESMF_GridWriteVTK(models(Iatmos)%grid3d,                     &
                         filename="atmos_"//                            &
                         trim(GRIDDES(models(Iatmos)%mesh(i)%gtype))//  &
                         "point_3d",                                    &
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
 110  format(" PET(",I3.3,") - DE(",I2.2,") - ",A20," : ",              &
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
      if (models(Iatmos)%exportField(k)%rank .eq. 2) then
!
!-----------------------------------------------------------------------
!     Set staggering type 
!-----------------------------------------------------------------------
!
      if (models(Iatmos)%exportField(k)%gtype == Icross) then
        staggerLoc = ESMF_STAGGERLOC_CENTER
      else if (models(Iatmos)%exportField(k)%gtype == Idot) then
        staggerLoc = ESMF_STAGGERLOC_CORNER
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
      if (models(Iatmos)%importField(k)%rank .eq. 2) then
!
!-----------------------------------------------------------------------
!     Set staggering type 
!-----------------------------------------------------------------------
!
      if (models(Iatmos)%importField(k)%gtype == Icross) then
        staggerLoc = ESMF_STAGGERLOC_CENTER
      else if (models(Iatmos)%importField(k)%gtype == Idot) then
        staggerLoc = ESMF_STAGGERLOC_CORNER
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
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,&
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
      else if (models(Iatmos)%exportField(k)%gtype == Idot) then
        staggerLoc = ESMF_STAGGERLOC_CORNER
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
      else if (models(Iatmos)%importField(k)%gtype == Idot) then
        staggerLoc = ESMF_STAGGERLOC_CORNER
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
      subroutine ATM_ModelAdvance(gcomp, rc)
!
!-----------------------------------------------------------------------
!     Used module declarations 
!-----------------------------------------------------------------------
!
      use mod_runparams, only : ifrest, ktau, dtsrf
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
      real*8 :: tstr, tend, tint
      integer :: localPet, petCount, phase, ddt 
      character(ESMF_MAXSTR) :: str1, str2
!     
      type(ESMF_VM) :: vm
      type(ESMF_Clock) :: clock, driverClock
      type(ESMF_TimeInterval) :: timeStep, timeStepDrv, timeFrom, timeTo
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
        timeFrom = currTime-esmRestartTime
      else
        timeFrom = currTime-esmStartTime
      end if
!
      call ESMF_TimeIntervalGet(timeFrom, s_r8=tstr, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      timeTo = timeFrom+timeStep
      call ESMF_TimeIntervalGet(timeTo, s_r8=tend, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      if (restarted .and. startTime == currTime) tstr = tstr+dtsrf
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
          write(*,50) trim(str1), trim(str2), phase, tstr, tend
        end if
      end if
!
!-----------------------------------------------------------------------
!     Get import fields 
!-----------------------------------------------------------------------
!
      ddt = maxval(connectors(Iatmos,:)%divDT, mask=models(:)%modActive)
      if (((currTime /= refTime) .or. restarted) .and.                  &
         ((currTime-startTime)/timeStep .eq. ddt)) then
        call ATM_Get(gcomp, rc=rc)
      end if
!
!-----------------------------------------------------------------------
!     Run ATM component
!-----------------------------------------------------------------------
!
      call ATM_Run(tstr, tend)
!
!-----------------------------------------------------------------------
!     Put export fields 
!-----------------------------------------------------------------------
!
      call ATM_Put(gcomp, rc=rc)
!
!-----------------------------------------------------------------------
!     Formats 
!-----------------------------------------------------------------------
!
 40   format(' Running ATM Component: ',A,' --> ',A,' Phase: ',I1)
 50   format(' Running ATM Component: ',A,' --> ',A,' Phase: ',I1,      &
             ' [',F15.2, '-', F15.2, ']')
!
      end subroutine ATM_ModelAdvance
!
      subroutine ATM_SetFinalize(gcomp, importState, exportState,       &
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
      call ATM_Finalize()
!
      end subroutine ATM_SetFinalize
!
      subroutine ATM_Get(gcomp, rc)
!
!-----------------------------------------------------------------------
!     Used module declarations 
!-----------------------------------------------------------------------
!
      use mod_update, only : importFields 
      use mod_dynparam, only : ici1, ici2, jci1, jci2
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
      integer :: i, j, ii, jj, n, m, id, imin, imax, jmin, jmax
      integer :: iyear, iday, imonth, ihour, iunit
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
                  ici1, ici2, jci1, jci2
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
        do m = ici1, ici2
          do n = jci1, jci2
            importFields%sst(n,m) = (ptr(m,n)*sfac)+addo
          end do
        end do 
#ifdef OCNICE
      case ('sit')
        do m = ici1, ici2
          do n = jci1, jci2
            importFields%sit(n,m) = (ptr(m,n)*sfac)+addo
          end do
        end do
#endif
      case ('msk')
        do m = ici1, ici2
          do n = jci1, jci2
            importFields%msk(n,m) = ptr(m,n)
          end do
        end do
!     Import from WAV
      case ('zo')
        do m = ici1, ici2
          do n = jci1, jci2
            importFields%zo(n,m) = (ptr(m,n)*sfac)+addo
          end do
        end do
      case ('ustar')
        do m = ici1, ici2
          do n = jci1, jci2
            importFields%ustar(n,m) = (ptr(m,n)*sfac)+addo
          end do
        end do        
      end select
!
!-----------------------------------------------------------------------
!     Debug: write field in ASCII format   
!-----------------------------------------------------------------------
!
      if (debugLevel == 4) then
        write(ofile,70) 'atm_import', trim(itemNameList(i)),            &
                        iyear, imonth, iday, ihour, localPet, j
        iunit = localPet*10
        open(unit=iunit, file=trim(ofile)//'.txt')
        call print_matrix(ptr, ici1, ici2, jci1, jci2, 1, 1,            &
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
      end subroutine ATM_Get
!
      subroutine ATM_Put(gcomp, rc)
!
!-----------------------------------------------------------------------
!     Used module declarations 
!-----------------------------------------------------------------------
!
      use mod_constants, only : regrav, d_100
      use mod_hgt, only : htsig_s !, nonhydrost_s
      use mod_vertint, only : intlinregz
      use mod_mppparam, only : ma
      use mod_update, only : exportFields, exportFields3d
      use mod_dynparam, only : ici1, ici2, jci1, jci2
      use mod_dynparam, only : ice1, ice2, jce1, jce2
      use mod_dynparam, only : kz, ptop, idynamic 
      use mod_atm_interface, only : mddom
      use mod_runparams, only : sigma
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
      integer :: i, j, k, ii, jj, dd, m, n, nz, imin, imax, jmin, jmax
      integer :: iyear, iday, imonth, ihour, iminute, isec, iunit
      integer :: petCount, localPet, itemCount, localDECount
      character(ESMF_MAXSTR) :: cname, ofile
      character(ESMF_MAXSTR), allocatable :: itemNameList(:)
      real(ESMF_KIND_R8), pointer :: ptr2d(:,:)
      real(ESMF_KIND_R8), pointer :: ptr3d(:,:,:)
      real(ESMF_KIND_R8), allocatable :: zvar(:,:,:), hzvar(:,:,:) 
      integer(ESMF_KIND_I8) :: tstep
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
!     Rotate wind on a rectangular north-south east-west oriented grid 
!-----------------------------------------------------------------------
!
      call uvrot(exportFields%wndu, exportFields%wndv)
      call uvrot(exportFields%taux, exportFields%tauy)
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
        do m = ici1, ici2
          do n = jci1, jci2
            ptr2d(m,n) = exportFields%psfc(n,m)
          end do
        end do
      case ('tsfc')
        do m = ici1, ici2
          do n = jci1, jci2
            ptr2d(m,n) = exportFields%tsfc(n,m)
          end do
        end do
      case ('qsfc')
        do m = ici1, ici2
          do n = jci1, jci2
            ptr2d(m,n) = exportFields%qsfc(n,m)
          end do
        end do
      case ('lwrd')
        do m = ici1, ici2
          do n = jci1, jci2
            ptr2d(m,n) = exportFields%lwrd(n,m)
          end do
        end do
      case ('dlwr')
        do m = ici1, ici2
          do n = jci1, jci2
            ptr2d(m,n) = exportFields%dlwr(n,m)
          end do
        end do
      case ('lhfx')
        do m = ici1, ici2
          do n = jci1, jci2
            ptr2d(m,n) = exportFields%lhfx(n,m)
          end do
        end do
      case ('shfx')
        do m = ici1, ici2
          do n = jci1, jci2
            ptr2d(m,n) = exportFields%shfx(n,m)
          end do
        end do
      case ('prec')
        do m = ici1, ici2
          do n = jci1, jci2
            ptr2d(m,n) = exportFields%prec(n,m)
          end do
        end do
      case ('wndu')
        do m = ici1, ici2
          do n = jci1, jci2
            ptr2d(m,n) = exportFields%wndu(n,m)
          end do
        end do
      case ('wndv')
        do m = ici1, ici2
          do n = jci1, jci2
            ptr2d(m,n) = exportFields%wndv(n,m)
          end do
        end do
      case ('swrd')
        do m = ici1, ici2
          do n = jci1, jci2
            ptr2d(m,n) = exportFields%swrd(n,m)
          end do
        end do
      case ('dswr')
        do m = ici1, ici2
          do n = jci1, jci2
            ptr2d(m,n) = exportFields%dswr(n,m)
          end do
        end do
      case ('rnof')
        do m = ici1, ici2
          do n = jci1, jci2
            ptr2d(m,n) = exportFields%rnof(n,m)
          end do
        end do
      case ('snof')
        do m = ici1, ici2
          do n = jci1, jci2
            ptr2d(m,n) = exportFields%snof(n,m)
          end do
        end do
      case ('taux')
        do m = ici1, ici2
          do n = jci1, jci2
            ptr2d(m,n) = exportFields%taux(n,m)
          end do
        end do
      case ('tauy')
        do m = ici1, ici2
          do n = jci1, jci2
            ptr2d(m,n) = exportFields%tauy(n,m)
          end do
        end do
      case ('wspd')
        do m = ici1, ici2
          do n = jci1, jci2
            ptr2d(m,n) = exportFields%wspd(n,m)
          end do
        end do
      case ('wdir')
        do m = ici1, ici2
          do n = jci1, jci2
            dd = atan2(exportFields%wndu(n,m), exportFields%wndv(n,m))
            if (dd < ZERO_R8) dd = dd+pi2
            ptr2d(m,n) = dd
          end do
        end do
      case ('ustr')
        do m = ici1, ici2
          do n = jci1, jci2
            ptr2d(m,n) = exportFields%ustr(n,m)
          end do
        end do
      case ('nflx')
        do m = ici1, ici2
          do n = jci1, jci2
            ptr2d(m,n) = exportFields%nflx(n,m)
          end do
        end do
      case ('sflx')
        do m = ici1, ici2
          do n = jci1, jci2
            ptr2d(m,n) = exportFields%sflx(n,m)
          end do
        end do
      case ('snow')
        do m = ici1, ici2
          do n = jci1, jci2
            ptr2d(m,n) = exportFields%snow(n,m)
          end do
        end do
      case ('topo')
        do m = ici1, ici2
          do n = jci1, jci2
            ptr2d(m,n) = mddom%ht(n,m)*regrav
          end do
        end do
      case ('mask')
        do m = ici1, ici2
          do n = jci1, jci2
            if ((mddom%mask(n,m) > ONE_R8)) then
              ptr2d(m,n) = ONE_R8
            else
              ptr2d(m,n) = ZERO_R8
            end if
          end do
        end do
      end select
!
!-----------------------------------------------------------------------
!     Fill domain boundaries with data 
!-----------------------------------------------------------------------
!
      if (ma%has_bdytop) then ! right 
        ptr2d(ice2,:) = ptr2d(ice2-1,:)
        ptr2d(ice2+1,:) = ptr2d(ice2-1,:)
      end if 
!
      if (ma%has_bdybottom) then ! left
        ptr2d(ice1,:) = ptr2d(ice1+1,:)
      end if
!
      if (ma%has_bdyright) then !top
        ptr2d(:,jce2) = ptr2d(:,jce2-1)
        ptr2d(:,jce2+1) = ptr2d(:,jce2-1)
      end if
!
      if (ma%has_bdyleft) then ! bottom
        ptr2d(:,jce1) = ptr2d(:,jce1+1)
      end if
!
!-----------------------------------------------------------------------
!     Debug: write field in ASCII format   
!-----------------------------------------------------------------------
!
      if (debugLevel == 4) then
        iunit = localPet
        write(ofile,90) 'atm_export', trim(itemNameList(i)),            &
                        iyear, imonth, iday, ihour, localPet, j
        open(unit=iunit, file=trim(ofile)//'.txt') 
        call print_matrix(transpose(ptr2d), ici1, ici2, jci1, jci2,     &
                          1, 1, localPet, iunit, "PTR/ATM/EXP")
        close(unit=iunit)
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
!     Calculate heights on sigma surfaces
!-----------------------------------------------------------------------
!
      if (.not. allocated(hzvar)) then
        allocate(hzvar(jce1:jce2,ice1:ice2,kz))
        hzvar = ZERO_R8
      end if    
!
      if (idynamic == 1) then
        call htsig_s(exportFields3d%t, hzvar, exportFields%psfc*d_100,&
                     mddom%ht(jce1:jce2,ice1:ice2)*regrav,              &
                     sigma, ptop*d_100, jce1, jce2, ice1, ice2, kz)
!      else
!        call nonhydrost_s(hzvar, exportFields3d%t,                      &
!                          (exportFields%psfc-ptop)*d_100, ptop*d_100,   &
!                          mddom%ht(jce1:jce2,ice1:ice2), sigma,         &
!                          jce1, jce2, ice1, ice2, kz)
      end if
!
!-----------------------------------------------------------------------
!     Perform vertical interpolation from sigma to height 
!-----------------------------------------------------------------------
!
      nz = models(Iatmos)%nLevs
!
      if (.not. allocated(zvar)) then
        allocate(zvar(jce1:jce2,ice1:ice2,nz))
        zvar = ZERO_R8
      end if
!
      select case (trim(adjustl(itemNameList(i))))
      case ('tlev')
        call intlinregz(zvar, exportFields3d%t, hzvar, sigma,           &
                        jce1, jce2, ice1, ice2, kz,                     &
                        models(Iatmos)%levs,nz)
      case ('qlev')
        call intlinregz(zvar, exportFields3d%q, hzvar, sigma,           &
                        jce1, jce2, ice1, ice2, kz,                     &
                        models(Iatmos)%levs,nz)
      case ('ulev')
        call intlinregz(zvar, exportFields3d%u, hzvar, sigma,           &
                        jce1, jce2, ice1, ice2, kz,                     &
                        models(Iatmos)%levs,nz)
      case ('vlev')
        call intlinregz(zvar, exportFields3d%v, hzvar, sigma,           &
                        jce1, jce2, ice1, ice2, kz,                     &
                        models(Iatmos)%levs,nz)
      case ('wlev')
        call intlinregz(zvar, exportFields3d%w, hzvar, sigma,           &
                        jce1, jce2, ice1, ice2, kz,                     &
                        models(Iatmos)%levs,nz)
      case ('cldfrc')
        call intlinregz(zvar, exportFields3d%cldfrc, hzvar, sigma,      &
                        jce1, jce2, ice1, ice2, kz,                     &
                        models(Iatmos)%levs,nz)
      case ('cldlwc')
        call intlinregz(zvar, exportFields3d%cldlwc, hzvar, sigma,      &
                        jce1, jce2, ice1, ice2, kz,                     &
                        models(Iatmos)%levs,nz)
      end select
!
!-----------------------------------------------------------------------
!     Put data to export field 
!-----------------------------------------------------------------------
!
      do k = 1 , nz
        do m = ice1, ice2
          do n = jce1, jce2
            ptr3d(m,n,k) = zvar(n,m,k)
          end do
        end do
      end do
!
!-----------------------------------------------------------------------
!     Fill domain boundaries with data 
!-----------------------------------------------------------------------
!
      if (ma%has_bdytop) then ! right 
        do k = 1 , nz
          ptr3d(ice2,:,k) = ptr3d(ice2-1,:,k)
          ptr3d(ice2+1,:,k) = ptr3d(ice2-1,:,k)
        end do
      end if
!
      if (ma%has_bdybottom) then ! left
        do k = 1 , nz
          ptr3d(ice1,:,k) = ptr3d(ice1+1,:,k)
        end do
      end if
!
      if (ma%has_bdyright) then !top
        do k = 1 , nz
          ptr3d(:,jce2,k) = ptr3d(:,jce2-1,k)
          ptr3d(:,jce2+1,k) = ptr3d(:,jce2-1,k)
        end do
      end if
!
      if (ma%has_bdyleft) then ! bottom
        do k = 1 , nz
          ptr3d(:,jce1,k) = ptr3d(:,jce1+1,k)
        end do
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
        call ESMF_FieldWrite(field, trim(ofile)//'.nc',                 &
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
      if (allocated(zvar)) deallocate(zvar)
      if (allocated(hzvar)) deallocate(hzvar)
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
      subroutine uvrot(u, v)
!
!-----------------------------------------------------------------------
!     Used module declarations 
!-----------------------------------------------------------------------
!
      use mod_constants, only : degrad
      use mod_atm_interface, only : mddom
      use mod_dynparam, only : iproj
      use mod_dynparam, only : clon, clat, plon, plat, xcone
      use mod_dynparam, only : ici1, ici2, jci1, jci2
!
      implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
      real*8, intent(inout) :: u(jci1:jci2,ici1:ici2)
      real*8, intent(inout) :: v(jci1:jci2,ici1:ici2)
!
!-----------------------------------------------------------------------
!     Local variable declarations 
!-----------------------------------------------------------------------
!
      integer*4 :: i, j, ii, jj
      real*8 :: x, xs, xc, d, us, vs, sindel, cosdel
      real*8 :: pollam, polphi, polcphi, polsphi
      real*8 :: zarg1, zarg2, znorm, zphi, zrla, zrlap
!
      if (iproj == 'ROTMER' .or. iproj == 'NORMER') then ! ROTMER, Rotated Mercator: NORMER, Normal  Mercator
        if (plat > 0.0d0) then
          pollam = plon+180.0d0
          polphi = 90.0d0-plat
        else
          polphi = 90.0d0+plat
          pollam = plon
        end if
        if (pollam > 180.0d0) pollam = pollam-360.0d0
!
        polcphi = dcos(degrad*polphi)
        polsphi = dsin(degrad*polphi)
!
        do j = jci1, jci2
          do i = ici1, ici2
            zphi = mddom%dlat(j,i)*degrad
            zrla = mddom%dlon(j,i)*degrad
            if (mddom%dlat(j,i) > 89.999999D0) zrla = 0.0d0
            zrlap = pollam*degrad-zrla
            zarg1 = polcphi*dsin(zrlap)
            zarg2 = polsphi*dcos(zphi)-polcphi*dsin(zphi)*dcos(zrlap)
            znorm = 1.0d0/dsqrt(zarg1**2+zarg2**2)
            sindel = zarg1*znorm
            cosdel = zarg2*znorm
!
            us = u(j,i)*cosdel+v(j,i)*sindel
            vs = -u(j,i)*sindel+v(j,i)*cosdel
            u(j,i) = us
            v(j,i) = vs 
          end do
        end do
      else ! LAMCON, Lambert conformal
        do i = ici1, ici2
          do j = jci1, jci2
            if ((clon >= 0.0d0 .and. mddom%xlon(j,i) >= 0.0d0) .or.     &
                (clon < 0.0d0 .and. mddom%xlon(j,i) < 0.0d0)) then
              x = (clon-mddom%xlon(j,i))*degrad*xcone
            else
              if (clon >= 0.0d0) then
                if (abs(clon-(mddom%xlon(j,i)+360.0d0)) <               &
                    abs(clon-mddom%xlon(j,i))) then
                  x = (clon-(mddom%xlon(j,i)+360.0d0))*degrad*xcone
                else
                  x = (clon-mddom%xlon(j,i))*degrad*xcone
                end if
              else
                if (abs(clon-(mddom%xlon(j,i)-360.0d0)) <               &
                    abs(clon-mddom%xlon(j,i))) then
                  x = (clon-(mddom%xlon(j,i)-360.0d0))*degrad*xcone
                else
                  x = (clon-mddom%xlon(j,i))*degrad*xcone
                end if              
              end if
            end if 
!
            xs = sin(x)
            xc = cos(x)
!
            if (clat >= 0.0d0) then
              d = u(j,i)*xc-v(j,i)*xs
              v(j,i) = u(j,i)*xs+v(j,i)*xc
              u(j,i) = d
            else
              d = u(j,i)*xc+v(j,i)*xs
              v(j,i) = v(j,i)*xc-u(j,i)*xs
              u(j,i) = d
            end if
          end do
        end do
      end if
!
      end subroutine uvrot
!
      end module mod_esmf_atm
