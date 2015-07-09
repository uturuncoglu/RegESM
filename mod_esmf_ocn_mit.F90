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
      use mod_mit_gcm, only : sNx, sNy, nSx, nSy, OLx, OLy, Nx, Ny,     &
                              nPx, nPy, myXGlobalLo, myYGlobalLo
!
      implicit none
      private
!
!-----------------------------------------------------------------------
!     Global module variables 
!-----------------------------------------------------------------------
!
      real*8  :: myTime = 0.0d0
      integer :: iLoop = 0
      integer :: myIter = 0
      integer, allocatable :: mpi_myXGlobalLo(:), mpi_myYGlobalLo(:)
!
      type(ESMF_RouteHandle) :: rh_halo
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
      call NUOPC_CompDerive(gcomp, NUOPC_SetServices, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Register initialize routine (P 1/2) for specific implementation   
!-----------------------------------------------------------------------
!
      call NUOPC_CompSetEntryPoint(gcomp,                               &
                                   methodflag=ESMF_METHOD_INITIALIZE,   &
                                   phaseLabelList=(/"IPDv00p1"/),       &
                                   userRoutine=OCN_SetInitializeP1,     &
                                   rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      call NUOPC_CompSetEntryPoint(gcomp,                               &
                                   methodflag=ESMF_METHOD_INITIALIZE,   &
                                   phaseLabelList=(/"IPDv00p2"/),       &
                                   userRoutine=OCN_SetInitializeP2,     &
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
                                specRoutine=OCN_DataInit, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      call NUOPC_CompSpecialize(gcomp, specLabel=NUOPC_Label_SetClock,  &
                                specRoutine=OCN_SetClock, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      call NUOPC_CompSpecialize(gcomp,                                  &
                                specLabel=NUOPC_Label_CheckImport,      &
                                specPhaseLabel="RunPhase1",             &
                                specRoutine=OCN_CheckImport, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      call NUOPC_CompSpecialize(gcomp, specLabel=NUOPC_Label_Advance,   &
                                specRoutine=OCN_ModelAdvance, rc=rc)
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
      integer :: myThid = 1
      integer :: comm, localPet, petCount
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
      call MIT_INIT(comm, iLoop, myTime, myIter, myThid) 
!
!-----------------------------------------------------------------------
!     Set-up grid and load coordinate data 
!-----------------------------------------------------------------------
!
      call OCN_SetGridArrays(gcomp, petCount, localPet, rc)
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
      use mod_mit_gcm, only : TheCalendar, startdate_1, startdate_2,    &
                              starttime
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
      type(ESMF_TimeInterval) :: timeStep, elapsedTime
      type(ESMF_Time) :: cmpRefTime, cmpStartTime, cmpStopTime
      type(ESMF_Time) :: dummTime, currTime
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
      call ESMF_TimeIntervalSet(elapsedTime, s_r8=starttime, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      cmpStartTime = cmpStartTime+elapsedTime
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
        dummTime = esmRestartTime
      else
        dummTime = esmStartTime
      end if
!
      if (cmpStartTime /= dummTime) then
        call ESMF_TimePrint(cmpStartTime, options="string", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
                               line=__LINE__, file=FILENAME)) return
!
        call ESMF_TimePrint(dummTime, options="string", rc=rc)
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
      type(ESMF_Time) :: startTime, currTime
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
!-----------------------------------------------------------------------
!     Get the start time and current time out of the clock
!-----------------------------------------------------------------------
!
      call ESMF_ClockGet(driverClock, startTime=startTime,              &
                         currTime=currTime, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Query component for its importState
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
        call ESMF_LogSetError(ESMF_RC_ARG_BAD,&
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
      subroutine OCN_SetGridArrays(gcomp, petCount, localPet, rc)
!
!-----------------------------------------------------------------------
!     Used module declarations 
!-----------------------------------------------------------------------
!
      use mod_mit_gcm, only : xC, yC, xG, yG, maskC, maskW, maskS, rA
!
      implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
      type(ESMF_GridComp), intent(inout) :: gcomp
      integer, intent(in) :: localPet 
      integer, intent(in) :: petCount 
      integer, intent(inout) :: rc
!
!-----------------------------------------------------------------------
!     Local variable declarations 
!-----------------------------------------------------------------------
!
      integer :: i, j, k, m, n, p, nr, bi, bj, iG, jG
      integer :: localDECount, tile
      character(ESMF_MAXSTR) :: name
!
      integer(ESMF_KIND_I4), pointer :: ptrM(:,:)
      real(ESMF_KIND_R8), pointer :: ptrX(:,:), ptrY(:,:), ptrA(:,:)
      character(ESMF_MAXSTR) :: cname
!
      type(ESMF_VM) :: vm
      type(ESMF_Array) :: arrX, arrY, arrM, arrA
      type(ESMF_StaggerLoc) :: staggerLoc
      type(ESMF_DistGrid) :: distGrid
      integer, allocatable :: deBlockList(:,:,:)
!
      rc = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!     Get gridded component 
!-----------------------------------------------------------------------
!
      call ESMF_GridCompGet(gcomp, vm=vm, name=cname, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Collect bottom-left X-index and bottom-left Y-index
!-----------------------------------------------------------------------
!
      if (.not. allocated(mpi_myXGlobalLo)) then
        allocate(mpi_myXGlobalLo(nPx*nPy))
        allocate(mpi_myYGlobalLo(nPx*nPy))
      end if
!
      call ESMF_VMAllGatherV(vm, sendData=(/ myXGlobalLo /),            &
                             sendCount=1, recvData=mpi_myXGlobalLo,     &
                             recvCounts=(/ (1, k = 0, petCount-1) /),   &
                             recvOffsets=(/ (k, k = 0, petCount-1) /),  &
                             rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return      
!
      call ESMF_VMAllGatherV(vm, sendData=(/ myYGlobalLo /),            &
                             sendCount=1, recvData=mpi_myYGlobalLo,     &
                             recvCounts=(/ (1, k = 0, petCount-1) /),   &
                             recvOffsets=(/ (k, k = 0, petCount-1) /),  &
                             rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Get limits of the grid arrays (based on PET and nest level)
!-----------------------------------------------------------------------
!
      if (.not.allocated(deBlockList)) then
        allocate(deBlockList(2,2,nPx*nPy))
      end if
!
      bj = 1
      bi = 1
      do tile = 0, (nPx*nPy)-1
        deBlockList(1,1,tile+1) = mpi_myXGlobalLo(tile+1)
        deBlockList(1,2,tile+1) = mpi_myXGlobalLo(tile+1)+sNx-1
        deBlockList(2,1,tile+1) = mpi_myYGlobalLo(tile+1) 
        deBlockList(2,2,tile+1) = mpi_myYGlobalLo(tile+1)+sNy-1
      end do
!
!-----------------------------------------------------------------------
!     Create ESMF DistGrid based on model domain decomposition
!-----------------------------------------------------------------------
!
      distGrid = ESMF_DistGridCreate(minIndex=(/ 1, 1 /),               &
                                     maxIndex=(/ Nx, Ny /),             &
                                     deBlockList=deBlockList,           &
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
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
      end if
!
!-----------------------------------------------------------------------
!     Set mask value for land and ocean 
!-----------------------------------------------------------------------
!
      models(Iocean)%isLand = 0
      models(Iocean)%isOcean = 1
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
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
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
                    lbound(ptrX, dim=1), ubound(ptrX, dim=1),           &
                    lbound(ptrX, dim=2), ubound(ptrX, dim=2)
      end if
!
!-----------------------------------------------------------------------
!     Fill the pointers    
!-----------------------------------------------------------------------
!
      bj = 1
      bi = 1
      if (models(Iocean)%mesh(p)%gtype == Idot) then
        do n = 1, sNy
          do m = 1, sNx
            iG = myXGlobalLo-1+(bi-1)*sNx+m
            jG = myYGlobalLo-1+(bj-1)*sNy+n
            ptrX(iG,jG) = xG(m,n,1,1)
            ptrY(iG,jG) = yG(m,n,1,1)
          end do
        end do
      else if (models(Iocean)%mesh(p)%gtype == Icross) then
        do n = 1, sNy
          do m = 1, sNx
            iG = myXGlobalLo-1+(bi-1)*sNx+m
            jG = myYGlobalLo-1+(bj-1)*sNy+n
            ptrX(iG,jG) = xC(m,n,1,1)
            ptrY(iG,jG) = yC(m,n,1,1)
            ptrM(iG,jG) = int(maskC(m,n,1,1,1))
            ptrA(iG,jG) = rA(m,n,1,1)
          end do
        end do
      else if (models(Iocean)%mesh(p)%gtype == Iupoint) then
        do n = 1, sNy
          do m = 1, sNx
            iG = myXGlobalLo-1+(bi-1)*sNx+m
            jG = myYGlobalLo-1+(bj-1)*sNy+n
            ptrX(iG,jG) = xG(m,n,1,1)
            ptrY(iG,jG) = yC(m,n,1,1)
            ptrM(iG,jG) = int(maskW(m,n,1,1,1))
          end do
        end do
      else if (models(Iocean)%mesh(p)%gtype == Ivpoint) then
        do n = 1, sNy
          do m = 1, sNx
            iG = myXGlobalLo-1+(bi-1)*sNx+m
            jG = myYGlobalLo-1+(bj-1)*sNy+n
            ptrX(iG,jG) = xC(m,n,1,1)
            ptrY(iG,jG) = yG(m,n,1,1)
            ptrM(iG,jG) = int(maskS(m,n,1,1,1))
          end do
        end do
      end if
!
      if (debugLevel > 0) then
        write(*,30) localPet, j, adjustl("DAT/OCN/GRD/"//name),         &
                    lbound(xG, dim=1), ubound(xG, dim=1),               &
                    lbound(xG, dim=2), ubound(xG, dim=2)
      end if
!
!-----------------------------------------------------------------------
!     Create temporary arrays.
!-----------------------------------------------------------------------
!
       if (models(Iocean)%mesh(p)%gtype == Icross) then
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
!     Collect data from arrays to first PET of the component
!-----------------------------------------------------------------------
!
      if (models(Iriver)%modActive) then
!
      i = minloc(models(Iocean)%mesh(:)%gtype, dim=1,                   &
                 mask=(models(Iocean)%mesh(:)%gtype == Icross))
!
      if (localPet == 0) then
        allocate(models(Iocean)%mesh(i)%glon(Nx,Ny))
        allocate(models(Iocean)%mesh(i)%glat(Nx,Ny))
        allocate(models(Iocean)%mesh(i)%gmsk(Nx,Ny))
        allocate(models(Iocean)%mesh(i)%gare(Nx,Ny))
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
 20   format(" RIVER(",I2.2,") - ",I4,3F8.2," [",I3.3,":",I3.3,"] - ",I4," ",A)
 30   format(" PET(",I3.3,") - DE(",I2.2,") - ", A20, " : ", 4I8)
!
      end subroutine OCN_SetGridArrays
!
      subroutine OCN_SetStates(gcomp, rc)
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
!-----------------------------------------------------------------------
!     Create field 
!-----------------------------------------------------------------------
!
      field = ESMF_FieldCreate(models(Iocean)%grid,                     &
                               arraySpec,                               &
                               staggerloc=staggerLoc,                   &
                               indexflag=ESMF_INDEX_GLOBAL,             &
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
!-----------------------------------------------------------------------
!     Create field
!-----------------------------------------------------------------------
!
      field = ESMF_FieldCreate(models(Iocean)%grid,                     &
                               arraySpec,                               &
                               totalLWidth=(/OLx,OLy/),                 &
                               totalUWidth=(/OLx,OLy/),                 &
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
!      if (i == 1) then
!      call ESMF_FieldHaloStore(field, routehandle=rh_halo, rc=rc)
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
      real*8 :: trun
      integer :: myThid = 1
      integer :: localPet, petCount, phase, iter
      character(ESMF_MAXSTR) :: str1, str2
!     
      type(ESMF_VM) :: vm
      type(ESMF_Clock) :: clock
      type(ESMF_TimeInterval) :: timeStep
      type(ESMF_Time) :: startTime, stopTime, currTime
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
      call ESMF_ClockGet(clock, timeStep=timeStep, startTime=startTime, &
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
      if ((currTime /= startTime) .or. restarted) then
        call OCN_Get(gcomp, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
                               line=__LINE__, file=FILENAME)) return
      end if
!
!-----------------------------------------------------------------------
!     Run OCN component
!-----------------------------------------------------------------------
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
      call MIT_FINALIZE()
!
      end subroutine OCN_SetFinalize
!
      subroutine OCN_Get(gcomp, rc)
!
!-----------------------------------------------------------------------
!     Used module declarations 
!-----------------------------------------------------------------------
!
      use mod_mit_gcm, only : ustress_ESMF, vstress_ESMF, hflux_ESMF,   &
                              sflux_ESMF, swflux_ESMF, atemp_ESMF,      &
                              aqh_ESMF, lwflux_ESMF, evap_ESMF,         &
                              wspeed_ESMF, precip_ESMF, runoff_ESMF,    &
                              swdown_ESMF, lwdown_ESMF, apressure_ESMF, &
                              snowprecip_ESMF, uwind_ESMF, vwind_ESMF
      use mod_mit_gcm, only : xG, yG
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
      integer :: i, j, ii, jj, bi, bj, iG, jG, imax, jmax
      integer :: id, iyear, iday, imonth, ihour, iunit
      integer :: LBi, UBi, LBj, UBj
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
!     Get pointer /from field
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
                  lbound(ustress_ESMF, dim=1),                          &
                  ubound(ustress_ESMF, dim=1),                          &
                  lbound(ustress_ESMF, dim=2),                          &
                  ubound(ustress_ESMF, dim=2)
      end if 
!
!-----------------------------------------------------------------------
!     Put data to OCN component variable
!-----------------------------------------------------------------------
!
      sfac = models(Iocean)%importField(id)%scale_factor
      addo = models(Iocean)%importField(id)%add_offset
!
      bi = 1
      bj = 1
      imax = Nx+1
      jmax = Ny+1
!
      where (isnan(ptr)) ptr = MISSING_R8
!
      select case (trim(adjustl(itemNameList(i))))
      case ('taux')
        do jj = 1-OLy, sNy+OLy
          do ii = 1-OLx, sNx+OLx
            iG = myXGlobalLo-1+(bi-1)*sNx+ii
            jG = myYGlobalLo-1+(bj-1)*sNy+jj
            if ((iG > 0 .and. iG < imax) .and.                          &
                (jG > 0 .and. jG < jmax) .and. ptr(iG,jG) < TOL_R8) then
              ustress_ESMF(ii,jj,1,1) = (ptr(iG,jG)*sfac)+addo
            end if
          end do
        end do
      case ('tauy')
        do jj = 1-OLy, sNy+OLy
          do ii = 1-OLx, sNx+OLx
            iG = myXGlobalLo-1+(bi-1)*sNx+ii
            jG = myYGlobalLo-1+(bj-1)*sNy+jj
            if ((iG > 0 .and. iG < imax) .and.                          &
                (jG > 0 .and. jG < jmax) .and. ptr(iG,jG) < TOL_R8) then
              vstress_ESMF(ii,jj,1,1) = (ptr(iG,jG)*sfac)+addo
            end if
          end do
        end do
      case ('nflx')
        do jj = 1-OLy, sNy+OLy
          do ii = 1-OLx, sNx+OLx
            iG = myXGlobalLo-1+(bi-1)*sNx+ii
            jG = myYGlobalLo-1+(bj-1)*sNy+jj
            if ((iG > 0 .and. iG < imax) .and.                          &
                (jG > 0 .and. jG < jmax) .and. ptr(iG,jG) < TOL_R8) then
              hflux_ESMF(ii,jj,1,1) = (ptr(iG,jG)*sfac)+addo
            end if
          end do
        end do
      case ('sflx')
        do jj = 1-OLy, sNy+OLy
          do ii = 1-OLx, sNx+OLx
            iG = myXGlobalLo-1+(bi-1)*sNx+ii
            jG = myYGlobalLo-1+(bj-1)*sNy+jj
            if ((iG > 0 .and. iG < imax) .and.                          &
                (jG > 0 .and. jG < jmax) .and. ptr(iG,jG) < TOL_R8) then
              sflux_ESMF(ii,jj,1,1) = (ptr(iG,jG)*sfac)+addo
            end if
          end do
        end do
      case ('swrd')
        do jj = 1-OLy, sNy+OLy
          do ii = 1-OLx, sNx+OLx
            iG = myXGlobalLo-1+(bi-1)*sNx+ii
            jG = myYGlobalLo-1+(bj-1)*sNy+jj
            if ((iG > 0 .and. iG < imax) .and.                          &
                (jG > 0 .and. jG < jmax) .and. ptr(iG,jG) < TOL_R8) then
              swflux_ESMF(ii,jj,1,1) = (ptr(iG,jG)*sfac)+addo
            end if
          end do
        end do
      case ('wndu')
        do jj = 1-OLy, sNy+OLy
          do ii = 1-OLx, sNx+OLx
            iG = myXGlobalLo-1+(bi-1)*sNx+ii
            jG = myYGlobalLo-1+(bj-1)*sNy+jj
            if ((iG > 0 .and. iG < imax) .and.                          &
                (jG > 0 .and. jG < jmax) .and. ptr(iG,jG) < TOL_R8) then
              uwind_ESMF(ii,jj,1,1) = (ptr(iG,jG)*sfac)+addo
            end if
          end do
        end do
      case ('wndv')
        do jj = 1-OLy, sNy+OLy
          do ii = 1-OLx, sNx+OLx
            iG = myXGlobalLo-1+(bi-1)*sNx+ii
            jG = myYGlobalLo-1+(bj-1)*sNy+jj
            if ((iG > 0 .and. iG < imax) .and.                          &
                (jG > 0 .and. jG < jmax) .and. ptr(iG,jG) < TOL_R8) then
              vwind_ESMF(ii,jj,1,1) = (ptr(iG,jG)*sfac)+addo
            end if
          end do
        end do
      case ('wspd')
        do jj = 1-OLy, sNy+OLy
          do ii = 1-OLx, sNx+OLx
            iG = myXGlobalLo-1+(bi-1)*sNx+ii
            jG = myYGlobalLo-1+(bj-1)*sNy+jj
            if ((iG > 0 .and. iG < imax) .and.                          &
                (jG > 0 .and. jG < jmax) .and. ptr(iG,jG) < TOL_R8) then
              wspeed_ESMF(ii,jj,1,1) = (ptr(iG,jG)*sfac)+addo
            end if
          end do
        end do
      case ('tsfc')
        do jj = 1-OLy, sNy+OLy
          do ii = 1-OLx, sNx+OLx
            iG = myXGlobalLo-1+(bi-1)*sNx+ii
            jG = myYGlobalLo-1+(bj-1)*sNy+jj
            if ((iG > 0 .and. iG < imax) .and.                          &
                (jG > 0 .and. jG < jmax) .and. ptr(iG,jG) < TOL_R8) then
              atemp_ESMF(ii,jj,1,1) = (ptr(iG,jG)*sfac)+addo
            end if
          end do
        end do
      case ('qsfc')
        do jj = 1-OLy, sNy+OLy
          do ii = 1-OLx, sNx+OLx
            iG = myXGlobalLo-1+(bi-1)*sNx+ii
            jG = myYGlobalLo-1+(bj-1)*sNy+jj
            if ((iG > 0 .and. iG < imax) .and.                          &
                (jG > 0 .and. jG < jmax) .and. ptr(iG,jG) < TOL_R8) then
              aqh_ESMF(ii,jj,1,1) = (ptr(iG,jG)*sfac)+addo
            end if
          end do
        end do
      case ('lwrd')
        do jj = 1-OLy, sNy+OLy
          do ii = 1-OLx, sNx+OLx
            iG = myXGlobalLo-1+(bi-1)*sNx+ii
            jG = myYGlobalLo-1+(bj-1)*sNy+jj
            if ((iG > 0 .and. iG < imax) .and.                          &
                (jG > 0 .and. jG < jmax) .and. ptr(iG,jG) < TOL_R8) then
              lwflux_ESMF(ii,jj,1,1) = (ptr(iG,jG)*sfac)+addo
            end if
          end do
        end do
      case ('evap')
        do jj = 1-OLy, sNy+OLy
          do ii = 1-OLx, sNx+OLx
            iG = myXGlobalLo-1+(bi-1)*sNx+ii
            jG = myYGlobalLo-1+(bj-1)*sNy+jj
            if ((iG > 0 .and. iG < imax) .and.                          &
                (jG > 0 .and. jG < jmax) .and. ptr(iG,jG) < TOL_R8) then
              evap_ESMF(ii,jj,1,1) = (ptr(iG,jG)*sfac)+addo
            end if
          end do
        end do
      case ('prec')
        do jj = 1-OLy, sNy+OLy
          do ii = 1-OLx, sNx+OLx
            iG = myXGlobalLo-1+(bi-1)*sNx+ii
            jG = myYGlobalLo-1+(bj-1)*sNy+jj
            if ((iG > 0 .and. iG < imax) .and.                          &
                (jG > 0 .and. jG < jmax) .and. ptr(iG,jG) < TOL_R8) then
              precip_ESMF(ii,jj,1,1) = (ptr(iG,jG)*sfac)+addo
            end if
          end do
        end do
      case ('snow')
        do jj = 1-OLy, sNy+OLy
          do ii = 1-OLx, sNx+OLx
            iG = myXGlobalLo-1+(bi-1)*sNx+ii
            jG = myYGlobalLo-1+(bj-1)*sNy+jj
            if ((iG > 0 .and. iG < imax) .and.                          &
                (jG > 0 .and. jG < jmax) .and. ptr(iG,jG) < TOL_R8) then
              snowprecip_ESMF(ii,jj,1,1) = (ptr(iG,jG)*sfac)+addo
            end if
          end do
        end do
      case ('dswr')
        do jj = 1-OLy, sNy+OLy
          do ii = 1-OLx, sNx+OLx
            iG = myXGlobalLo-1+(bi-1)*sNx+ii
            jG = myYGlobalLo-1+(bj-1)*sNy+jj
            if ((iG > 0 .and. iG < imax) .and.                          &
                (jG > 0 .and. jG < jmax) .and. ptr(iG,jG) < TOL_R8) then
              swdown_ESMF(ii,jj,1,1) = (ptr(iG,jG)*sfac)+addo
            end if
          end do
        end do
      case ('dlwr')
        do jj = 1-OLy, sNy+OLy
          do ii = 1-OLx, sNx+OLx
            iG = myXGlobalLo-1+(bi-1)*sNx+ii
            jG = myYGlobalLo-1+(bj-1)*sNy+jj
            if ((iG > 0 .and. iG < imax) .and.                          &
                (jG > 0 .and. jG < jmax) .and. ptr(iG,jG) < TOL_R8) then
              lwdown_ESMF(ii,jj,1,1) = (ptr(iG,jG)*sfac)+addo
            end if
          end do
        end do
      case ('psfc')
        do jj = 1-OLy, sNy+OLy
          do ii = 1-OLx, sNx+OLx
            iG = myXGlobalLo-1+(bi-1)*sNx+ii
            jG = myYGlobalLo-1+(bj-1)*sNy+jj
            if ((iG > 0 .and. iG < imax) .and.                          &
                (jG > 0 .and. jG < jmax) .and. ptr(iG,jG) < TOL_R8) then
              apressure_ESMF(ii,jj,1,1) = (ptr(iG,jG)*sfac)+addo
            end if
          end do
        end do
      case ('rdis')
        LBi = myXGlobalLo-1+(bi-1)*sNx+(1-OLx)
        UBi = myXGlobalLo-1+(bi-1)*sNx+(sNx+OLx)
        LBj = myYGlobalLo-1+(bj-1)*sNy+(1-OLy)
        UBj = myYGlobalLo-1+(bj-1)*sNy+(sNy+OLy)
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
        call print_matrix(uwind_ESMF(:,:,1,1), 1-OLx, sNx+OLx,          &
                          1-OLy, sNy+OLy, 1, 1,                         &
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
      integer :: bi, bj, iG, jG, imax, jmax
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
!-----------------------------------------------------------------------
!     Perform halo region update 
!-----------------------------------------------------------------------
!
!      call ESMF_FieldHalo(field, routehandle=rh_halo,                   &
!                          checkflag=.false., rc=rc)
!      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
!                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Loop over decomposition elements (DEs) 
!-----------------------------------------------------------------------
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
      bi = 1
      bj = 1
      imax = Nx+1
      jmax = Ny+1
!
      select case (trim(adjustl(itemNameList(i))))
      case ('sst')
        do jj = 1, sNy
          do ii = 1, sNx
            iG = myXGlobalLo-1+(bi-1)*sNx+ii
            jG = myYGlobalLo-1+(bj-1)*sNy+jj
            ptr(iG,jG) = theta(ii,jj,1,1,1)
          end do
        end do
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
        call print_matrix(ptr, 1, sNx, 1, sNy, 1, 1,                    &
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
      use mod_mit_gcm, only : runoff_ESMF
      use mod_mit_gcm, only : xG, yG
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
      integer :: i, j, k, r, ii, jj, iG, jG, bi, bj
      integer :: mm, ng, np, nr, localPet, petCount 
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
      ! reinitialize temporary river discharge array
      runoff_ESMF(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy) = ZERO_R8
!
      ! get number of rivers
      nr = size(rivers, dim=1)
!
      k = 0
      bi = 1
      bj = 1
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
            call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc,        &
                 msg='riverOpt == 1 is not supported by MITgcm')
            return
!
          ! apply as surface boundary condition
          else if (riverOpt == 2) then
            ! distribute data to the mapped ocean grid points
            np = rivers(r)%mapSize 
            do k = 1, np
              i = int(rivers(r)%mapTable(1,k))
              j = int(rivers(r)%mapTable(2,k))                

              do jj = 1-OLy, sNy+OLy
                do ii = 1-OLx, sNx+OLx
                  iG = myXGlobalLo-1+(bi-1)*sNx+ii
                  jG = myYGlobalLo-1+(bj-1)*sNy+jj
                  if (iG == i .and. jG ==j .and. rdis(1) < TOL_R8) then
                    runoff_ESMF(ii,jj,1,1) = runoff_ESMF(ii,jj,1,1)+  &
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
      integer :: k, n, m, bi, bj, iG, jG
      integer :: petCount, localPet, rootPet, sendData(1)
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
      bj = 1
      bi = 1
!
      do k = 1, petCount
        do n = 1, sNy
          do m = 1, sNx
            iG = mpi_myXGlobalLo(k)-1+(bi-1)*sNx+m
            jG = mpi_myYGlobalLo(k)-1+(bj-1)*sNy+n
            if (iG == i .and. jG == j) then
              rootPet = k-1
              exit
            end if
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

