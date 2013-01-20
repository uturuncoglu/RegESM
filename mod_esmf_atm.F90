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
      use NUOPC_Model, only :                                           &
          NUOPC_SetServices   => routine_SetServices,                   &
          NUOPC_Label_Advance => label_Advance
!
      use mod_types
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
                                      userRoutine=ATM_SetInitializeP1,  &
                                      phase=1,                          &
                                      rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      call ESMF_GridCompSetEntryPoint(gcomp,                            &
                                      methodflag=ESMF_METHOD_INITIALIZE,&
                                      userRoutine=ATM_SetInitializeP2,  &
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
                          userRoutine=ATM_ModelAdvance, rc=rc)

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
      rc = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!     Set import fields 
!-----------------------------------------------------------------------
!
      call NUOPC_StateAdvertiseField(importState, Units='K',            &
           StandardName='sea_surface_temperature', name='sst', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      call NUOPC_StateAdvertiseField(importState, Units='mm',           &
           StandardName='sea_ice_thickness', name='sit', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Set export fields 
!-----------------------------------------------------------------------
!
      call NUOPC_StateAdvertiseField(exportState, Units='Pa',           &
           StandardName='surface_air_pressure', name='psfc', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      call NUOPC_StateAdvertiseField(exportState, Units='K',            &
           StandardName='surface_air_temperature', name='tsfc', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      call NUOPC_StateAdvertiseField(exportState, Units='1',            &
           StandardName='surface_specific_humidity', name='qsfc', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      end subroutine ATM_SetInitializeP1
!
      subroutine ATM_SetInitializeP2(gcomp, importState, exportState,   &
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
      integer :: comm, localPet, petCount
      type(ESMF_VM) :: vm
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
      call ATM_initialize(mpiCommunicator=comm)
!
!-----------------------------------------------------------------------
!     Set-up internal clock for gridded component
!-----------------------------------------------------------------------
!
      call ATM_SetClock(clock, rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return


!
      end subroutine ATM_SetInitializeP2
!
      subroutine ATM_ModelAdvance(gcomp, rc)
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
!
      rc = ESMF_SUCCESS 
!
      end subroutine ATM_ModelAdvance
!
      subroutine ATM_SetClock(clock, rc)
      use mod_dynparam , only : calendar
!
      implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
      type(ESMF_Clock) :: clock
      integer :: rc
!
!-----------------------------------------------------------------------
!     Local variable declarations 
!-----------------------------------------------------------------------
!
      integer :: ref_year,   str_year,   end_year
      integer :: ref_month,  str_month,  end_month
      integer :: ref_day,    str_day,    end_day
      integer :: ref_hour,   str_hour,   end_hour
      integer :: ref_minute, str_minute, end_minute
      integer :: ref_second, str_second, end_second
!
      type(ESMF_Calendar) :: cal
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





      end subroutine ATM_SetClock

      end module mod_esmf_atm
