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
#define FILENAME "mod_esmf_esm.F90"
!
!-----------------------------------------------------------------------
!     ESM gridded component code 
!-----------------------------------------------------------------------
!
      module mod_esmf_esm
!
!-----------------------------------------------------------------------
!     Used module declarations 
!-----------------------------------------------------------------------
!
      use ESMF
      use NUOPC
      use NUOPC_Driver,                                                 &
          NUOPC_SetServices            => SetServices,                  &
          NUOPC_Label_SetModelServices => label_SetModelServices,       &
          NUOPC_Label_SetRunSequence   => label_SetRunSequence
!
      use mod_types
      use mod_esmf_atm, only: ATM_SetServices
      use mod_esmf_ocn, only: OCN_SetServices
      use mod_esmf_rtm, only: RTM_SetServices
      use mod_esmf_wav, only: WAV_SetServices
      use mod_esmf_cpl, only: CPL_SetServices
!
      implicit none
      private
!
!-----------------------------------------------------------------------
!     Public subroutines 
!-----------------------------------------------------------------------
!
      public :: ESM_SetServices
!
      contains
!
      subroutine ESM_SetServices(gcomp, rc)
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
!     Register generic methods 
!-----------------------------------------------------------------------
!
      call NUOPC_CompDerive(gcomp, NUOPC_SetServices, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Attach specializing methods 
!-----------------------------------------------------------------------
!
      call NUOPC_CompSpecialize(gcomp,                                  &
                                specLabel=NUOPC_Label_SetModelServices, &
                                specRoutine=ESM_SetModelServices, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      call NUOPC_CompSpecialize(gcomp,                                  &
                                specLabel=NUOPC_Label_SetRunSequence,   &
                                specRoutine=ESM_SetRunSequence, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      end subroutine ESM_SetServices
!
      subroutine ESM_SetModelServices(gcomp, rc)
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
      integer :: i, j
      logical :: restarted
!
      type(ESMF_GridComp) :: child
      type(ESMF_CplComp) :: connector
!
      rc = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!     SetServices for model components 
!-----------------------------------------------------------------------
!
      do i = 1, nModels
        if (models(i)%modActive) then
          if (i == Iatmos) then
            call NUOPC_DriverAddComp(gcomp, trim(models(i)%name),       &
                                     ATM_SetServices,                   &
                                     petList=models(i)%petList(:),      &
                                     comp=child, rc=rc)
          else if (i == Iocean) then
            call NUOPC_DriverAddComp(gcomp, trim(models(i)%name),       &
                                     OCN_SetServices,                   &
                                     petList=models(i)%petList(:),      &
                                     comp=child, rc=rc)
          else if (i == Iriver) then
            call NUOPC_DriverAddComp(gcomp, trim(models(i)%name),       &
                                     RTM_SetServices,                   &
                                     petList=models(i)%petList(:),      &
                                     comp=child, rc=rc)
          else if (i == Iwavee) then
            call NUOPC_DriverAddComp(gcomp, trim(models(i)%name),       &
                                     WAV_SetServices,                   &
                                     petList=models(i)%petList(:),      &
                                     comp=child, rc=rc)
          end if
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,&
              line=__LINE__, file=FILENAME)) return
!
          if (debugLevel > 0) then
          call ESMF_AttributeSet(child, name="Verbosity", value="high", &
                                 rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,&
                                 line=__LINE__, file=FILENAME)) return
          end if
        end if
      end do
!
!-----------------------------------------------------------------------
!     SetServices for connector components 
!-----------------------------------------------------------------------
!
      do i = 1, nModels
        do j = 1, nModels
          if (connectors(i,j)%modActive) then
            call NUOPC_DriverAddComp(gcomp,                             &
                           srcCompLabel=trim(models(i)%name),           &
                           dstCompLabel=trim(models(j)%name),           &
                           compSetServicesRoutine=CPL_SetServices,      &
                           comp=connector, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc,                        &
                                   msg=ESMF_LOGERR_PASSTHRU,            &
                                   line=__LINE__, file=FILENAME)) return

            if (debugLevel > 0) then
              call ESMF_AttributeSet(connector, name="Verbosity",       &
                                     value="high", rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc,                      &
                                     msg=ESMF_LOGERR_PASSTHRU,          &
                                     line=__LINE__, file=FILENAME)) return
            end if
          end if
        end do
      end do
!
!-----------------------------------------------------------------------
!     Set internal clock for application (gcomp). The time step must be 
!     set to the slowest time interval of the connector components
!-----------------------------------------------------------------------
!
      restarted = .false.
      if (esmStartTime /= esmRestartTime) then
        restarted = .true.
      end if
!
      if (restarted) then
        esmClock = ESMF_ClockCreate(esmTimeStep,                        &
                                    esmRestartTime,                     &
                                    stopTime=esmStopTime,               &
                                    name='ESM_clock', rc=rc)
      else
        esmClock = ESMF_ClockCreate(esmTimeStep,                        &
                                    esmStartTime,                       &
                                    stopTime=esmStopTime,               &
                                    name='ESM_clock', rc=rc)
      end if
!
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      call ESMF_GridCompSet(gcomp, clock=esmClock, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      end subroutine ESM_SetModelServices 
!
      subroutine ESM_SetRunSequence(gcomp, rc)
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
      integer :: i, j, maxdiv, runid
      character(ESMF_MAXSTR) :: cname
!
      type(ESMF_Time) :: startTime
      type(ESMF_Time) :: stopTime
      type(ESMF_TimeInterval) :: timeStep
      type(ESMF_Clock) :: internalClock
!
      rc = ESMF_SUCCESS
!  
!-----------------------------------------------------------------------
!     Replace default RunSequence
!     - single RunSequence for coupling. In this case, all components 
!       interact with same interval (coupling time step).
!     - multiple RunSequence for coupling. In this case, multiple slots
!       are created to support different coupling time step among the
!       model components. 
!-----------------------------------------------------------------------
!
      runid = 0
      do i = 1, nModels
        do j = 1, nModels
          if (connectors(i,j)%modActive) then
            runid = runid+10**(nModels-j)
          end if 
        end do
      end do 
!
      if (runid == 1100) then      ! ATM-OCN
        call NUOPC_DriverNewRunSequence(gcomp, slotCount=1, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
           line=__LINE__, file=FILENAME)) return
!
        call NUOPC_DriverAddRunElement(gcomp, slot=1,                   &
                                 srcCompLabel=trim(models(Iatmos)%name),&
                                 dstCompLabel=trim(models(Iocean)%name),&
                                 rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
           line=__LINE__, file=FILENAME)) return
!
        call NUOPC_DriverAddRunElement(gcomp, slot=1,                   &
                                 srcCompLabel=trim(models(Iocean)%name),&
                                 dstCompLabel=trim(models(Iatmos)%name),&
                                 rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
           line=__LINE__, file=FILENAME)) return
!
        call NUOPC_DriverAddRunElement(gcomp, slot=1,                   &
                                    compLabel=trim(models(Iatmos)%name),&
                                    rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
           line=__LINE__, file=FILENAME)) return
!
        call NUOPC_DriverAddRunElement(gcomp, slot=1,                   &
                                    compLabel=trim(models(Iocean)%name),&
                                    rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
           line=__LINE__, file=FILENAME)) return
!
      else if (runid == 1001) then ! ATM-WAV
        call NUOPC_DriverNewRunSequence(gcomp, slotCount=1, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
           line=__LINE__, file=FILENAME)) return
!
        call NUOPC_DriverAddRunElement(gcomp, slot=1,                   &
                                 srcCompLabel=trim(models(Iatmos)%name),&
                                 dstCompLabel=trim(models(Iwavee)%name),&
                                 rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
           line=__LINE__, file=FILENAME)) return
!
        call NUOPC_DriverAddRunElement(gcomp, slot=1,                   &
                                 srcCompLabel=trim(models(Iwavee)%name),&
                                 dstCompLabel=trim(models(Iatmos)%name),&
                                 rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
           line=__LINE__, file=FILENAME)) return
!
        call NUOPC_DriverAddRunElement(gcomp, slot=1,                   &
                                    compLabel=trim(models(Iatmos)%name),&
                                    rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
           line=__LINE__, file=FILENAME)) return
!
        call NUOPC_DriverAddRunElement(gcomp, slot=1,                   &
                                    compLabel=trim(models(Iwavee)%name),&
                                    rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
           line=__LINE__, file=FILENAME)) return
!
      else if (runid == 1210) then ! ATM-OCN-RTM
        call NUOPC_DriverNewRunSequence(gcomp, slotCount=2, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
           line=__LINE__, file=FILENAME)) return
!
        call NUOPC_DriverAddRunElement(gcomp, slot=1,                   &
                                 srcCompLabel=trim(models(Iatmos)%name),&
                                 dstCompLabel=trim(models(Iriver)%name),&
                                 rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
           line=__LINE__, file=FILENAME)) return
!
        call NUOPC_DriverAddRunElement(gcomp, slot=1,                   &
                                 srcCompLabel=trim(models(Iriver)%name),&
                                 dstCompLabel=trim(models(Iocean)%name),&
                                 rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
           line=__LINE__, file=FILENAME)) return
!
        call NUOPC_DriverAddRunElement(gcomp, slot=1,                   &
                                       linkSlot=2, rc=rc) 
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
           line=__LINE__, file=FILENAME)) return
!
        call NUOPC_DriverAddRunElement(gcomp, slot=1,                   &
                                    compLabel=trim(models(Iriver)%name),&
                                    rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
           line=__LINE__, file=FILENAME)) return
!
        call NUOPC_DriverAddRunElement(gcomp, slot=2,                   &
                                 srcCompLabel=trim(models(Iatmos)%name),&
                                 dstCompLabel=trim(models(Iocean)%name),&
                                 rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
           line=__LINE__, file=FILENAME)) return
!
        call NUOPC_DriverAddRunElement(gcomp, slot=2,                   &
                                 srcCompLabel=trim(models(Iocean)%name),&
                                 dstCompLabel=trim(models(Iatmos)%name),&
                                 rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
           line=__LINE__, file=FILENAME)) return
!
        call NUOPC_DriverAddRunElement(gcomp, slot=2,                   &
                                    compLabel=trim(models(Iatmos)%name),&
                                    rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
           line=__LINE__, file=FILENAME)) return
!
        call NUOPC_DriverAddRunElement(gcomp, slot=2,                   &
                                    compLabel=trim(models(Iocean)%name),&
                                    rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
           line=__LINE__, file=FILENAME)) return
!
        call ESMF_GridCompGet(gcomp, clock=internalClock, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
           line=__LINE__, file=FILENAME)) return
!
        call ESMF_ClockGet(internalClock, timeStep=timeStep,            &
                           startTime=startTime, stopTime=stopTime,      &
                           rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
           line=__LINE__, file=FILENAME)) return
!
        maxdiv = max(connectors(Iatmos,Iocean)%divDT,                   &
                     connectors(Iocean,Iatmos)%divDT) 
        cname = trim(connectors(Iatmos,Iocean)%name)//'_clock'
!
        internalClock = ESMF_ClockCreate(name=trim(cname),              &
                                         timeStep=timeStep/maxdiv,      &
                                         startTime=startTime,           &
                                         stopTime=stopTime,             &
                                         rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
           line=__LINE__, file=FILENAME)) return
!
        call NUOPC_DriverSetRunSequence(gcomp, slot=2,                  &
                                        clock=internalClock, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
           line=__LINE__, file=FILENAME)) return
!
      else if (runid == 2211) then ! ATM-OCN-RTM-WAV
        call NUOPC_DriverNewRunSequence(gcomp, slotCount=2, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
           line=__LINE__, file=FILENAME)) return
!
        call NUOPC_DriverAddRunElement(gcomp, slot=1,                   &
                                 srcCompLabel=trim(models(Iatmos)%name),&
                                 dstCompLabel=trim(models(Iriver)%name),&
                                 rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
           line=__LINE__, file=FILENAME)) return
!
        call NUOPC_DriverAddRunElement(gcomp, slot=1,                   &
                                 srcCompLabel=trim(models(Iriver)%name),&
                                 dstCompLabel=trim(models(Iocean)%name),&
                                 rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
           line=__LINE__, file=FILENAME)) return
!
        call NUOPC_DriverAddRunElement(gcomp, slot=1,                   &
                                       linkSlot=2, rc=rc) 
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
           line=__LINE__, file=FILENAME)) return
!
        call NUOPC_DriverAddRunElement(gcomp, slot=1,                   &
                                    compLabel=trim(models(Iriver)%name),&
                                    rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
           line=__LINE__, file=FILENAME)) return
!
        call NUOPC_DriverAddRunElement(gcomp, slot=2,                   &
                                 srcCompLabel=trim(models(Iatmos)%name),&
                                 dstCompLabel=trim(models(Iocean)%name),&
                                 rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
           line=__LINE__, file=FILENAME)) return
!
        call NUOPC_DriverAddRunElement(gcomp, slot=2,                   &
                                 srcCompLabel=trim(models(Iatmos)%name),&
                                 dstCompLabel=trim(models(Iwavee)%name),&
                                 rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
           line=__LINE__, file=FILENAME)) return
!
        call NUOPC_DriverAddRunElement(gcomp, slot=2,                   &
                                 srcCompLabel=trim(models(Iocean)%name),&
                                 dstCompLabel=trim(models(Iatmos)%name),&
                                 rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
           line=__LINE__, file=FILENAME)) return
!
!        call NUOPC_DriverAddRunElement(gcomp, slot=2,                   &
!                                 srcCompLabel=trim(models(Iocean)%name),&
!                                 dstCompLabel=trim(models(Iwavee)%name),&
!                                 rc=rc)
!        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
!           line=__LINE__, file=FILENAME)) return
!
        call NUOPC_DriverAddRunElement(gcomp, slot=2,                   &
                                 srcCompLabel=trim(models(Iwavee)%name),&
                                 dstCompLabel=trim(models(Iatmos)%name),&
                                 rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
           line=__LINE__, file=FILENAME)) return
!
!        call NUOPC_DriverAddRunElement(gcomp, slot=2,                   &
!                                 srcCompLabel=trim(models(Iwavee)%name),&
!                                 dstCompLabel=trim(models(Iocean)%name),&
!                                 rc=rc)
!        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
!           line=__LINE__, file=FILENAME)) return
!
        call NUOPC_DriverAddRunElement(gcomp, slot=2,                   &
                                    compLabel=trim(models(Iatmos)%name),&
                                    rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
           line=__LINE__, file=FILENAME)) return
!
        call NUOPC_DriverAddRunElement(gcomp, slot=2,                   &
                                    compLabel=trim(models(Iocean)%name),&
                                    rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
           line=__LINE__, file=FILENAME)) return
!
        call NUOPC_DriverAddRunElement(gcomp, slot=2,                   &
                                    compLabel=trim(models(Iwavee)%name),&
                                    rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
           line=__LINE__, file=FILENAME)) return
!
        call ESMF_GridCompGet(gcomp, clock=internalClock, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
           line=__LINE__, file=FILENAME)) return
!
        call ESMF_ClockGet(internalClock, timeStep=timeStep,            &
                           startTime=startTime, stopTime=stopTime,      &
                           rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
           line=__LINE__, file=FILENAME)) return
!
        maxdiv = max(connectors(Iatmos,Iocean)%divDT,                   &
                     connectors(Iocean,Iatmos)%divDT) 
        cname = trim(connectors(Iatmos,Iocean)%name)//'_clock'
!
        internalClock = ESMF_ClockCreate(name=trim(cname),              &
                                         timeStep=timeStep/maxdiv,      &
                                         startTime=startTime,           &
                                         stopTime=stopTime,             &
                                         rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,&
           line=__LINE__, file=FILENAME)) return
!
        call NUOPC_DriverSetRunSequence(gcomp, slot=2,                  &
                                        clock=internalClock, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
           line=__LINE__, file=FILENAME)) return
      end if
!
!-----------------------------------------------------------------------
!     Print internal gcomp information
!-----------------------------------------------------------------------
!
      if (debugLevel > 1) then
        call NUOPC_DriverPrint(gcomp, orderflag=.true.)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
           line=__LINE__, file=FILENAME)) return
      end if
!
      end subroutine ESM_SetRunSequence
!
      end module mod_esmf_esm
