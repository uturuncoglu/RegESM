!=======================================================================
! Regional Earth System Model (RegESM)
! Copyright (c) 2013-2019 Ufuk Turuncoglu
! Licensed under the MIT License.
!=======================================================================
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
      use mod_esmf_cop, only: COP_SetServices
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
          else if (i == Icopro) then
            call NUOPC_DriverAddComp(gcomp, trim(models(i)%name),       &
                                     COP_SetServices,                   &
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
      integer :: i, j, maxdiv, runid, localPet, petCount
      character(ESMF_MAXSTR) :: cname
!
      type(ESMF_VM) :: vm
      type(ESMF_Time) :: startTime
      type(ESMF_Time) :: stopTime
      type(ESMF_TimeInterval) :: timeStep
      type(ESMF_Clock) :: internalClock
!
      rc = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!     Query gridded component
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
      if (localPet == 0) then
        write(*,fmt="(A,I5)") "RUN ID = ", runid
      end if
!  
!-----------------------------------------------------------------------
!     ATM-OCN
!-----------------------------------------------------------------------
!
      if (runid == 11000) then
!  
!-----------------------------------------------------------------------
!     ATM-OCN: Explicit
!-----------------------------------------------------------------------
!
      if (cplType == 1) then
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
!-----------------------------------------------------------------------
!     ATM-OCN: Semi-implicit 
!-----------------------------------------------------------------------
!
      else
        call NUOPC_DriverNewRunSequence(gcomp, slotCount=1, rc=rc)
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
                                 srcCompLabel=trim(models(Iatmos)%name),&
                                 dstCompLabel=trim(models(Iocean)%name),&
                                 rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
           line=__LINE__, file=FILENAME)) return
!
        call NUOPC_DriverAddRunElement(gcomp, slot=1,                   &
                                    compLabel=trim(models(Iocean)%name),&
                                    rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
           line=__LINE__, file=FILENAME)) return
      end if      
!  
!-----------------------------------------------------------------------
!     ATM-WAV 
!-----------------------------------------------------------------------
!
      else if (runid == 10010) then
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
!-----------------------------------------------------------------------
!     ATM-OCN-RTM
!-----------------------------------------------------------------------
!
      else if (runid == 12100) then
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
!-----------------------------------------------------------------------
!     ATM-OCN-RTM-WAV
!-----------------------------------------------------------------------
!
      else if (runid == 22110) then
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
!  
!-----------------------------------------------------------------------
!     ATM-OCN-COP 
!-----------------------------------------------------------------------
!
      else if (runid == 11002) then
        call NUOPC_DriverNewRunSequence(gcomp, slotCount=2, rc=rc)
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
                                       linkSlot=2, rc=rc) 
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
           line=__LINE__, file=FILENAME)) return
!
        call NUOPC_DriverAddRunElement(gcomp, slot=2,                   &
                                 srcCompLabel=trim(models(Iatmos)%name),&
                                 dstCompLabel=trim(models(Icopro)%name),&
                                 rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
           line=__LINE__, file=FILENAME)) return
!
        call NUOPC_DriverAddRunElement(gcomp, slot=2,                   &
                                 srcCompLabel=trim(models(Iocean)%name),&
                                 dstCompLabel=trim(models(Icopro)%name),&
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
        call NUOPC_DriverAddRunElement(gcomp, slot=2,                   &
                                    compLabel=trim(models(Icopro)%name),&
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
                     connectors(Iocean,Iatmos)%divDT,                   &
                     connectors(Iatmos,Icopro)%divDT,                   &
                     connectors(Iocean,Icopro)%divDT) 
        cname = trim(models(Iatmos)%name)//"-TO-"//                     &
                trim(models(Iocean)%name)//"-TO-"//                     &
                trim(models(Icopro)%name)//'_clock'
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
