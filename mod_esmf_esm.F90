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
      use NUOPC_Driver, only:                                           &
          NUOPC_SetServices            => routine_SetServices,          &
          NUOPC_Type_IS                => type_InternalState,           &
          NUOPC_Label_IS               => label_InternalState,          &
          NUOPC_Label_SetModelCount    => label_SetModelCount,          &
          NUOPC_Label_SetModelPetLists => label_SetModelPetLists,       &
          NUOPC_Label_SetModelServices => label_SetModelServices,       &
          NUOPC_Label_Finalize         => label_Finalize
!
      use mod_types
      use mod_esmf_atm, only: ATM_SetServices
      use mod_esmf_ocn, only: OCN_SetServices
      use mod_esmf_rtm, only: RTM_SetServices
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
      integer, intent(inout) :: rc
!
      rc = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!     Register generic methods 
!-----------------------------------------------------------------------
!
      call NUOPC_SetServices(gcomp, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Attach specializing methods 
!-----------------------------------------------------------------------
!
      call ESMF_MethodAdd(gcomp, label=NUOPC_Label_SetModelCount,       &
                          userRoutine=ESM_SetModelCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      call ESMF_MethodAdd(gcomp, label=NUOPC_Label_SetModelPetLists,    &
                          userRoutine=ESM_SetModelPetLists, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      call ESMF_MethodAdd(gcomp, label=NUOPC_Label_SetModelServices,    &
                          userRoutine=ESM_SetModelServices, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      call ESMF_MethodAdd(gcomp, label=NUOPC_Label_Finalize,            &
                          userRoutine=ESM_Finalize, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      end subroutine ESM_SetServices
!
      subroutine ESM_SetModelCount(gcomp, rc)
      implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
      type(ESMF_GridComp) :: gcomp
      integer, intent(inout) :: rc
!     
!-----------------------------------------------------------------------
!     Local variable declarations 
!-----------------------------------------------------------------------
!
      type(NUOPC_Type_IS) :: genIS
!
      rc = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!     Get internal state 
!-----------------------------------------------------------------------
!
      nullify(genIS%wrap)
      call ESMF_UserCompGetInternalState(gcomp, NUOPC_Label_IS,         &
                                         genIS, rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Set model count
!-----------------------------------------------------------------------
!
      genIS%wrap%modelCount = nModels
      end subroutine ESM_SetModelCount
!
      subroutine ESM_SetModelPetLists(gcomp, rc)
      implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
      type(ESMF_GridComp) :: gcomp
      integer, intent(inout) :: rc
!     
!-----------------------------------------------------------------------
!     Local variable declarations 
!-----------------------------------------------------------------------
!
      integer :: i, j 
      type(NUOPC_Type_IS) :: genIS
!
      rc = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!     Get internal state 
!-----------------------------------------------------------------------
!
      nullify(genIS%wrap)
      call ESMF_UserCompGetInternalState(gcomp, NUOPC_Label_IS,         &
                                         genIS, rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Set PET list for model components
!-----------------------------------------------------------------------
!
      do i = 1, nModels
        if (models(i)%modActive) then
          genIS%wrap%modelPetLists(i)%petList => models(i)%petList(:)
        end if
      end do
!
!-----------------------------------------------------------------------
!     Set PET list for connectors (couplers)
!-----------------------------------------------------------------------
!
      do i = 1, nModels
        do j = 1, nModels
          if (connectors(i,j)%modActive) then
            genIS%wrap%connectorPetLists(i,j)%petList =>                &
                              connectors(i,j)%petList(:)
          end if
        end do
      end do
!
      end subroutine ESM_SetModelPetLists
!
      subroutine ESM_SetModelServices(gcomp, rc)
      implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
      type(ESMF_GridComp) :: gcomp
      integer, intent(inout) :: rc
!     
!-----------------------------------------------------------------------
!     Local variable declarations 
!-----------------------------------------------------------------------
!
      integer :: i, j, maxdiv, urc
      character(len=13) :: cname
      type(ESMF_Clock) :: iclock
      type(NUOPC_Type_IS) :: genIS
!
      rc = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!     Get internal state 
!-----------------------------------------------------------------------
!
      nullify(genIS%wrap)
      call ESMF_UserCompGetInternalState(gcomp, NUOPC_Label_IS,         &
                                         genIS, rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Create gridded components 
!-----------------------------------------------------------------------
!
      do i = 1, nModels
        if (models(i)%modActive) then
          call ESMF_GridCompSet(genIS%wrap%modelComp(i),                &
                                name=models(i)%name, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,&
              line=__LINE__, file=FILENAME)) return
        end if
      end do
!
!-----------------------------------------------------------------------
!     Create connector (coupler) components
!-----------------------------------------------------------------------
!
      do i = 1, nModels
        do j = 1, nModels
          if (connectors(i,j)%modActive) then
            call ESMF_CplCompSet(genIS%wrap%connectorComp(i,j),         &
                                name=connectors(i,j)%name, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc,                        &
                msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=FILENAME))&
                return
          end if
        end do
      end do
!
!-----------------------------------------------------------------------
!     SetServices routine for model components 
!-----------------------------------------------------------------------
!
      do i = 1, nModels
        if (models(i)%modActive) then
          if (i == Iatmos) then
            call ESMF_GridCompSetServices(genIS%wrap%modelComp(i),      &
                                          ATM_SetServices,              &
                                          userRc=urc, rc=rc)
          else if (i == Iocean) then
            call ESMF_GridCompSetServices(genIS%wrap%modelComp(i),      &
                                          OCN_SetServices,              &
                                          userRc=urc, rc=rc)
          else if (i == Iriver) then
            call ESMF_GridCompSetServices(genIS%wrap%modelComp(i),      &
                                          RTM_SetServices,              &
                                          userRc=urc, rc=rc)
          end if
!
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,&
                                 line=__LINE__, file=FILENAME)) return
          if (ESMF_LogFoundError(rcToCheck=urc,msg=ESMF_LOGERR_PASSTHRU,&
                                 line=__LINE__, file=FILENAME)) return
!
          if (debugLevel > 0) then
          call ESMF_AttributeSet(genIS%wrap%modelComp(i),               &
                                 name="Verbosity", value="high",        &
                                 convention="NUOPC", purpose="General", &
                                 rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,&
                                 line=__LINE__, file=FILENAME)) return
          end if
        end if
      end do
!
!-----------------------------------------------------------------------
!     SetServices routine for connector components 
!-----------------------------------------------------------------------
!
      do i = 1, nModels
        do j = 1, nModels
          if (connectors(i,j)%modActive) then
            call  ESMF_CplCompSetServices(genIS%wrap%connectorComp(i,j),&
                                          CPL_SetServices,              &
                                          userRc=urc, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc,                        &
                msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=FILENAME))&
                return
            if (ESMF_LogFoundError(rcToCheck=urc,                       &
                msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=FILENAME))&
                return
!
          if (debugLevel > 0) then
          call ESMF_AttributeSet(genIS%wrap%connectorComp(i,j),         &
                                 name="Verbosity", value="high",        &
                                 convention="NUOPC", purpose="General", &
                                 rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,&
                                 line=__LINE__, file=FILENAME)) return
          end if
          end if
        end do
      end do
!
!-----------------------------------------------------------------------
!     Set internal clock for application (ESM or driver) component 
!     The ESM time step must be set to the longest time interval of
!     the connector components
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
!-----------------------------------------------------------------------
!     Change default run sequence. Currently, two options are supported:
!     - single runSeq for ATM-OCN coupling
!     - multiple (2) runSeq for ATM-OCN-RTM coupling
!       + fast processes (ATM-OCN and OCN-ATM coupling) i.e. 3-hr
!       + slow processes (ATM-RTM and RTM-OCN coupling) i.e. 1-day
!-----------------------------------------------------------------------
!
      call NUOPC_RunSequenceDeallocate(genIS%wrap%runSeq, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!  
!-----------------------------------------------------------------------
!     ATM and OCN model components are activated
!-----------------------------------------------------------------------
!
      if (models(Iatmos)%modActive .and.                                &
          models(Iocean)%modActive .and. .not.                          &
          models(Iriver)%modActive) then
        call NUOPC_RunSequenceAdd(genIS%wrap%runSeq, 1, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
            line=__LINE__, file=FILENAME)) return
!
        do i = 1, nModels
          do j = 1, nModels
            if (connectors(i,j)%modActive) then      
              call NUOPC_RunElementAdd(genIS%wrap%runSeq(1),            &
                                       i=i, j=j, phase=1, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc,                      &
                                     msg=ESMF_LOGERR_PASSTHRU,          &
                                     line=__LINE__,                     &
                                     file=FILENAME)) return
            end if
          end do
        end do
!
        do i = 1, nModels
          if (models(i)%modActive) then
            call NUOPC_RunElementAdd(genIS%wrap%runSeq(1),              &
                                     i=i, j=-1, phase=1, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc,                        &
                                   msg=ESMF_LOGERR_PASSTHRU,&
                                   line=__LINE__, file=FILENAME)) return
          end if
        end do
!
        call NUOPC_RunSequencePrint(genIS%wrap%runSeq(1), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
            line=__LINE__, file=FILENAME)) return
!  
!-----------------------------------------------------------------------
!     ATM, OCN and RTM model components are activated
!     Two runSeq are defined. runSeq(1) is for slow processes and 
!     runSeq(2) is for fast processes (ATM-OCN coupling)
!-----------------------------------------------------------------------
!
      else if (models(Iatmos)%modActive .and.                           &
               models(Iocean)%modActive .and.                           &
               models(Iriver)%modActive) then
        call NUOPC_RunSequenceAdd(genIS%wrap%runSeq, 2, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
            line=__LINE__, file=FILENAME)) return
!
        call NUOPC_RunElementAdd(genIS%wrap%runSeq(1),                  &
                                 i=Iatmos, j=Iriver, phase=1, rc=rc) 
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
            line=__LINE__, file=FILENAME)) return
!
        call NUOPC_RunElementAdd(genIS%wrap%runSeq(1),                  &
                                 i=Iriver, j=Iocean, phase=1, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
            line=__LINE__, file=FILENAME)) return
!
        call NUOPC_RunElementAdd(genIS%wrap%runSeq(1),                  &
                                 i=-2, j=0, phase=1, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
            line=__LINE__, file=FILENAME)) return
!
        call NUOPC_RunElementAdd(genIS%wrap%runSeq(1),                  &
                                 i=Iriver, j=-1, phase=1, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
            line=__LINE__, file=FILENAME)) return
!
        maxdiv = max(connectors(Iatmos,Iocean)%divDT,                   &
                     connectors(Iocean,Iatmos)%divDT) 
        cname = trim(connectors(Iatmos,Iocean)%name)//'_clock'
!
        if (restarted) then
          iclock = ESMF_ClockCreate(esmTimeStep/maxdiv,                 &
                                    esmRestartTime,                     &
                                    stopTime=esmStopTime,               &
                                    name=trim(cname), rc=rc)
        else
          iclock = ESMF_ClockCreate(esmTimeStep/maxdiv,                 &
                                    esmStartTime,                       &
                                    stopTime=esmStopTime,               &
                                    name=trim(cname), rc=rc)
        end if       
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
            line=__LINE__, file=FILENAME)) return
!
        call NUOPC_RunSequenceSet(genIS%wrap%runSeq(2), iclock, rc=rc) 
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
            line=__LINE__, file=FILENAME)) return
!
        call NUOPC_RunElementAdd(genIS%wrap%runSeq(2),                  &
                             i=Iatmos, j=Iocean, phase=1, rc=rc) 
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
            line=__LINE__, file=FILENAME)) return
!
        call NUOPC_RunElementAdd(genIS%wrap%runSeq(2),                  &
                             i=Iocean, j=Iatmos, phase=1, rc=rc) 
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
            line=__LINE__, file=FILENAME)) return
!
        call NUOPC_RunElementAdd(genIS%wrap%runSeq(2),                  &
                             i=Iatmos, j=-1, phase=1, rc=rc) 
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
            line=__LINE__, file=FILENAME)) return
!
        call NUOPC_RunElementAdd(genIS%wrap%runSeq(2),                  &
                             i=Iocean, j=-1, phase=1, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
            line=__LINE__, file=FILENAME)) return
!
        call NUOPC_RunSequencePrint(genIS%wrap%runSeq, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
            line=__LINE__, file=FILENAME)) return
      else
        call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc,              &
             msg='Unknown coupling setup: please activate one of the '//&
             'following options -> ATM-OCN or ATM-OCN-RTM')
        return
      end if
!
      end subroutine ESM_SetModelServices
!
      subroutine ESM_Finalize(gcomp, rc)
      implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
      type(ESMF_GridComp) :: gcomp
      integer, intent(inout) :: rc
!     
!-----------------------------------------------------------------------
!     Local variable declarations 
!-----------------------------------------------------------------------
!
      integer :: urc, stat
      logical :: existflag
      type(NUOPC_Type_IS) :: genIS
!
      rc = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!     Execute finalize routine 
!-----------------------------------------------------------------------
!
      call ESMF_MethodExecute(gcomp, label=NUOPC_Label_Finalize,        &
                              existflag=existflag,                      &
                              userRc=urc, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
      if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU,   &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Get internal state 
!-----------------------------------------------------------------------
!
      nullify(genIS%wrap)
      call ESMF_UserCompGetInternalState(gcomp, NUOPC_Label_IS,         &
                                         genIS, rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Deallocate internal state 
!-----------------------------------------------------------------------
!
      deallocate(genIS%wrap, stat=stat)
      if (ESMF_LogFoundDeallocError(statusToCheck=stat,                 &
          msg="Deallocation of internal state memory failed.",          &
         line=__LINE__, file=__FILE__, rcToReturn=rc)) return
!
      end subroutine ESM_Finalize
!
      end module mod_esmf_esm
