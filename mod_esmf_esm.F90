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
      integer :: i, j, urc
      type(ESMF_Clock) :: clock
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
!
          if (debugLevel > 1) then
          call ESMF_AttributeSet(genIS%wrap%modelComp(i),               &
                                 name="Verbosity",                      &
                                 value="high",                          &
                                 convention="NUOPC",                    &
                                 purpose="General", rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,&
              line=__LINE__, file=FILENAME)) return
          end if
        end if
      end do
!
!-----------------------------------------------------------------------
!     Create connector (coupled) components
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
!
            if (debugLevel > 1) then
            call ESMF_AttributeSet(genIS%wrap%connectorComp(i,j),       &
                                   name="Verbosity",                    &
                                   value="high",                        &
                                   convention="NUOPC",                  &
                                   purpose="General", rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc,                        &
                msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=FILENAME))&
                return
            end if
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

          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,&
                                 line=__LINE__, file=FILENAME)) return
          if (ESMF_LogFoundError(rcToCheck=urc,msg=ESMF_LOGERR_PASSTHRU,&
                                 line=__LINE__, file=FILENAME)) return
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
          end if
        end do
      end do
!
!-----------------------------------------------------------------------
!     Set internal model clock 
!-----------------------------------------------------------------------
!
      clock = ESMF_ClockCreate(esmTimeStep,                             &
                               esmStartTime,                            &
                               stopTime=esmStopTime,                    &
                               name='esm_clock', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      call ESMF_GridCompSet(gcomp, clock=clock, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!  
!-----------------------------------------------------------------------
!     Change default run sequence 
!-----------------------------------------------------------------------
!
      call NUOPC_RunSequenceDeallocate(genIS%wrap%runSeq, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      call NUOPC_RunSequenceAdd(genIS%wrap%runSeq, 1, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      do i = 1, nModels
      do j = 1, nModels
        if (connectors(i,j)%modActive) then      
          call NUOPC_RunElementAdd(genIS%wrap%runSeq(1),                &
                                   i=i, j=j, phase=1, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,&
                                 line=__LINE__, file=FILENAME)) return
        end if
      end do
      end do
!
      do i = 1, nModels
        if (models(i)%modActive) then
          call NUOPC_RunElementAdd(genIS%wrap%runSeq(1),                &
                                   i=i, j=-1, phase=1, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,&
              line=__LINE__, file=FILENAME)) return
        end if
      end do
!
      call NUOPC_RunSequencePrint(genIS%wrap%runSeq(1))
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
