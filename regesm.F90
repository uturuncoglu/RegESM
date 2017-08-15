!=======================================================================
! Regional Earth System Model (RegESM)
! Copyright (c) 2013-2017 Ufuk Turuncoglu
! Licensed under the MIT License.
!=======================================================================
#define FILENAME "regesm.F90"
!
!-----------------------------------------------------------------------
!     RegESM (REGional Earth System Model) Application
!-----------------------------------------------------------------------
!
      program regesm
!
!-----------------------------------------------------------------------
!     Used module declarations 
!-----------------------------------------------------------------------
!
      use ESMF
!
      use mod_config
      use mod_esmf_esm, only : ESM_SetServices 
!
      implicit none
!
!-----------------------------------------------------------------------
!     Local variable declarations  
!-----------------------------------------------------------------------
!
      integer :: rc, urc
      type(ESMF_GridComp) :: esmComp
      type(ESMF_VM) :: vm
!
!-----------------------------------------------------------------------
!     Initialize ESMF framework
!-----------------------------------------------------------------------
!
      call ESMF_Initialize(logkindflag=ESMF_LOGKIND_MULTI, vm=vm,       &
                           ioUnitLBound=20, ioUnitUBound=1000, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=__FILE__))                                &
          call ESMF_Finalize(endflag=ESMF_END_ABORT)
!
!-----------------------------------------------------------------------
!     Create component 
!-----------------------------------------------------------------------
!
      esmComp = ESMF_GridCompCreate(name="regesm", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=__FILE__))                                &
          call ESMF_Finalize(endflag=ESMF_END_ABORT)
!
!-----------------------------------------------------------------------
!     Read main configuration file 
!-----------------------------------------------------------------------
!
      call read_config(vm, rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=__FILE__))                                &
          call ESMF_Finalize(endflag=ESMF_END_ABORT)
!
!-----------------------------------------------------------------------
!     Add additional fields to NUOPC field dictionary 
!-----------------------------------------------------------------------
!
      call set_field_dir(vm, rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=__FILE__))                                &
          call ESMF_Finalize(endflag=ESMF_END_ABORT)
!
!-----------------------------------------------------------------------
!     Register component 
!-----------------------------------------------------------------------
!
      call ESMF_GridCompSetServices(esmComp,                            &
                                    ESM_SetServices,                    &
                                    userRc=urc,                         &
                                    rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=__FILE__))                                &
          call ESMF_Finalize(endflag=ESMF_END_ABORT)
      if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU,   &
          line=__LINE__, file=__FILE__))                                &
          call ESMF_Finalize(endflag=ESMF_END_ABORT)
!
!-----------------------------------------------------------------------
!     Initialize component 
!-----------------------------------------------------------------------
!
      call ESMF_GridCompInitialize(esmComp, userRc=urc, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=__FILE__))                                &
          call ESMF_Finalize(endflag=ESMF_END_ABORT)
      if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU,   &
          line=__LINE__, file=__FILE__))                                &
          call ESMF_Finalize(endflag=ESMF_END_ABORT)
!
!-----------------------------------------------------------------------
!     Wait for finishing initialize phase
!-----------------------------------------------------------------------
!
      call ESMF_VMBarrier(vm, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=__FILE__))                                &
          call ESMF_Finalize(endflag=ESMF_END_ABORT)
!
!-----------------------------------------------------------------------
!     Run component 
!-----------------------------------------------------------------------
!
      call ESMF_GridCompRun(esmComp, userRc=urc, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=__FILE__))                                &
          call ESMF_Finalize(endflag=ESMF_END_ABORT)
      if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU,   &
          line=__LINE__, file=__FILE__))                                &
          call ESMF_Finalize(endflag=ESMF_END_ABORT)
!
!-----------------------------------------------------------------------
!     Finalize component 
!-----------------------------------------------------------------------
!
      call ESMF_GridCompFinalize(esmComp, userRc=urc, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=__FILE__))                                &
          call ESMF_Finalize(endflag=ESMF_END_ABORT)
      if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU,   &
          line=__LINE__, file=__FILE__))                                &
          call ESMF_Finalize(endflag=ESMF_END_ABORT)
! 
!-----------------------------------------------------------------------
!     Destroy the earth system Component
!-----------------------------------------------------------------------
! 
      call ESMF_GridCompDestroy(esmComp, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=__FILE__)) &
          call ESMF_Finalize(endflag=ESMF_END_ABORT)
! 
!-----------------------------------------------------------------------
!     Finalize ESMF framework 
!-----------------------------------------------------------------------
!
      call ESMF_Finalize(rc=rc)
!
      end program regesm
