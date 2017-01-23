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
#define FILENAME "mod_esmf_cpl.F90"
!
!-----------------------------------------------------------------------
!     CPL gridded component code 
!-----------------------------------------------------------------------
!
      module mod_esmf_cpl
!
!-----------------------------------------------------------------------
!     Used module declarations 
!-----------------------------------------------------------------------
!
      use ESMF
      use NUOPC
      use NUOPC_Connector, only :                                       &
          NUOPC_SetServices     => SetServices,                         &
          NUOPC_Label_ComputeRH => label_ComputeRouteHandle,            &
          NUOPC_Label_ExecuteRH => label_ExecuteRouteHandle,            &
          NUOPC_Label_ReleaseRH => label_ReleaseRouteHandle,            &
          NUOPC_ConnectorGet, NUOPC_ConnectorSet
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
      public :: CPL_SetServices
!
      contains
!
      subroutine CPL_SetServices(ccomp, rc)
      implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
      type(ESMF_CplComp) :: ccomp
      integer, intent(out) :: rc
!
      rc = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!     Register generic methods 
!-----------------------------------------------------------------------
!
      call NUOPC_CompDerive(ccomp, NUOPC_SetServices, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Attach specializing methods 
!-----------------------------------------------------------------------
!
      call NUOPC_CompSpecialize(ccomp, specLabel=NUOPC_Label_ComputeRH, &
                                specRoutine=CPL_ComputeRH, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      call NUOPC_CompSpecialize(ccomp, specLabel=NUOPC_Label_ExecuteRH, &
                                specRoutine=CPL_ExecuteRH, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      call NUOPC_CompSpecialize(ccomp, specLabel=NUOPC_Label_ReleaseRH, &
                                specRoutine=CPL_ReleaseRH, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      end subroutine CPL_SetServices
!
      subroutine CPL_ComputeRH(ccomp, rc)
      implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
      type(ESMF_CplComp) :: ccomp
      integer, intent(out) :: rc
!     
!-----------------------------------------------------------------------
!     Local variable declarations 
!-----------------------------------------------------------------------
!
      logical :: enableExtp, coprocActive, rhExist, rh1Exist, rh2Exist
      integer :: i, j, localPet, petCount, localDECount
      integer :: iSrc, iDst, idSrc, idDst, itSrc, itDst, grSrc, grDst
      integer :: srcCount, dstCount, itemCount, srcTermProcessing
      integer :: srcMaskVal, dstMaskVal
      integer(ESMF_KIND_I4), allocatable, dimension(:,:) :: tlw, tuw
      character(ESMF_MAXSTR) :: ofile
      character(ESMF_MAXSTR) :: cname, fname, rname, msgString
      character(ESMF_MAXSTR), pointer :: cplList(:)
      character(ESMF_MAXSTR), pointer :: srcList(:), dstList(:)
!
      type(ESMF_VM) :: vm
      type(ESMF_State) :: state
      type(ESMF_DistGrid) :: distgrid 
      type(ESMF_Grid) :: srcGrid, dstGrid
      type(ESMF_StaggerLoc) :: srcSLoc, dstSLoc
      type(ESMF_UnmappedAction_Flag) :: unmap
      type(ESMF_RegridMethod_Flag) :: regridMethod
      type(ESMF_RouteHandle) :: routeHandle
      type(ESMF_Field) :: srcField, dstField, tmpField
      type(ESMF_FieldBundle) :: dstFields, srcFields
!
      rc = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!     Query coupler component
!-----------------------------------------------------------------------
!
      call ESMF_CplCompGet(ccomp, name=cname, vm=vm, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      do i = 1, nModels
        do j = 1, nModels
          if (connectors(i,j)%modActive .and.                           &
              trim(connectors(i,j)%name) == trim(cname)) then       
            iSrc = i
            iDst = j
          end if
        end do
      end do
!
      enableExtp = connectors(iSrc,iDst)%modExtrapolation
!
!-----------------------------------------------------------------------
!     Interacting with co-processing component or not? 
!-----------------------------------------------------------------------
!
      coprocActive = .false.
      if (iDst == Icopro) then
        coprocActive = .true.
      end if
!
!-----------------------------------------------------------------------
!     Exchange land-sea mask information 
!-----------------------------------------------------------------------
!
      call UTIL_VMGlobalBroadcast(models(iSrc)%isLand,                  &
                                  models(iSrc)%petList(1), rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      call UTIL_VMGlobalBroadcast(models(iDst)%isLand,                  &
                                  models(iDst)%petList(1), rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      call UTIL_VMGlobalBroadcast(models(iSrc)%isOcean,                 &
                                  models(iSrc)%petList(1), rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      call UTIL_VMGlobalBroadcast(models(iDst)%isOcean,                 &
                                  models(iDst)%petList(1), rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Set source and destination masks for connector 
!-----------------------------------------------------------------------
!
      if (connectors(iSrc,iDst)%modInteraction == Ioverocn) then
        srcMaskVal = models(iSrc)%isLand
        dstMaskVal = models(iDst)%isLand
      else if (connectors(iSrc,iDst)%modInteraction == Ioverlnd) then
        srcMaskVal = models(iSrc)%isOcean
        dstMaskVal = models(iDst)%isOcean
      end if
!
!-----------------------------------------------------------------------
!     Get size of source and destination field list
!-----------------------------------------------------------------------
!
      call NUOPC_ConnectorGet(ccomp, srcFields=srcFields,               &
                              dstFields=dstFields, state=state, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      call ESMF_FieldBundleGet(srcFields, fieldCount=srcCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      call ESMF_FieldBundleGet(dstFields, fieldCount=dstCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Get source and destination fields
!-----------------------------------------------------------------------
!
      if (srcCount == dstCount .and. dstCount > 0) then
!
!-----------------------------------------------------------------------
!     Allocate list arrays and routehandle 
!-----------------------------------------------------------------------
!
      allocate(srcList(srcCount))
      allocate(dstList(dstCount))
!
!-----------------------------------------------------------------------
!     Query field lists
!-----------------------------------------------------------------------
!
      call ESMF_FieldBundleGet(srcFields, fieldNameList=srcList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      call ESMF_FieldBundleGet(dstFields, fieldNameList=dstList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Loop over exchange fields 
!-----------------------------------------------------------------------
!
      do i = 1, srcCount
!
!-----------------------------------------------------------------------
!     Get source and destination field index 
!-----------------------------------------------------------------------
!
      idSrc = get_varid(models(iSrc)%exportField, srcList(i))
      idDst = get_varid(models(iDst)%importField, dstList(i))
!
!-----------------------------------------------------------------------
!     Get interpolation type 
!-----------------------------------------------------------------------
!
      itSrc = models(iSrc)%exportField(idSrc)%itype
      itDst = models(iDst)%importField(idDst)%itype
!
      if (itSrc /= itDst) then
        write(msgString,'(a)') trim(cname)//                            &
              ': src and dst field interpolation type does not match!'
        call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_ERROR)
        return
      end if
!
!-----------------------------------------------------------------------
!     Get grid type 
!-----------------------------------------------------------------------
!
      grSrc = models(iSrc)%exportField(idSrc)%gtype
      grDst = models(iDst)%importField(idDst)%gtype
!
!-----------------------------------------------------------------------
!     Get source and destination field bundle
!-----------------------------------------------------------------------
!
      fname = trim(models(iSrc)%exportField(idSrc)%short_name)
!
      call ESMF_FieldBundleGet(srcFields, trim(fname),                  &
                               field=srcField, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      call ESMF_FieldBundleGet(dstFields, trim(fname),                  &
                               field=dstField, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Get source and destination field
!-----------------------------------------------------------------------
!
      call ESMF_FieldGet(srcField, grid=srcGrid,                        &
                         staggerloc=srcSLoc, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      call ESMF_FieldGet(dstField, grid=dstGrid,                        &
                         staggerloc=dstSLoc, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
         line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     If co-processing is active, then create routehandle just for
!     field redistribution without regridding 
!-----------------------------------------------------------------------
!
      if (coprocActive) then
!
!-----------------------------------------------------------------------
!     Check routehandle (i.e. rh_ATM-COP_redist)
!-----------------------------------------------------------------------
!
      write(rname, fmt="(A,'_',I1,'d_redist')") trim(cname),            &
            models(iSrc)%exportField(idSrc)%rank
!
      call ESMF_StateGet(state, itemSearch='rh_'//trim(rname),          &
                         itemCount=itemCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
         line=__LINE__, file=FILENAME)) return
!
      rhExist = .true.
      if (itemCount <= 0) rhExist = .false.
!
      if (.not. rhExist) then
!
!-----------------------------------------------------------------------
!     Create routehandle
!-----------------------------------------------------------------------
!
      call ESMF_FieldRedistStore(srcField=srcField,                     &
                                 dstField=dstField,                     &
                                 routeHandle=routeHandle,               &
                                 rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Add name to routehandle    
!-----------------------------------------------------------------------
!
      call ESMF_RouteHandleSet(routeHandle,                             &
                               name='rh_'//trim(rname), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Add 1st routehandle to the state    
!-----------------------------------------------------------------------
!
      call ESMF_StateAdd(state, (/ routeHandle /), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      end if
!
!-----------------------------------------------------------------------
!     Interacting with other earth system model components (ATM, OCN), 
!     co-processing component is not active.
!-----------------------------------------------------------------------
!
      else
!
!-----------------------------------------------------------------------
!     Check for extrapolation option for field?
!-----------------------------------------------------------------------
!
      if (enableExtp) then 
!
!-----------------------------------------------------------------------
!     Create routehandles for two step interpolation 
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!     Check 1st routehandle (i.e. rh_CROSS_DOT_BLIN_ATM-OCN)
!-----------------------------------------------------------------------
!
      rname = trim(GRIDDES(grSrc))//'_'//trim(GRIDDES(grDst))//'_'//    &
              trim(INTPDES(Ibilin))//'_'//trim(cname)//'_ext'
!
      call ESMF_StateGet(state, itemSearch='rh_'//trim(rname),          &
                         itemCount=itemCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
         line=__LINE__, file=FILENAME)) return
!
      rh1Exist = .true.
      if (itemCount <= 0) rh1Exist = .false.
!
      if (.not. rh1Exist) then
!
!-----------------------------------------------------------------------
!     Create 1st routehandle
!-----------------------------------------------------------------------
!
      unmap = ESMF_UNMAPPEDACTION_IGNORE
      regridMethod = ESMF_REGRIDMETHOD_BILINEAR
!
      srcTermProcessing = 0
!
      call ESMF_FieldRegridStore(srcField=srcField,                     &
                                 dstField=dstField,                     &
                                 srcMaskValues=(/srcMaskVal/),          &
                                 dstMaskValues=(/dstMaskVal/),          &
                                 unmappedaction=unmap,                  &
                                 routeHandle=routeHandle,               &
                                 regridmethod=regridMethod,             &
                                 ignoreDegenerate=.true.,               &
                                 srcTermProcessing=srcTermProcessing,   &
                                 rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Add name to 1st routehandle    
!-----------------------------------------------------------------------
!
      call ESMF_RouteHandleSet(routeHandle,                             &
                               name='rh_'//trim(rname), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Add 1st routehandle to the state    
!-----------------------------------------------------------------------
!
      call ESMF_StateAdd(state, (/ routeHandle /), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      end if
!
!-----------------------------------------------------------------------
!     Check 2nd routehandle (i.e. rh_CROSS_DOT_NS2D_ATM-OCN)
!-----------------------------------------------------------------------
!
      rname = trim(GRIDDES(grSrc))//'_'//trim(GRIDDES(grDst))//'_'//    &
              trim(INTPDES(Instod))//'_'//trim(cname)//'_ext'
!
      call ESMF_StateGet(state, itemSearch='rh_'//trim(rname),          &
                         itemCount=itemCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
         line=__LINE__, file=FILENAME)) return
!
      rh2Exist = .true.
      if (itemCount <= 0) rh2Exist = .false.
!
      if (.not. rh2Exist) then
!
!-----------------------------------------------------------------------
!     Create temporary field in destination grid
!-----------------------------------------------------------------------
!
      tmpField = UTIL_FieldCreate(dstField, 'temp_field', ONE_R8, -1,rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Modify grid mask to split masked and unmasked grid cells    
!-----------------------------------------------------------------------
!
      call UTIL_FindUnmapped(srcField, dstField, srcMaskVal,            &
                             dstMaskVal, iSrc, iDst, rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Create 2nd routehandle
!-----------------------------------------------------------------------
!
      unmap = ESMF_UNMAPPEDACTION_IGNORE
      regridMethod = ESMF_REGRIDMETHOD_NEAREST_STOD
!
      srcTermProcessing = 0
!
      call ESMF_FieldRegridStore(srcField=tmpField,                     &
                                 dstField=dstField,                     &
                                 srcMaskValues=(/ dstMaskVal,           &
                                                  UNMAPPED_MASK /),     &
                                 dstMaskValues=(/ dstMaskVal,           &
                                                  MAPPED_MASK /),       &
                                 unmappedaction=unmap,                  &
                                 routeHandle=routeHandle,               &
                                 regridmethod=regridMethod,             &
                                 srcTermProcessing=srcTermProcessing,   &
                                 ignoreDegenerate=.true.,               &
                                 rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Add name to 2nd routehandle    
!-----------------------------------------------------------------------
!
      call ESMF_RouteHandleSet(routeHandle,                             &
                               name='rh_'//trim(rname), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Add 2nd routehandle to the state    
!-----------------------------------------------------------------------
!
      call ESMF_StateAdd(state, (/ routeHandle /), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Delete temporary field    
!-----------------------------------------------------------------------
!
      call ESMF_FieldDestroy(tmpField, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      end if
!
      else
!
!-----------------------------------------------------------------------
!     Create routehandle for one step interpolation    
!-----------------------------------------------------------------------
!
      rname = trim(GRIDDES(grSrc))//'_'//trim(GRIDDES(grDst))//'_'//    &
              trim(INTPDES(itSrc))//'_'//trim(cname)
!
      call ESMF_StateGet(state, itemSearch='rh_'//trim(rname),          &
                         itemCount=itemCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
         line=__LINE__, file=FILENAME)) return
!
      rh1Exist = .true.
      rh2Exist = .false.
      if (itemCount <= 0) rh1Exist = .false.
!
      if (.not. rh1Exist) then
      unmap = ESMF_UNMAPPEDACTION_IGNORE
! 
      if (itSrc == Ibilin) then
        regridMethod = ESMF_REGRIDMETHOD_BILINEAR
      else if (itSrc == Instod) then
        regridMethod = ESMF_REGRIDMETHOD_NEAREST_STOD
      else if (itSrc == Indtos) then
        regridMethod = ESMF_REGRIDMETHOD_NEAREST_DTOS
      else
        write(msgString,'(a)') trim(cname)//': selected '//             &
              'interpolation type is not supported! '//INTPDES(itSrc)
        call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_ERROR)
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      end if
!
      srcTermProcessing = 0
!
      if (iSrc == Iocean) then
      call ESMF_FieldRegridStore(srcField=srcField,                     &
                                 dstField=dstField,                     &
                                 srcMaskValues=(/models(iSrc)%isLand/), &
                                 unmappedaction=unmap,                  &
                                 routeHandle=routeHandle,               &
                                 regridmethod=regridMethod,             &
                                 srcTermProcessing=srcTermProcessing,   &
                                 ignoreDegenerate=.true.,               &
                                 rc=rc)
      else
      call ESMF_FieldRegridStore(srcField=srcField,                     &
                                 dstField=dstField,                     &
                                 unmappedaction=unmap,                  &
                                 routeHandle=routeHandle,               &
                                 regridmethod=regridMethod,             &
                                 srcTermProcessing=srcTermProcessing,   &
                                 ignoreDegenerate=.true.,               &
                                 rc=rc)
      end if
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Add name to routehandle    
!-----------------------------------------------------------------------
!
      call ESMF_RouteHandleSet(routeHandle,                             &
                               name='rh_'//trim(rname), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Add routehandle to the state    
!-----------------------------------------------------------------------
!
      call ESMF_StateAdd(state, (/ routeHandle /), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      end if
!
      end if
!
!-----------------------------------------------------------------------
!     Debug: print out exchange fields    
!-----------------------------------------------------------------------
!
      if ((debugLevel > 0) .and. (localPet == 0)) then
      write(*,40) trim(cname),                                          &
                  trim(models(iSrc)%exportField(idSrc)%short_name),     &
                  trim(GRIDDES(models(iSrc)%exportField(idSrc)%gtype)), &
                  trim(models(iDst)%importField(idDst)%short_name),     &
                  trim(GRIDDES(models(iDst)%importField(idDst)%gtype)), &
                  trim(INTPDES(models(iSrc)%exportField(idSrc)%itype)), &
                  rh1Exist, rh2Exist 
      end if
!
      end if
!
      end do
!
      end if
!
!-----------------------------------------------------------------------
!     Formats 
!-----------------------------------------------------------------------
!
 40   format(A10,': routehandle ',A4,'[',A,'] to ',A4,'[',A,']',        &
             ' >> ',A, ' - ',L1,' - ',L1)
!
      end subroutine CPL_ComputeRH
!
      subroutine CPL_ExecuteRH(ccomp, rc)
      implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
      type(ESMF_CplComp) :: ccomp
      integer, intent(out) :: rc
!     
!-----------------------------------------------------------------------
!     Local variable declarations 
!-----------------------------------------------------------------------
!
      logical :: coprocActive, enableExtp
      real*8 :: src_total, dst_total, rel_error
      integer :: srcValueList(9), dstValueList(9)
      integer :: localPet, petCount, localDECount
      integer :: i, j, srcCount, dstCount
      integer :: iSrc, iDst, idSrc, idDst, itSrc, itDst, grSrc, grDst
      character(ESMF_MAXSTR), pointer :: srcList(:), dstList(:)
      character(ESMF_MAXSTR) :: msgString, cname, fname, rname
!
      type(ESMF_VM) :: vm
      type(ESMF_State) :: state
      type(ESMF_Grid) :: srcGrid, dstGrid
      type(ESMF_RouteHandle) :: routeHandle
      type(ESMF_StaggerLoc) :: srcSLoc, dstSLoc
      type(ESMF_Field) :: srcField, dstField, tmpField
      type(ESMF_FieldBundle) :: dstFields, srcFields
      real(ESMF_KIND_R8), dimension(:,:), pointer :: ptr2d
!
      rc = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!     Query coupler component
!-----------------------------------------------------------------------
!
      call ESMF_CplCompGet(ccomp, name=cname, vm=vm, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      do i = 1, nModels
      do j = 1, nModels
        if (connectors(i,j)%modActive .and.                             &
            trim(connectors(i,j)%name) == trim(cname)) then
          iSrc = i
          iDst = j
        end if
      end do
      end do
!
      enableExtp = connectors(iSrc,iDst)%modExtrapolation
!
!-----------------------------------------------------------------------
!     Interacting with co-processing component or not? 
!-----------------------------------------------------------------------
!
      coprocActive = .false.
      if (iDst == Icopro) then
        coprocActive = .true.
      end if
!
!-----------------------------------------------------------------------
!     Get size of field list
!-----------------------------------------------------------------------
!
      call NUOPC_ConnectorGet(ccomp, srcFields=srcFields,               &
                              dstFields=dstFields, state=state, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      call ESMF_FieldBundleGet(srcFields, fieldCount=srcCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      call ESMF_FieldBundleGet(dstFields, fieldCount=dstCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Get name of fields 
!-----------------------------------------------------------------------
!
      allocate(srcList(srcCount))
      call ESMF_FieldBundleGet(srcFields, fieldNameList=srcList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      allocate(dstList(dstCount))
      call ESMF_FieldBundleGet(dstFields, fieldNameList=dstList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Loop over exchange fields 
!-----------------------------------------------------------------------
!
      do i = 1, srcCount
!
!-----------------------------------------------------------------------
!     Get source and destination field index 
!-----------------------------------------------------------------------
!
      idSrc = get_varid(models(iSrc)%exportField, srcList(i))
      idDst = get_varid(models(iDst)%importField, dstList(i))
!
!-----------------------------------------------------------------------
!     Get interpolation type 
!-----------------------------------------------------------------------
!
      itSrc = models(iSrc)%exportField(idSrc)%itype
      itDst = models(iDst)%importField(idDst)%itype
!
      if (itSrc /= itDst) then
        write(msgString,'(a)') trim(cname)//                            &
              ': src and dst field interpolation type does not match!'
        call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_ERROR)
        return
      end if
!
!-----------------------------------------------------------------------
!     Get grid type 
!-----------------------------------------------------------------------
!
      grSrc = models(iSrc)%exportField(idSrc)%gtype
      grDst = models(iDst)%importField(idDst)%gtype
!
!-----------------------------------------------------------------------
!     Get source and destination field
!-----------------------------------------------------------------------
!
      fname = trim(models(iSrc)%exportField(idSrc)%short_name)
!
      call ESMF_FieldBundleGet(srcFields, trim(fname),                  &
                               field=srcField, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      call ESMF_FieldBundleGet(dstFields, trim(fname),                  &
                               field=dstField, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     If co-processing is active, then create routehandle just for
!     field redistribution without regridding 
!-----------------------------------------------------------------------
!
      if (coprocActive) then
!
!-----------------------------------------------------------------------
!     Perform redistribute
!-----------------------------------------------------------------------
!
      write(rname, fmt="(A,'_',I1,'d_redist')") trim(cname),            &
            models(iSrc)%exportField(idSrc)%rank
!
      call ESMF_StateGet(state, 'rh_'//trim(rname), routeHandle, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
         line=__LINE__, file=FILENAME)) return
!
      call ESMF_FieldRedist(srcField, dstField, routeHandle, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
         line=__LINE__, file=FILENAME)) return
!
!
!-----------------------------------------------------------------------
!     Debug: print out exchange fields    
!-----------------------------------------------------------------------
!
      if ((debugLevel > 0) .and. (localPet == 0)) then
      write(*,80) trim(cname),                                          &
                  trim(models(iSrc)%exportField(idSrc)%short_name),     &
                  trim(GRIDDES(models(iSrc)%exportField(idSrc)%gtype)), &
                  trim(models(iDst)%importField(idDst)%short_name),     &
                  trim(GRIDDES(models(iDst)%importField(idDst)%gtype))
      end if

!
      else
!
!-----------------------------------------------------------------------
!     Perform regrid
!-----------------------------------------------------------------------
!
      if (enableExtp) then
!
!-----------------------------------------------------------------------
!     Perform regrid with extrapolation support
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!     Get 1st routehandle from state 
!-----------------------------------------------------------------------
!
      rname = trim(GRIDDES(grSrc))//'_'//trim(GRIDDES(grDst))//'_'//    &
              trim(INTPDES(itSrc))//'_'//trim(cname)//'_ext'
!
      call ESMF_StateGet(state, 'rh_'//trim(rname), routeHandle, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Create temporary field in destination grid
!-----------------------------------------------------------------------
!
      tmpField = UTIL_FieldCreate(dstField, fname, MISSING_R8, -1, rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Perform 1st regrid operation
!-----------------------------------------------------------------------
!
      call ESMF_FieldRegrid(srcField, tmpField, routeHandle,            &
                            zeroregion=ESMF_REGION_SELECT,              &
                            termorderflag=ESMF_TERMORDER_SRCSEQ,        &
                            rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Copy content from temporary field to destination field 
!-----------------------------------------------------------------------
!
      call ESMF_FieldCopy(dstField, tmpField, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Get 2nd routehandle from state 
!-----------------------------------------------------------------------
!
      rname = trim(GRIDDES(grSrc))//'_'//trim(GRIDDES(grDst))//'_'//    &
              'NS2D_'//trim(cname)//'_ext'
!
      call ESMF_StateGet(state, 'rh_'//trim(rname), routeHandle, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Perform 2nd regrid operation to fill unmapped grid points
!-----------------------------------------------------------------------
!
      call ESMF_FieldRegrid(tmpField, dstField, routeHandle,            &
                            zeroregion=ESMF_REGION_SELECT,              &
                            termorderflag=ESMF_TERMORDER_SRCSEQ,        &
                            rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      call ESMF_FieldDestroy(tmpField, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Check: integral adjustment is activated or not for the field 
!-----------------------------------------------------------------------
!
      if (models(iSrc)%exportField(idSrc)%enable_integral_adj) then
!
!-----------------------------------------------------------------------
!     Calculate integral 
!-----------------------------------------------------------------------
!
      src_total = ZERO_R8
      src_total = UTIL_CalcIntegral(vm, srcField,                       &
                                   (/ UNMAPPED_MASK, MAPPED_MASK /), rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      if (localPet == 0) then
        write(*,70) localPet, 'SRC. INTEGRAL', src_total,               &
                    trim(models(iSrc)%exportField(idSrc)%short_name) 
      end if
!
      dst_total = ZERO_R8
      dst_total = UTIL_CalcIntegral(vm, dstField,                       &
                                   (/ UNMAPPED_MASK, MAPPED_MASK /), rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      if (localPet == 0) then
        write(*,70) localPet, 'DST. INTEGRAL', dst_total,               &
                    trim(models(iSrc)%exportField(idSrc)%short_name)
        rel_error = 0.0d0
        if (src_total /= 0.0d0) then
          rel_error = (dst_total-src_total)/src_total
        end if
        write(*,70) localPet, 'RELATIVE ERROR 1', rel_error,            &
                    trim(models(iSrc)%exportField(idSrc)%short_name) 
      end if
!
!-----------------------------------------------------------------------
!     Adjust destination field based on calculated integral 
!-----------------------------------------------------------------------
!
      call UTIL_AdjustField(vm, dstField,                               &
                            (/ UNMAPPED_MASK, MAPPED_MASK /),           &
                            dst_total-src_total, rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      dst_total = ZERO_R8
      dst_total = UTIL_CalcIntegral(vm, dstField,                       &
                                   (/ UNMAPPED_MASK, MAPPED_MASK /), rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      if (localPet == 0) then
        write(*,70) localPet, 'DST. INTEGRAL (CORR)', dst_total,        &
                    trim(models(iSrc)%exportField(idSrc)%short_name)
        rel_error = 0.0d0
        if (src_total /= 0.0d0) then
          rel_error = (dst_total-src_total)/src_total
        end if
        write(*,70) localPet, 'RELATIVE ERROR 2', rel_error,            &
                    trim(models(iSrc)%exportField(idSrc)%short_name)
      end if
      end if
!
      else
!
!-----------------------------------------------------------------------
!     Perform regrid without extrapolation support
!-----------------------------------------------------------------------
!
      rname = trim(GRIDDES(grSrc))//'_'//trim(GRIDDES(grDst))//'_'//    &
              trim(INTPDES(itSrc))//'_'//trim(cname)
!
      call ESMF_StateGet(state, 'rh_'//trim(rname), routeHandle, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      call ESMF_FieldRegrid(srcField, dstField, routeHandle,            &
                            zeroregion=ESMF_REGION_SELECT,              &
                            termorderflag=ESMF_TERMORDER_SRCSEQ,        &
                            rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      end if
!
!-----------------------------------------------------------------------
!     Debug: write data to disk   
!-----------------------------------------------------------------------
!
      if (debugLevel > 4) then
      call ESMF_FieldWrite(srcField, 'src_data_'//trim(cname)//'.nc',   &
                           variableName='src_data', overwrite=.true.,   &
                           rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      call ESMF_FieldWrite(dstField, 'dst_data_'//trim(cname)//'.nc',   &
                           variableName='dst_data', overwrite=.true.,   &
                           rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
      end if
!
!-----------------------------------------------------------------------
!     Debug: print out exchange fields    
!-----------------------------------------------------------------------
!
      if ((debugLevel > 0) .and. (localPet == 0)) then
      write(*,60) trim(cname),                                          &
                  trim(models(iSrc)%exportField(idSrc)%short_name),     &
                  trim(GRIDDES(models(iSrc)%exportField(idSrc)%gtype)), &
                  trim(models(iDst)%importField(idDst)%short_name),     &
                  trim(GRIDDES(models(iDst)%importField(idDst)%gtype)), &
                  trim(INTPDES(models(iSrc)%exportField(idSrc)%itype))
      end if
!
!-----------------------------------------------------------------------
!     Debug: print out import/export fields time stamp info    
!-----------------------------------------------------------------------
!
      if ((debugLevel > 0) .and. (localPet == 0)) then
        call ESMF_AttributeGet(srcField, name="TimeStamp",              &
                               valueList=srcValueList,                  &
                               convention="NUOPC", purpose="Instance",  &
                               rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
            line=__LINE__, file=FILENAME)) return
!
        call ESMF_AttributeGet(dstField, name="TimeStamp",              &
                               valueList=dstValueList,                  &
                               convention="NUOPC", purpose="Instance",  &
                               rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
            line=__LINE__, file=FILENAME)) return
!
        write(*,50) trim(cname),                                        &
                    trim(models(iSrc)%exportField(idSrc)%short_name),   &
                    srcValueList(1), srcValueList(2), srcValueList(3),  &
                    srcValueList(4), srcValueList(5),                   &
                    trim(models(iDst)%importField(idDst)%short_name),   &
                    dstValueList(1), dstValueList(2), dstValueList(3),  &
                    dstValueList(4), dstValueList(5)
      end if
!
      end if
!
      end do
!
!-----------------------------------------------------------------------
!     Deallocate temporary arrays
!-----------------------------------------------------------------------
!
      deallocate(srcList)
      deallocate(dstList)
!
!-----------------------------------------------------------------------
!     Formats 
!-----------------------------------------------------------------------
!
 50   format(A10,': tstamp ',A4,' [',I4,'-',I2.2,'-',                   &
             I2.2,'_',I2.2,'_',I2.2,'] to ',A4,' [',I4,'-',I2.2,'-',    &
             I2.2,'_',I2.2,'_',I2.2,']')
 60   format(A10,': regrid ',A4,' [',A,'] to ',A4,' [',A,']',' >> ',A)
 70   format(" PET(",I3.3,") - ",A," = ",E14.5," (",A,")")
 80   format(A10,': redist ',A4,' [',A,'] to ',A4,' [',A,']')
!
      end subroutine CPL_ExecuteRH
!
      subroutine CPL_ReleaseRH(ccomp, rc)
      implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
      type(ESMF_CplComp) :: ccomp
      integer, intent(out) :: rc
!     
!-----------------------------------------------------------------------
!     Local variable declarations 
!-----------------------------------------------------------------------
!
      logical :: enableExtp, rhExist, rh1Exist, rh2Exist
      integer :: i, j, localPet, petCount
      integer :: itemCount, srcCount, dstCount
      integer :: iSrc, iDst, idSrc, idDst, itSrc, itDst, grSrc, grDst
      character(ESMF_MAXSTR), pointer :: srcList(:), dstList(:)
      character(ESMF_MAXSTR) :: cname, rname
!
      type(ESMF_VM) :: vm
      type(ESMF_State) :: state
      type(ESMF_FieldBundle) :: srcFields, dstFields
      type(ESMF_RouteHandle) :: routeHandle
!
      rc = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!     Query coupler component
!-----------------------------------------------------------------------
!
      call ESMF_CplCompGet(ccomp, name=cname, vm=vm, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      do i = 1, nModels
      do j = 1, nModels
        if (connectors(i,j)%modActive .and.                             &
            trim(connectors(i,j)%name) == trim(cname)) then
          iSrc = i
          iDst = j
        end if
      end do
      end do
!
      enableExtp = connectors(iSrc,iDst)%modExtrapolation
!
!-----------------------------------------------------------------------
!     Get size of field list
!-----------------------------------------------------------------------
!
      call NUOPC_ConnectorGet(ccomp, srcFields=srcFields,               &
                              dstFields=dstFields, state=state, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      call ESMF_FieldBundleGet(srcFields, fieldCount=srcCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      call ESMF_FieldBundleGet(dstFields, fieldCount=dstCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Allocate list arrays and routehandle 
!-----------------------------------------------------------------------
!
      allocate(srcList(srcCount))
      allocate(dstList(dstCount))
!
!-----------------------------------------------------------------------
!     Query field lists
!-----------------------------------------------------------------------
!
      call ESMF_FieldBundleGet(srcFields, fieldNameList=srcList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      call ESMF_FieldBundleGet(dstFields, fieldNameList=dstList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Loop over exchange fields 
!-----------------------------------------------------------------------
!
      do i = 1, srcCount
!
!-----------------------------------------------------------------------
!     Get source and destination field index 
!-----------------------------------------------------------------------
!
      idSrc = get_varid(models(iSrc)%exportField, srcList(i))
      idDst = get_varid(models(iDst)%importField, dstList(i))
!
!-----------------------------------------------------------------------
!     Get interpolation type 
!-----------------------------------------------------------------------
!
      itSrc = models(iSrc)%exportField(idSrc)%itype
      itDst = models(iDst)%importField(idDst)%itype
!
!-----------------------------------------------------------------------
!     Get grid type 
!-----------------------------------------------------------------------
!
      grSrc = models(iSrc)%exportField(idSrc)%gtype
      grDst = models(iDst)%importField(idDst)%gtype
!
!-----------------------------------------------------------------------
!     Check for extrapolation option for field?
!-----------------------------------------------------------------------
!
      if (enableExtp) then 
!
!-----------------------------------------------------------------------
!     Check 1st routehandle (i.e. rh_CROSS_DOT_BLIN_ATM-OCN)
!-----------------------------------------------------------------------
!
      rname = trim(GRIDDES(grSrc))//'_'//trim(GRIDDES(grDst))//'_'//    &
              trim(INTPDES(Ibilin))//'_'//trim(cname)//'_ext'
!
      call ESMF_StateGet(state, itemSearch='rh_'//trim(rname),          &
                         itemCount=itemCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
         line=__LINE__, file=FILENAME)) return
!
      rh1Exist = .true.
      if (itemCount <= 0) rh1Exist = .false.
!
!-----------------------------------------------------------------------
!     Release 1st routehandle
!-----------------------------------------------------------------------
!
      if (rh1Exist) then
        call ESMF_StateGet(state, 'rh_'//trim(rname),                   &
                           routeHandle, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
            line=__LINE__, file=FILENAME)) return
!
        call ESMF_FieldBundleRegridRelease(routeHandle, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
            line=__LINE__, file=FILENAME)) return
      end if
!
!-----------------------------------------------------------------------
!     Check 2nd routehandle (i.e. rh_CROSS_DOT_NS2D_ATM-OCN)
!-----------------------------------------------------------------------
!
      rname = trim(GRIDDES(grSrc))//'_'//trim(GRIDDES(grDst))//'_'//    &
              trim(INTPDES(Instod))//'_'//trim(cname)//'_ext'
!
      call ESMF_StateGet(state, itemSearch='rh_'//trim(rname),          &
                         itemCount=itemCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
         line=__LINE__, file=FILENAME)) return
!
      rh2Exist = .true.
      if (itemCount <= 0) rh2Exist = .false.
!
!-----------------------------------------------------------------------
!     Release 2nd routehandle
!-----------------------------------------------------------------------
!
      if (rh2Exist) then
        call ESMF_StateGet(state, 'rh_'//trim(rname),                   &
                           routeHandle, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
            line=__LINE__, file=FILENAME)) return
!
        call ESMF_FieldBundleRegridRelease(routeHandle, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
            line=__LINE__, file=FILENAME)) return
      end if
!
      else
!
!-----------------------------------------------------------------------
!     Check routehandle for one step interpolation    
!-----------------------------------------------------------------------
!
      rname = trim(GRIDDES(grSrc))//'_'//trim(GRIDDES(grDst))//'_'//    &
              trim(INTPDES(itSrc))//'_'//trim(cname)
!
      call ESMF_StateGet(state, itemSearch='rh_'//trim(rname),          &
                         itemCount=itemCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
         line=__LINE__, file=FILENAME)) return
!
      rhExist = .true.
      if (itemCount <= 0) rhExist = .false.
!
!-----------------------------------------------------------------------
!     Release routehandle
!-----------------------------------------------------------------------
!
      if (rhExist) then
        call ESMF_StateGet(state, 'rh_'//trim(rname),                   &
                           routeHandle, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
            line=__LINE__, file=FILENAME)) return
!
        call ESMF_FieldBundleRegridRelease(routeHandle, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
            line=__LINE__, file=FILENAME)) return
      end if
!
      end if
      end do
!
      end subroutine CPL_ReleaseRH
!
      end module mod_esmf_cpl
