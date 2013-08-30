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
          NUOPC_SetServices     => routine_SetServices,                 &
          NUOPC_Type_IS         => type_InternalState,                  &
          NUOPC_Label_IS        => label_InternalState,                 &
          NUOPC_Label_ComputeRH => label_ComputeRouteHandle,            &
          NUOPC_Label_ExecuteRH => label_ExecuteRouteHandle,            &
          NUOPC_Label_ReleaseRH => label_ReleaseRouteHandle
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
!-----------------------------------------------------------------------
!     Private module variables 
!-----------------------------------------------------------------------
!
      type(ESMF_FieldBundle) :: savFields
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
      call NUOPC_SetServices(ccomp, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Attach specializing methods 
!-----------------------------------------------------------------------
!
      call ESMF_MethodAdd(ccomp, label=NUOPC_Label_ComputeRH,           &
                          userRoutine=CPL_ComputeRH, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      call ESMF_MethodAdd(ccomp, label=NUOPC_Label_ExecuteRH,           &
                          userRoutine=CPL_ExecuteRH, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      call ESMF_MethodAdd(ccomp, label=NUOPC_Label_ReleaseRH,           &
                          userRoutine=CPL_ReleaseRH, rc=rc)
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
      logical :: flag1, flag2
      integer :: i, j, localPet, petCount
      integer :: iSrc, iDst, idSrc, idDst, itSrc, itDst, grSrc, grDst
      integer :: cplCount, srcCount, dstCount, itemCount
      character(ESMF_MAXSTR) :: cname, fname, rname, msgString
      character(ESMF_MAXSTR), pointer :: cplList(:)
      character(ESMF_MAXSTR), pointer :: srcList(:), dstList(:)
!
      type(ESMF_VM) :: vm
      type(ESMF_Grid) :: srcGrid, dstGrid
      type(ESMF_Field) :: srcFrac, dstFrac
      type(ESMF_ArraySpec) :: srcArrSpec, dstArrSpec
      type(ESMF_StaggerLoc) :: srcSLoc, dstSLoc
      type(ESMF_UnmappedAction_Flag) :: unmap
      type(ESMF_RegridMethod_Flag) :: regridMethod
      type(ESMF_RouteHandle) :: routeHandle
      type(ESMF_Field) :: srcField, dstField, tmpField
!
      type(NUOPC_Type_IS) :: genIS
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
!-----------------------------------------------------------------------
!     Get internal state 
!-----------------------------------------------------------------------
!
      nullify(genIS%wrap)
      call ESMF_UserCompGetInternalState(ccomp, NUOPC_Label_IS,         &
                                         genIS, rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Create empty field bundle to store frac and area fields 
!-----------------------------------------------------------------------
!
      call ESMF_FieldBundleGet(savFields, rc=rc)
      if (rc /= ESMF_SUCCESS) then
        savFields = ESMF_FieldBundleCreate(name='saved_fields', rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
            line=__LINE__, file=FILENAME)) return
      end if
!
!-----------------------------------------------------------------------
!     Get size of source and destination field list
!-----------------------------------------------------------------------
!
      call NUOPC_CplCompAttributeGet(ccomp, cplListSize=cplCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      call ESMF_FieldBundleGet(genIS%wrap%srcFields,                    &
                               fieldCount=srcCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      call ESMF_FieldBundleGet(genIS%wrap%dstFields,                    &
                               fieldCount=dstCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      if (cplCount /= srcCount .or. cplCount /= dstCount) then
        write(msgString,'(a)') trim(cname)//                            &
              ': cplList count does not agree with FieldBundle counts!'
        call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_ERROR)
        return
      end if
!
!-----------------------------------------------------------------------
!     Get source and destination fields
!-----------------------------------------------------------------------
!
      if (cplCount > 0) then
!
!-----------------------------------------------------------------------
!     Allocate list arrays and routehandle 
!-----------------------------------------------------------------------
!
      allocate(cplList(cplCount))
      allocate(srcList(cplCount))
      allocate(dstList(cplCount))
!
!-----------------------------------------------------------------------
!     Query field lists
!-----------------------------------------------------------------------
!
      call NUOPC_CplCompAttributeGet(ccomp, cplList=cplList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      call ESMF_FieldBundleGet(genIS%wrap%srcFields,                    &
                               fieldNameList=srcList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      call ESMF_FieldBundleGet(genIS%wrap%dstFields,                    &
                               fieldNameList=dstList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Loop over exchange fields 
!-----------------------------------------------------------------------
!
      do i = 1, cplCount 
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
      call ESMF_FieldBundleGet(genIS%wrap%srcFields, trim(fname),       &
                               field=srcField, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      call ESMF_FieldBundleGet(genIS%wrap%dstFields, trim(fname),       &
                               field=dstField, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Get source and destination field
!-----------------------------------------------------------------------
!
      call ESMF_FieldGet(srcField, arrayspec=srcArrSpec,                &
                         grid=srcGrid, staggerloc=srcSLoc, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      call ESMF_FieldGet(dstField, arrayspec=dstArrSpec,                &
                         grid=dstGrid, staggerloc=dstSLoc, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
         line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Create temporary field in destination grid
!-----------------------------------------------------------------------
!
      tmpField = ESMF_FieldCreate(dstGrid, dstArrSpec,                  &
                                  staggerloc=dstSLoc, rc=rc) 
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
         line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Create routehandles for two step interpolation 
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!     Check 1st routehandle (i.e. rh_CROSS_DOT_BLIN_ATM-OCN)
!-----------------------------------------------------------------------
!
      rname = trim(GRIDDES(grSrc))//'_'//trim(GRIDDES(grDst))//'_'//    &
              trim(INTPDES(itSrc))//'_'//trim(cname)
!
      call ESMF_StateGet(genIS%wrap%state,itemSearch='rh_'//trim(rname),&
                         itemCount=itemCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
         line=__LINE__, file=FILENAME)) return
!
      flag1 = .false.
      if (itemCount <= 0) flag1 = .true.
!
!-----------------------------------------------------------------------
!     Create 1st routehandle
!-----------------------------------------------------------------------
!
      if (flag1) then
      unmap = ESMF_UNMAPPEDACTION_IGNORE
      regridMethod = ESMF_REGRIDMETHOD_BILINEAR
!
      call ESMF_FieldRegridStore(srcField=srcField,                     &
                                 dstField=dstField,                     &
                                 srcMaskValues=(/models(iSrc)%isLand/), &
                                 dstMaskValues=(/models(iDst)%isLand/), &
                                 unmappedaction=unmap,                  &
                                 routeHandle=routeHandle,               &
                                 regridmethod=regridMethod,             &
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
      call ESMF_StateAdd(genIS%wrap%state, (/ routeHandle /), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
      end if
!
!-----------------------------------------------------------------------
!     Check 2nd routehandle (i.e. rh_CROSS_DOT_NS2D_ATM-OCN)
!-----------------------------------------------------------------------
!
      rname = trim(GRIDDES(grSrc))//'_'//trim(GRIDDES(grDst))//'_'//    &
              'NS2D_'//trim(cname)
!
      call ESMF_StateGet(genIS%wrap%state,itemSearch='rh_'//trim(rname),&
                         itemCount=itemCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
         line=__LINE__, file=FILENAME)) return
!
      flag1 = .false.
      if (itemCount <= 0) flag1 = .true.
!
!-----------------------------------------------------------------------
!     Create 2nd routehandle
!-----------------------------------------------------------------------
!
      if (flag1) then
!
!-----------------------------------------------------------------------
!     Modify grid mask to split masked and unmasked grid cells    
!-----------------------------------------------------------------------
!
      call UTIL_FindUnmapped(srcField, dstField, models(iSrc)%isLand,   &
                             models(iDst)%isLand, rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      unmap = ESMF_UNMAPPEDACTION_IGNORE
      regridMethod = ESMF_REGRIDMETHOD_NEAREST_STOD
!
      call ESMF_FieldRegridStore(srcField=dstField,                     &
                                 dstField=tmpField,                     &
                                 srcMaskValues=(/models(iDst)%isLand, UNMAPPED_MASK /), &
                                 dstMaskValues=(/models(iDst)%isLand, MAPPED_MASK /), &
                                 unmappedaction=unmap,                  &
                                 routeHandle=routeHandle,               &
                                 regridmethod=regridMethod,             &
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
      call ESMF_StateAdd(genIS%wrap%state, (/ routeHandle /), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
      end if
!
!-----------------------------------------------------------------------
!     Check fraction fields (i.e. ff_CROSS_DOT_ATM-OCN_SRC) 
!     The fraction fields are used in the calculation of the integrals
!-----------------------------------------------------------------------
!
      flag2 = .false.
!
      if (models(iSrc)%exportField(idSrc)%enable_integral_adj) then
      fname = trim(GRIDDES(grSrc))//'_'//trim(GRIDDES(grDst))//'_'//    &
              trim(cname)
!
      call ESMF_FieldBundleGet(savFields,                               &
                               fieldName='ff_'//trim(fname)//'_SRC',    &
                               isPresent=flag2, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Get frac fields to calculate integrals 
!-----------------------------------------------------------------------
!
      if (.not. flag2) then
!
!-----------------------------------------------------------------------
!     Get source and destination field
!-----------------------------------------------------------------------
!
      call ESMF_FieldGet(srcField, arrayspec=srcArrSpec,                &
                         grid=srcGrid, staggerloc=srcSLoc, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      call ESMF_FieldGet(dstField, arrayspec=dstArrSpec,                &
                         grid=dstGrid, staggerloc=dstSLoc, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
         line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Create frac fields 
!-----------------------------------------------------------------------
!
      srcFrac = ESMF_FieldCreate(srcGrid, srcArrSpec,                   &
                                 staggerloc=srcSLoc,                    &
                                 name='ff_'//trim(fname)//'_SRC', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      dstFrac = ESMF_FieldCreate(dstGrid, dstArrSpec,                   &
                                 staggerloc=dstSLoc,                    &
                                 name='ff_'//trim(fname)//'_DST', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Calculate frac fields 
!-----------------------------------------------------------------------
!  
      regridMethod = ESMF_REGRIDMETHOD_CONSERVE
      call ESMF_FieldRegridStore(srcField=srcField,                     &
                                 dstField=dstField,                     &
                                 srcMaskValues=(/models(iSrc)%isLand/), &
                                 dstMaskValues=(/models(iDst)%isLand/), &
                                 srcFracField=srcFrac,                  &
                                 dstFracField=dstFrac,                  &
                                 unmappedaction=unmap,                  &
                                 routeHandle=routeHandle,               &
                                 regridmethod=regridMethod,             &
                                 rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Add fields to fieldbundle  
!-----------------------------------------------------------------------
!
      call ESMF_FieldBundleAdd(savFields, (/ srcFrac, dstFrac /), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      end if
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
                  flag1, (.not. flag2)
      end if
!
      end do
      end if
!
!-----------------------------------------------------------------------
!     Formats 
!-----------------------------------------------------------------------
!
 40   format(A8,': routehandle ',A4,'[',A,'] to ',A4,'[',A,']',         &
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
      logical :: flag
      real*8 :: src_total, dst_total
      integer :: srcValueList(9), dstValueList(9)
      integer :: localPet, petCount
      integer :: i, j, srcCount, dstCount
      integer :: iSrc, iDst, idSrc, idDst, itSrc, itDst, grSrc, grDst
      character(ESMF_MAXSTR), pointer :: srcList(:), dstList(:)
      character(ESMF_MAXSTR) :: msgString, cname, fname, rname
!
      type(NUOPC_Type_IS) :: genIS
      type(ESMF_VM) :: vm
      type(ESMF_Grid) :: srcGrid, dstGrid
      type(ESMF_RouteHandle) :: routeHandle
      type(ESMF_ArraySpec) :: srcArrSpec, dstArrSpec
      type(ESMF_StaggerLoc) :: srcSLoc, dstSLoc
      type(ESMF_Field) :: srcArea, dstArea
      type(ESMF_Field) :: srcField, dstField, tmpField
      type(ESMF_Field) :: srcFrac, dstFrac
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
!-----------------------------------------------------------------------
!     Get internal state 
!-----------------------------------------------------------------------
!
      nullify(genIS%wrap)
      call ESMF_UserCompGetInternalState(ccomp, NUOPC_Label_IS,         &
                                         genIS, rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Get size of field list
!-----------------------------------------------------------------------
!
      call ESMF_FieldBundleGet(genIS%wrap%srcFields,                    &
                               fieldCount=srcCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      call ESMF_FieldBundleGet(genIS%wrap%dstFields,                    &
                               fieldCount=dstCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Get name of fields 
!-----------------------------------------------------------------------
!
      allocate(srcList(srcCount))
      call ESMF_FieldBundleGet(genIS%wrap%srcFields,                    &
                               fieldNameList=srcList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      allocate(dstList(dstCount))
      call ESMF_FieldBundleGet(genIS%wrap%dstFields,                    &
                               fieldNameList=dstList, rc=rc)
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
      call ESMF_FieldBundleGet(genIS%wrap%srcFields, trim(fname),       &
                               field=srcField, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      call ESMF_FieldBundleGet(genIS%wrap%dstFields, trim(fname),       &
                               field=dstField, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Create temporary field in destination grid
!-----------------------------------------------------------------------
!
!      call ESMF_FieldGet(dstField, arrayspec=dstArrSpec,                &
!                         grid=dstGrid, staggerloc=dstSLoc, rc=rc)
!      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
!         line=__LINE__, file=FILENAME)) return
!
!      tmpField = ESMF_FieldCreate(dstGrid, dstArrSpec,                  &
!                                  staggerloc=dstSLoc, name='tmp', rc=rc)
!      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
!         line=__LINE__, file=FILENAME)) return
!
!      call ESMF_FieldEmptyComplete(tmpField, dstArrSpec, rc=rc)
!      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
!         line=__LINE__, file=FILENAME)) return
      tmpField = UTIL_FieldCreate(dstField, fname, ONE_R8, rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Get 1st routehandle from state 
!-----------------------------------------------------------------------
!
      rname = trim(GRIDDES(grSrc))//'_'//trim(GRIDDES(grDst))//'_'//    &
              trim(INTPDES(itSrc))//'_'//trim(cname)
!
      call ESMF_StateGet(genIS%wrap%state, 'rh_'//trim(rname),          &
                         routeHandle, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Perform regrid operation
!-----------------------------------------------------------------------
!
      call ESMF_FieldRegrid(srcField, tmpField, routeHandle,            &
                            zeroregion=ESMF_REGION_SELECT, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      call ESMF_FieldWrite(dstField, 'dst_ones.nc', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Get 2nd routehandle from state 
!-----------------------------------------------------------------------
!
      rname = trim(GRIDDES(grSrc))//'_'//trim(GRIDDES(grDst))//'_'//    &
              'NS2D_'//trim(cname)
!
      call ESMF_StateGet(genIS%wrap%state, 'rh_'//trim(rname),          &
                         routeHandle, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Perform regrid operation
!-----------------------------------------------------------------------
!
      call ESMF_FieldRegrid(tmpField, dstField, routeHandle,            &
                            zeroregion=ESMF_REGION_SELECT, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      call ESMF_FieldWrite(dstField, 'dst_ones_v2.nc', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Check: integral adjustment is activated for the field 
!-----------------------------------------------------------------------
!
      flag = .false.
!
      if (models(iSrc)%exportField(idSrc)%enable_integral_adj) then
!
!-----------------------------------------------------------------------
!     Check: area fields are created before or not 
!     Example area field name, aa_CROSS_DOT_ATM-OCN_SRC        
!-----------------------------------------------------------------------
!
      fname = trim(GRIDDES(grSrc))//'_'//trim(GRIDDES(grDst))//'_'//    &
              trim(cname)
!
      call ESMF_FieldBundleGet(savFields,                               &
                               fieldName='aa_'//trim(fname)//'_SRC',    &
                               isPresent=flag, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Get area fields
!-----------------------------------------------------------------------
!
      if (.not. flag) then
!
!-----------------------------------------------------------------------
!     Get routehandle from state 
!-----------------------------------------------------------------------
!
!      rname = trim(GRIDDES(grSrc))//'_'//trim(GRIDDES(grDst))//'_'//    &
!              trim(INTPDES(Iconsv))//'_'//trim(cname)
!
!      call ESMF_StateGet(genIS%wrap%state, 'rh_'//trim(rname),          &
!                         routeHandle, rc=rc)
!      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
!          line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Perform regrid operation (conservative)
!-----------------------------------------------------------------------
!
!      call ESMF_FieldRegrid(srcField, dstField, routeHandle,            &
!                            zeroregion=ESMF_REGION_SELECT, rc=rc)
!      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
!          line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Query source and destination field
!-----------------------------------------------------------------------
!
      call ESMF_FieldGet(srcField, arrayspec=srcArrSpec,                &
                         grid=srcGrid, staggerloc=srcSLoc, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      call ESMF_FieldGet(dstField, arrayspec=dstArrSpec,                &
                         grid=dstGrid, staggerloc=dstSLoc, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
         line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Create fields to store grid areas 
!-----------------------------------------------------------------------
!
      srcArea = ESMF_FieldCreate(srcGrid, srcArrSpec,                   &
                                 staggerloc=srcSLoc,                    &
                                 name='aa_'//trim(fname)//'_SRC', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      dstArea = ESMF_FieldCreate(dstGrid, dstArrSpec,                   &
                                 staggerloc=dstSLoc,                    & 
                                 name='aa_'//trim(fname)//'_DST', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Get grid areas 
!-----------------------------------------------------------------------
!        
      call ESMF_FieldRegridGetArea(srcArea, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      call ESMF_FieldRegridGetArea(dstArea, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Put grid areas to the field bundle
!-----------------------------------------------------------------------
! 
      call ESMF_FieldBundleAdd(savFields, (/ srcArea, dstArea /), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      else
!
!-----------------------------------------------------------------------
!     Get source and destination area from field bundle 
!-----------------------------------------------------------------------
! 
      call ESMF_FieldBundleGet(savFields,                               &
                               fieldName='aa_'//trim(fname)//'_SRC',    &
                               field=srcArea, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      call ESMF_FieldBundleGet(savFields,                               &
                               fieldName='aa_'//trim(fname)//'_DST',    &
                               field=dstArea, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      end if
!
!-----------------------------------------------------------------------
!     Get source and destination frac fields
!-----------------------------------------------------------------------
!
      flag = .false.
      call ESMF_FieldBundleGet(savFields,                               &
                               fieldName='ff_'//trim(fname)//'_SRC',    &
                               isPresent=flag, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      if (flag) then
      call ESMF_FieldBundleGet(savFields,                               &
                               fieldName='ff_'//trim(fname)//'_SRC',    &
                               field=srcFrac, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      call ESMF_FieldBundleGet(savFields,                               &
                               fieldName='ff_'//trim(fname)//'_DST',    &
                               field=dstFrac, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      end if
!
!-----------------------------------------------------------------------
!     Calculate integral 
!-----------------------------------------------------------------------
!
      src_total = ZERO_R8
      src_total = UTIL_CalcIntegral(vm, srcField, srcFrac, srcArea, rc) 
      if (localPet == 0) print*, "src_integral = ", src_total
!
      dst_total = ZERO_R8
      dst_total = UTIL_CalcIntegral(vm, dstField, dstFrac, dstArea, rc) 
      if (localPet == 0) print*, "dst_integral = ", dst_total
!
!-----------------------------------------------------------------------
!     Debug: write data to disk (in binary)    
!-----------------------------------------------------------------------
!
      if (debugLevel > 2) then
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
!
      call ESMF_FieldWrite(srcFrac, 'src_frac_'//trim(cname)//'.nc',    &
                           variableName='src_frac', overwrite=.true.,   &
                           rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
! 
      call ESMF_FieldWrite(dstFrac, 'dst_frac_'//trim(cname)//'.nc',    &
                           variableName='dst_frac', overwrite=.true.,   &
                           rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      call ESMF_FieldWrite(srcArea, 'src_area_'//trim(cname)//'.nc',    &
                           variableName='src_area', overwrite=.true.,   &
                           rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      call ESMF_FieldWrite(dstArea, 'dst_area_'//trim(cname)//'.nc',    &
                           variableName='dst_area', overwrite=.true.,   &
                           rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
      end if
!
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
                               convention="NUOPC", purpose="General",   &
                               rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
            line=__LINE__, file=FILENAME)) return
!
        call ESMF_AttributeGet(dstField, name="TimeStamp",              &
                               valueList=dstValueList,                  &
                               convention="NUOPC", purpose="General",   &
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
 50   format(A8,': tstamp ',A4,' [',I4,'-',I2.2,'-',                    &
             I2.2,'_',I2.2,'_',I2.2,'] to ',A4,' [',I4,'-',I2.2,'-',    &
             I2.2,'_',I2.2,'_',I2.2,']')
 60   format(A8,': regrid ',A4,' [',A,'] to ',A4,' [',A,']',' >> ',A)
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
      integer :: localPet, petCount
      integer :: i, j, sid, isrc, idst, srcCount
      character(ESMF_MAXSTR), pointer :: srcList(:)
      character(ESMF_MAXSTR) :: cname
!
      type(ESMF_VM) :: vm
      type(NUOPC_Type_IS) :: genIS
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
      call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      do i = 1, nModels
      do j = 1, nModels
        if (connectors(i,j)%modActive .and.                             &
            trim(connectors(i,j)%name) == trim(cname)) then
          isrc = i
          idst = j
        end if
      end do
      end do
!
!-----------------------------------------------------------------------
!     Get internal state 
!-----------------------------------------------------------------------
!
      nullify(genIS%wrap)
      call ESMF_UserCompGetInternalState(ccomp, NUOPC_Label_IS,         &
                                         genIS, rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Get size of field list
!-----------------------------------------------------------------------
!
      call ESMF_FieldBundleGet(genIS%wrap%srcFields,                    &
                               fieldCount=srcCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Get name of fields 
!-----------------------------------------------------------------------
!
      allocate(srcList(srcCount))
      call ESMF_FieldBundleGet(genIS%wrap%srcFields,                    &
                               fieldNameList=srcList, rc=rc)
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
!     Get source field index 
!-----------------------------------------------------------------------
!
      sid = get_varid(models(isrc)%exportField, srcList(i))
!
!-----------------------------------------------------------------------
!     Get routehandle from state 
!-----------------------------------------------------------------------
!
      call ESMF_StateGet(genIS%wrap%state, 'rh_'//                      &
                         trim(models(isrc)%exportField(sid)%short_name),&
                         routeHandle, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Release routehandle
!-----------------------------------------------------------------------
!
      call ESMF_FieldBundleRegridRelease(routeHandle, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
      end do
!
      end subroutine CPL_ReleaseRH
!
      end module mod_esmf_cpl
