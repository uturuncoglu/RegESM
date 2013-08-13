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
      logical :: flag
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
      type(ESMF_Field) :: srcField, dstField
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
!     Create frac fields for conservative type regridding 
!-----------------------------------------------------------------------
!
      if (itSrc == Iconsv) then
      srcFrac = ESMF_FieldCreate(srcGrid, srcArrSpec,                   &
                                 staggerloc=srcSLoc, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      dstFrac = ESMF_FieldCreate(dstGrid, dstArrSpec,                   &
                                 staggerloc=dstSLoc, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
      end if 
!
!-----------------------------------------------------------------------
!     Check: routehandle created or not 
!-----------------------------------------------------------------------
!
      rname = trim(GRIDDES(grSrc))//'_'//trim(GRIDDES(grDst))//'_'//    &
              trim(INTPDES(itSrc))
!
      call ESMF_StateGet(genIS%wrap%state,itemSearch='rh_'//trim(rname),&
                         itemCount=itemCount, rc=rc)
!
      flag = .false.
      if (itemCount <= 0) flag = .true.
!
!-----------------------------------------------------------------------
!     Create routehandle based on selected grid and interpolation type 
!-----------------------------------------------------------------------
!
     if (flag) then
      unmap = ESMF_UNMAPPEDACTION_IGNORE
      if (itSrc == Iconsv) then
        regridMethod = ESMF_REGRIDMETHOD_CONSERVE
        call ESMF_FieldRegridStore(srcField=srcField,                   &
                                   dstField=dstField,                   &
                                   srcFracField=srcFrac,                &
                                   dstFracField=dstfrac,                &
                                   unmappedaction=unmap,                &
                                   routeHandle=routeHandle,             &
                                   regridmethod=regridMethod,           &
                                   rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
            line=__LINE__, file=FILENAME)) return
      else if (itSrc == Ibilin) then
        regridMethod = ESMF_REGRIDMETHOD_BILINEAR
        if (iSrc == Iocean) then
        call ESMF_FieldRegridStore(srcField=srcField,                   &
                                   dstField=dstField,                   &
                                   srcMaskValues=(/0/),                 &
                                   unmappedaction=unmap,                &
                                   routeHandle=routeHandle,             &
                                   regridmethod=regridMethod,           &
                                   rc=rc)
        else
        call ESMF_FieldRegridStore(srcField=srcField,                   &
                                   dstField=dstField,                   &
                                   unmappedaction=unmap,                &
                                   routeHandle=routeHandle,             &
                                   regridmethod=regridMethod,           &
                                   rc=rc)
        end if
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
            line=__LINE__, file=FILENAME)) return
      else if (itSrc == Instod) then
        regridMethod = ESMF_REGRIDMETHOD_NEAREST_STOD
        call ESMF_FieldRegridStore(srcField=srcField,                   &
                                   dstField=dstField,                   &
                                   unmappedaction=unmap,                &
                                   routeHandle=routeHandle,             &
                                   regridmethod=regridMethod,           &
                                   rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
            line=__LINE__, file=FILENAME)) return
      else if (itSrc == Indtos) then
        regridMethod = ESMF_REGRIDMETHOD_NEAREST_DTOS
        call ESMF_FieldRegridStore(srcField=srcField,                   &
                                   dstField=dstField,                   &
                                   unmappedaction=unmap,                &
                                   routeHandle=routeHandle,             &
                                   regridmethod=regridMethod,           &
                                   rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
            line=__LINE__, file=FILENAME)) return
      else
        write(msgString,'(a)') trim(cname)//': selected '//             &
              'interpolation type is not supported! '//INTPDES(itSrc)
        call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_ERROR)
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      end if
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
      call ESMF_StateAdd(genIS%wrap%state, (/ routeHandle /), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
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
                  flag
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
             ' >> ',A, ' - ',L1)
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
      integer :: srcValueList(9), dstValueList(9)
      integer :: localPet, petCount
      integer :: i, j, srcCount, dstCount
      integer :: iSrc, iDst, idSrc, idDst, itSrc, itDst, grSrc, grDst
      character(ESMF_MAXSTR), pointer :: srcList(:), dstList(:)
      character(ESMF_MAXSTR) :: msgString, cname, fname, rname
!
      type(NUOPC_Type_IS) :: genIS
      type(ESMF_VM) :: vm
      type(ESMF_RouteHandle) :: routeHandle
      type(ESMF_Field) :: srcField, dstField
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
!     Get routehandle from state 
!-----------------------------------------------------------------------
!
      rname = trim(GRIDDES(grSrc))//'_'//trim(GRIDDES(grDst))//'_'//    &
              trim(INTPDES(itSrc))
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
      call ESMF_FieldRegrid(srcField, dstField, routeHandle,            &
                            zeroregion=ESMF_REGION_SELECT, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
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
