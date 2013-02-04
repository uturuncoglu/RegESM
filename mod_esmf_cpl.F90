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
      integer, intent(inout) :: rc
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
      integer, intent(inout) :: rc
!     
!-----------------------------------------------------------------------
!     Local variable declarations 
!-----------------------------------------------------------------------
!
      integer :: i, j, isrc, idst, sid, did, localPet, petCount
      integer :: itype, cplCount, srcCount, dstCount
      character(ESMF_MAXSTR) :: cname, msgString
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
      type(ESMF_Field), allocatable :: srcFields(:), dstFields(:)
      type(ESMF_RouteHandle), allocatable :: routeHandle(:)
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
!     Get size of source and destination field list
!-----------------------------------------------------------------------
!
      call NUOPC_CplCompAttributeGet(ccomp, cplListSize=cplCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
      call ESMF_FieldBundleGet(genIS%wrap%srcFields,                    &
                               fieldCount=srcCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
      call ESMF_FieldBundleGet(genIS%wrap%dstFields,                    &
                               fieldCount=dstCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
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
      allocate(routeHandle(cplCount))
!
!-----------------------------------------------------------------------
!     Query field lists
!-----------------------------------------------------------------------
!
      call NUOPC_CplCompAttributeGet(ccomp, cplList=cplList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
      call ESMF_FieldBundleGet(genIS%wrap%srcFields,                    &
                               fieldNameList=srcList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
      call ESMF_FieldBundleGet(genIS%wrap%dstFields,                    &
                               fieldNameList=dstList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Query fields from field bundle 
!-----------------------------------------------------------------------
!
      if (.not. allocated(srcFields)) allocate(srcFields(srcCount))
      call ESMF_FieldBundleGet(genIS%wrap%srcFields,                    &
                               fieldList=srcFields, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      if (.not. allocated(dstFields)) allocate(dstFields(dstCount))
      call ESMF_FieldBundleGet(genIS%wrap%dstFields,                    &
                               fieldList=dstFields, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      do i = 1, cplCount 
!
!-----------------------------------------------------------------------
!     Get source and destination field index 
!-----------------------------------------------------------------------
!
      sid = get_varid(models(isrc)%exportField, srcList(i))
      did = get_varid(models(idst)%importField, dstList(i))
!
!-----------------------------------------------------------------------
!     Get interpolation type 
!-----------------------------------------------------------------------
!
      itype = models(isrc)%exportField(sid)%itype
!
!-----------------------------------------------------------------------
!     Get source and destination field
!-----------------------------------------------------------------------
!
      call ESMF_FieldGet(srcFields(i), arrayspec=srcArrSpec,            &
                         grid=srcGrid, staggerloc=srcSLoc, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      call ESMF_FieldGet(dstFields(i), arrayspec=dstArrSpec,            &
                         grid=dstGrid, staggerloc=dstSLoc, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Create frac fields for conservative type regridding 
!-----------------------------------------------------------------------
!
      if (itype == Iconsv) then
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
!     Create routehandle based on selected interpolation type 
!-----------------------------------------------------------------------
!
      unmap = ESMF_UNMAPPEDACTION_IGNORE
      if (itype == Iconsv) then
        regridMethod = ESMF_REGRIDMETHOD_CONSERVE
        call ESMF_FieldRegridStore(srcField=srcFields(i),               &
                                   dstField=dstFields(i),               &
                                   srcFracField=srcFrac,                &
                                   dstFracField=dstfrac,                &
                                   unmappedaction=unmap,                &
                                   routeHandle=routeHandle(i),          &
                                   regridmethod=regridMethod,           &
                                   rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
            line=__LINE__, file=FILENAME)) return
      else if (itype == Ibilin) then
        regridMethod = ESMF_REGRIDMETHOD_BILINEAR
        if (isrc == Iocean) then
        call ESMF_FieldRegridStore(srcField=srcFields(i),               &
                                   dstField=dstFields(i),               &
                                   srcMaskValues=(/0/),                 &
                                   unmappedaction=unmap,                &
                                   routeHandle=routeHandle(i),          &
                                   regridmethod=regridMethod,           &
                                   rc=rc)
        else
        call ESMF_FieldRegridStore(srcField=srcFields(i),               &
                                   dstField=dstFields(i),               &
                                   unmappedaction=unmap,                &
                                   routeHandle=routeHandle(i),          &
                                   regridmethod=regridMethod,           &
                                   rc=rc)
        end if
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
            line=__LINE__, file=FILENAME)) return
      else if (itype == Inone) then
         write(msgString,'(a)') trim(cname)//': no interpolation '//    &
               'needed! skip routehandle generation phase.'
      else
        write(msgString,'(a)') trim(cname)//': selected '//             &
              'interpolation type is not supported! '//INTPDES(itype)
        call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_ERROR)
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      end if
!
!-----------------------------------------------------------------------
!     Add name to routehandle    
!-----------------------------------------------------------------------
!
      call ESMF_RouteHandleSet(routeHandle(i), name='rh_'//             &
                         trim(models(isrc)%exportField(sid)%short_name),&
                         rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Debug: print out exchange fields    
!-----------------------------------------------------------------------
!
      if ((debugLevel > 0) .and. (localPet == 0)) then
      write(*,40) trim(cname),                                          &
                  trim(models(isrc)%exportField(sid)%short_name),       &
                  trim(GRIDDES(models(isrc)%exportField(sid)%gtype)),   &
                  trim(models(idst)%importField(did)%short_name),       &
                  trim(GRIDDES(models(idst)%importField(did)%gtype)),   &
                  trim(INTPDES(models(isrc)%exportField(sid)%itype))
      end if
      end do
!
!-----------------------------------------------------------------------
!     Add routehandles to the state    
!-----------------------------------------------------------------------
!
      call ESMF_StateAdd(genIS%wrap%state, routeHandle, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Deallocate arrays 
!-----------------------------------------------------------------------
!
      if (allocated(srcFields)) deallocate(srcFields)
      if (allocated(dstFields)) deallocate(dstFields)
      if (allocated(routeHandle)) deallocate(routeHandle)
      deallocate(cplList, srcList, dstList)
      end if
!
!-----------------------------------------------------------------------
!     Formats 
!-----------------------------------------------------------------------
!
 40   format(A8,': routehandle ',A4,'[',A,'] to ',A4,'[',A,']',' >> ',A)
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
      integer, intent(inout) :: rc
!     
!-----------------------------------------------------------------------
!     Local variable declarations 
!-----------------------------------------------------------------------
!
      integer :: localPet, petCount
      integer :: i, j, sid, did, isrc, idst, srcCount, dstCount
      character(ESMF_MAXSTR), pointer :: srcList(:), dstList(:)
      character(ESMF_MAXSTR) :: cname
!
      type(NUOPC_Type_IS) :: genIS
      type(ESMF_VM) :: vm
      type(ESMF_RouteHandle) :: routeHandle
      type(ESMF_Field), allocatable :: srcFields(:), dstFields(:)
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
!     Get source and destination fields
!-----------------------------------------------------------------------
!
      if (.not. allocated(srcFields)) allocate(srcFields(srcCount))
      call ESMF_FieldBundleGet(genIS%wrap%srcFields,                    &
                               fieldList=srcFields, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      if (.not. allocated(dstFields)) allocate(dstFields(dstCount))
      call ESMF_FieldBundleGet(genIS%wrap%dstFields,                    &
                               fieldList=dstFields, rc=rc)
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
      sid = get_varid(models(isrc)%exportField, srcList(i))
      did = get_varid(models(idst)%importField, dstList(i))
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
!     Perform regrid operation
!-----------------------------------------------------------------------
!
      call ESMF_FieldRegrid(srcFields(i), dstFields(i), routeHandle,    &
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
                  trim(models(isrc)%exportField(sid)%short_name),       &
                  trim(GRIDDES(models(isrc)%exportField(sid)%gtype)),   &
                  trim(models(idst)%importField(did)%short_name),       &
                  trim(GRIDDES(models(idst)%importField(did)%gtype)),   &
                  trim(INTPDES(models(isrc)%exportField(sid)%itype))
      end if
      end do
!
!-----------------------------------------------------------------------
!     Deallocate arrays
!-----------------------------------------------------------------------
!
      if (allocated(srcFields)) deallocate(srcFields)
      if (allocated(dstFields)) deallocate(dstFields)
      deallocate(srcList)
!
!-----------------------------------------------------------------------
!     Formats 
!-----------------------------------------------------------------------
!
 60   format(A8,': regrid ',A4,'[',A,'] to ',A4,'[',A,']',' >> ',A)
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
      integer, intent(inout) :: rc
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
