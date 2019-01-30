!=======================================================================
! Regional Earth System Model (RegESM)
! Copyright (c) 2013-2019 Ufuk Turuncoglu
! Licensed under the MIT License.
!=======================================================================
#define FILENAME "util/mod_utils.F90" 
!
!-----------------------------------------------------------------------
!     Module file for generic utilities
!-----------------------------------------------------------------------
!
      module mod_utils
!
!-----------------------------------------------------------------------
!     Used module declarations 
!-----------------------------------------------------------------------
!
      use ESMF
!
      use mod_types
!
      implicit none
!
!-----------------------------------------------------------------------
!     Interfaces 
!-----------------------------------------------------------------------
!
      interface UTIL_VMGlobalBroadcast 
        module procedure UTIL_VMGlobalBroadcastI4
      end interface UTIL_VMGlobalBroadcast
!
      contains
!
      subroutine UTIL_FindUnmapped(srcField, dstField,                  &
                                   srcLandMask, dstLandMask,            &
                                   srcMId, dstMId, rc)
      implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
      type(ESMF_Field), intent(in) :: srcField 
      type(ESMF_Field), intent(in) :: dstField
      integer, intent(in) :: srcLandMask 
      integer, intent(in) :: dstLandMask
      integer, intent(in) :: srcMId
      integer, intent(in) :: dstMId
      integer, intent(out) :: rc
!
!-----------------------------------------------------------------------
!     Local variable declarations 
!-----------------------------------------------------------------------
!
      integer :: cLbnd(2), cUbnd(2)
      integer :: i, j, k, srcTermProcessing, localDECount
      character(ESMF_MAXSTR) :: fname
      real(ESMF_KIND_R8), dimension(:,:), pointer :: ptr2d, bdy2d
      integer(ESMF_KIND_I4), dimension(:,:), pointer :: msk2d
!
      type(ESMF_Grid) :: grid
      type(ESMF_Field) :: aField, bField, cField
      type(ESMF_UnmappedAction_Flag) :: unmap
      type(ESMF_RegridMethod_Flag) :: regridMethod
      type(ESMF_RouteHandle) :: routeHandle
      type(ESMF_StaggerLoc) :: sLoc
!
      rc = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!     Create dummy fields 
!-----------------------------------------------------------------------
!
      fname = 'const_1'
      aField = UTIL_FieldCreate(srcField, fname, ONE_R8,                &
                                srcLandMask, rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      fname = 'const_2'
      bField = UTIL_FieldCreate(dstField, fname, MISSING_R8,            &
                                dstLandMask, rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      fname = 'const_3'
      cField = UTIL_FieldCreate(dstField, fname, ZERO_R8, -1, rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Create 1st routehandle 
!     Used to find the boundary of the destination grid
!-----------------------------------------------------------------------
!
      unmap = ESMF_UNMAPPEDACTION_IGNORE
      if (srcMId == Iatmos) then
        regridMethod = ESMF_REGRIDMETHOD_NEAREST_STOD
      else
        regridMethod = ESMF_REGRIDMETHOD_NEAREST_DTOS
      end if
!
      srcTermProcessing = 0
!
      call ESMF_FieldRegridStore(srcField=aField,                       &
                                 dstField=bField,                       &
                                 srcMaskValues=(/srcLandMask/),         &
                                 dstMaskValues=(/dstLandMask/),         &
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
!     Perform regrid using 1st routehandle 
!-----------------------------------------------------------------------
!
      call ESMF_FieldRegrid(aField, bField, routeHandle,                &
                            zeroregion=ESMF_REGION_EMPTY,               &
                            termorderflag=ESMF_TERMORDER_SRCSEQ,        &
                            rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Create 2nd routehandle 
!     Used to find the unmapped grid cells
!-----------------------------------------------------------------------
!
      unmap = ESMF_UNMAPPEDACTION_IGNORE
      regridMethod = ESMF_REGRIDMETHOD_BILINEAR
!
      call ESMF_FieldRegridStore(srcField=aField,                       &
                                 dstField=cField,                       &
                                 srcMaskValues=(/srcLandMask/),         &
                                 dstMaskValues=(/dstLandMask/),         &
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
!     Perform regrid using 2nd routehandle
!-----------------------------------------------------------------------
!
      call ESMF_FieldRegrid(aField, cField, routeHandle,                &
                            zeroregion=ESMF_REGION_TOTAL,               &
                            termorderflag=ESMF_TERMORDER_SRCSEQ,        &
                            rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Query result field
!-----------------------------------------------------------------------
!
      call ESMF_FieldGet(cField, grid=grid, staggerloc=sLoc, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Get number of local DEs in the grid
!-----------------------------------------------------------------------
!
      call ESMF_GridGet(grid, localDECount=localDECount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      do k = 0, localDECount-1
!
!-----------------------------------------------------------------------
!     Get pointer from fields 
!-----------------------------------------------------------------------
!
      call ESMF_FieldGet(bField, localDe=k, farrayPtr=bdy2d, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      call ESMF_FieldGet(cField, localDe=k, farrayPtr=ptr2d,            &
                         computationalLBound=cLbnd,                     &
                         computationalUBound=cUbnd, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Get pointer from grid (mask item) 
!-----------------------------------------------------------------------
!
      call ESMF_GridGetItem(grid, ESMF_GRIDITEM_MASK, staggerloc=sLoc,  &
                            localDe=k, farrayPtr=msk2d, rc=rc) 
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Modify masking to split mapped and unmapped grid cells    
!-----------------------------------------------------------------------
!
      do i = cLbnd(1), cUbnd(1)
      do j = cLbnd(2), cUbnd(2)
        if ((bdy2d(i,j) < TOL_R8).and.(msk2d(i,j) /= dstLandMask)) then
          if (ptr2d(i,j) < ONE_R8/2.0d0) then
            msk2d(i,j) = UNMAPPED_MASK
          else
            msk2d(i,j) = MAPPED_MASK
          end if   
        end if
      end do
      end do
!
!-----------------------------------------------------------------------
!     Nullify pointer to make sure that it does not point on a random 
!     part in the memory 
!-----------------------------------------------------------------------
!
      if (associated(ptr2d)) then
        nullify(ptr2d)
      end if
      if (associated(bdy2d)) then
        nullify(bdy2d)
      end if
      if (associated(msk2d)) then
        nullify(msk2d)
      end if
!
      end do
!
!-----------------------------------------------------------------------
!     Remove temporary fields
!-----------------------------------------------------------------------
!
      call ESMF_FieldDestroy(aField, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      call ESMF_FieldDestroy(bField, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      call ESMF_FieldDestroy(cField, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
! 
      end subroutine UTIL_FindUnmapped
!
      function UTIL_FieldCreate(field, fname, initVal, dstLandMask, rc)
      implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
      type(ESMF_Field) :: UTIL_FieldCreate 
! 
      type(ESMF_Field), intent(in) :: field
      character(*), intent(in) :: fname
      real(ESMF_KIND_R8), intent(in) :: initVal
      integer(ESMF_KIND_I4), intent(in) :: dstLandMask      
      integer, intent(out) :: rc 
!
!-----------------------------------------------------------------------
!     Local variable declarations 
!-----------------------------------------------------------------------
!
      integer :: i, j, k, localDECount
      integer :: cLbnd(2), cUbnd(2)
      real(ESMF_KIND_R8), dimension(:,:), pointer :: ptr2d
      integer(ESMF_KIND_I4), dimension(:,:), pointer :: msk2d
      integer(ESMF_KIND_I4), allocatable, dimension(:,:) :: tlw, tuw
!
      type(ESMF_Grid) :: grid
      type(ESMF_DistGrid) :: distGrid
      type(ESMF_ArraySpec) :: arraySpec
      type(ESMF_StaggerLoc) :: staggerLoc      
!
      rc = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!     Query field
!-----------------------------------------------------------------------
!
      call ESMF_FieldGet(field, arrayspec=arraySpec,                    &
                         grid=grid, staggerloc=staggerLoc, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Query grid
!-----------------------------------------------------------------------
!
      call ESMF_GridGet(grid, distgrid=distGrid,                        &
                        localDECount=localDECount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Allocate arrays for totalLWidth, totalUWidth and query field 
!-----------------------------------------------------------------------
!
      if (.not. allocated(tlw)) then
        allocate(tlw(2,localDECount))
        allocate(tuw(2,localDECount))
      end if
!
      call ESMF_FieldGet(field, totalLWidth=tlw, totalUWidth=tuw, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Create field from base field attributes 
!-----------------------------------------------------------------------
!
      if (localDECount == 1) then
        UTIL_FieldCreate = ESMF_FieldCreate(grid, arraySpec,            &
                                            staggerloc=staggerLoc,      &
                                            totalLWidth=tlw(:,1),       &
                                            totalUWidth=tuw(:,1),       &
                                            name=trim(fname), rc=rc)
      else
        UTIL_FieldCreate = ESMF_FieldCreate(grid, arraySpec,            &
                                            staggerloc=staggerLoc,      &
                                            name=trim(fname), rc=rc)
      end if
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      do k = 0, localDECount-1
!
!-----------------------------------------------------------------------
!     Get pointer from field 
!-----------------------------------------------------------------------
!
      call ESMF_FieldGet(UTIL_FieldCreate, localDe=k, farrayPtr=ptr2d,  &
                         computationalLBound=cLbnd,                     &
                         computationalUBound=cUbnd, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Get pointer from grid (mask item) 
!-----------------------------------------------------------------------
!
      call ESMF_GridGetItem(grid, ESMF_GRIDITEM_MASK,                   &
                            staggerloc=staggerLoc,                      &
                            localDe=k, farrayPtr=msk2d, rc=rc) 
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Initialize pointer 
!-----------------------------------------------------------------------
!
      do i = cLbnd(1), cUbnd(1)
        do j = cLbnd(2), cUbnd(2)
          if (msk2d(i,j) /= dstLandMask) then
            ptr2d(i,j) = initVal
          else
            ptr2d(i,j) = MISSING_R8
          end if
        end do
      end do
!
!-----------------------------------------------------------------------
!     Nullify pointer to make sure that it does not point on a random 
!     part in the memory 
!-----------------------------------------------------------------------
!
      if (associated(ptr2d)) then
        nullify(ptr2d)
      end if
      if (associated(msk2d)) then
        nullify(msk2d)
      end if
!
      end do
!
!-----------------------------------------------------------------------
!     Deallocate temporary fields
!-----------------------------------------------------------------------
!
      if (allocated(tlw)) then
        deallocate(tlw)
        deallocate(tuw)
      end if
!
!-----------------------------------------------------------------------
!     Check consistency of the created field 
!-----------------------------------------------------------------------
!
      call ESMF_FieldValidate(UTIL_FieldCreate, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!      
      end function UTIL_FieldCreate
!
      function UTIL_CalcIntegral(vm, field, maskval, rc)
      implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
      real(ESMF_KIND_R8) :: UTIL_CalcIntegral
!
      type(ESMF_VM), intent(in) :: vm
      type(ESMF_Field), intent(in) :: field
      integer(ESMF_KIND_I4), intent(in) :: maskval(:)
      integer, intent(out) :: rc
!
!-----------------------------------------------------------------------
!     Local variable declarations 
!-----------------------------------------------------------------------
!
      integer :: cLbnd(2), cUbnd(2)
      integer :: i, j, k, localDECount, localPet, petCount
      real(ESMF_KIND_R8), pointer :: ptrField(:,:) 
      real(ESMF_KIND_R8), pointer :: ptrArea(:,:)      
      integer(ESMF_KIND_I4), pointer :: ptrMask(:,:)      
      real*8 :: total_de(1), total_global(1)
      character(ESMF_MAXSTR) :: fname
!
      type(ESMF_Grid) :: grid
      type(ESMF_StaggerLoc) :: sLoc
!
      UTIL_CalcIntegral = ZERO_R8 
      rc = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!     Query VM 
!-----------------------------------------------------------------------
!
      call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Get number of local DEs
!-----------------------------------------------------------------------
!
      call ESMF_FieldGet(field, grid=grid, staggerloc=sLoc, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      call ESMF_GridGet(grid, localDECount=localDECount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      total_de(1) = ZERO_R8
      do k = 0, localDECount-1
!
!-----------------------------------------------------------------------
!     Get field pointers 
!-----------------------------------------------------------------------
!
      call ESMF_FieldGet(field, localDe=k, farrayPtr=ptrField,          &
                         computationalLBound=cLbnd,                     &
                         computationalUBound=cUbnd, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Get pointer from grid (area item) 
!-----------------------------------------------------------------------
!
      call ESMF_GridGetItem(grid, ESMF_GRIDITEM_AREA, staggerloc=sLoc,  &
                            localDe=k, farrayPtr=ptrArea, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Get pointer from grid (mask item) 
!-----------------------------------------------------------------------
!
      call ESMF_GridGetItem(grid, ESMF_GRIDITEM_MASK, staggerloc=sLoc,  &
                            localDe=k, farrayPtr=ptrMask, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Calculate integral for each local DE and PET 
!-----------------------------------------------------------------------
!
      do i = cLbnd(1), cUbnd(1)
      do j = cLbnd(2), cUbnd(2)
        if (any(ptrMask(i,j) == maskval)) then 
          total_de(1) = total_de(1)+ptrField(i,j)*ptrArea(i,j)
        end if
      end do
      end do
!
!-----------------------------------------------------------------------
!     Nullify pointer to make sure that it does not point on a random 
!     part in the memory 
!-----------------------------------------------------------------------
!
      if (associated(ptrField)) then
        nullify(ptrField)
      end if
      if (associated(ptrArea)) then
        nullify(ptrArea)
      end if
      if (associated(ptrMask)) then
        nullify(ptrMask)
      end if
!
      end do
!
!-----------------------------------------------------------------------
!     Debug: write sum of each PETs    
!-----------------------------------------------------------------------
!
      if (debugLevel > 2) then
        call ESMF_FieldGet(field, name=fname, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
                               line=__LINE__, file=FILENAME)) return
        write(*,20) localPet, k, total_de(1), trim(fname)
        call ESMF_VMBarrier(vm, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
                               line=__LINE__, file=FILENAME)) return
      end if
!
!-----------------------------------------------------------------------
!     Collect integral from PETs and calculate global one 
!-----------------------------------------------------------------------
!
      call ESMF_VMAllReduce(vm, total_de, total_global, 1,              &
                            ESMF_REDUCE_SUM, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      UTIL_CalcIntegral = total_global(1)
!
!-----------------------------------------------------------------------
!     Debug: write global sum    
!-----------------------------------------------------------------------
!
      if (debugLevel > 1) then
        if (localPet == 0) then
          write(*,30) localPet, total_global(1), trim(fname) 
        end if
        call ESMF_VMBarrier(vm, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
                             line=__LINE__, file=FILENAME)) return
      end if
!
!-----------------------------------------------------------------------
!     Format definition 
!-----------------------------------------------------------------------
!
 20   format(" PET(",I3.3,") - DE(",I2.2,                               &
             ") - INTEGRAL = ",E14.5," (",A,")")
 30   format(" PET(",I3.3,") - GLOBAL INTEGRAL = ",E14.5," (",A,")")
!
      end function UTIL_CalcIntegral
!
      subroutine UTIL_AdjustField(vm, field, maskval, error, rc)
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
      type(ESMF_VM), intent(in) :: vm
      type(ESMF_Field), intent(inout) :: field
      integer(ESMF_KIND_I4), intent(in) :: maskval(:)
      real(ESMF_KIND_R8), intent(in) :: error
      integer, intent(out) :: rc
!
!-----------------------------------------------------------------------
!     Local variable declarations 
!-----------------------------------------------------------------------
!
      integer :: cLbnd(2), cUbnd(2)
      integer :: i, j, k, localDECount, localPet, petCount
      real(ESMF_KIND_R8), pointer :: ptrField(:,:) 
      real(ESMF_KIND_R8), pointer :: ptrArea(:,:)    
      integer(ESMF_KIND_I4), pointer :: ptrMask(:,:)    
      real(ESMF_KIND_R8) :: total_de(1), total_global(1)
      real(ESMF_KIND_R8) :: error_unit
      character(ESMF_MAXSTR) :: fname
!
      type(ESMF_Grid) :: grid
      type(ESMF_StaggerLoc) :: sLoc
!
      rc = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!     Query VM 
!-----------------------------------------------------------------------
!
      call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Get number of local DEs
!-----------------------------------------------------------------------
!
      call ESMF_FieldGet(field, grid=grid, name=fname,                  &
                         staggerloc=sLoc, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      call ESMF_GridGet(grid, localDECount=localDECount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      total_de(1) = ZERO_R8
      do k = 0, localDECount-1
!
!-----------------------------------------------------------------------
!     Get pointer from grid (area item) 
!-----------------------------------------------------------------------
!
      call ESMF_GridGetItem(grid, ESMF_GRIDITEM_AREA, staggerloc=sLoc,  &
                            localDe=k, farrayPtr=ptrArea, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Get pointer from grid (mask item) 
!-----------------------------------------------------------------------
!
      call ESMF_GridGetItem(grid, ESMF_GRIDITEM_MASK, staggerloc=sLoc,  &
                            localDe=k, farrayPtr=ptrMask, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Calculate total area of matched region 
!-----------------------------------------------------------------------
!
      cLbnd(1) = lbound(ptrMask, dim=1)
      cUbnd(1) = ubound(ptrMask, dim=1)
      cLbnd(2) = lbound(ptrMask, dim=2)
      cUbnd(2) = ubound(ptrMask, dim=2)
!
      do i = cLbnd(1), cUbnd(1)
      do j = cLbnd(2), cUbnd(2)      
        if (any(ptrMask(i,j) == maskval)) then
          total_de(1) = total_de(1)+ptrArea(i,j)
        end if
      end do
      end do
!
!-----------------------------------------------------------------------
!     Nullify pointer to make sure that it does not point on a random 
!     part in the memory 
!-----------------------------------------------------------------------
!
      if (associated(ptrArea)) then
        nullify(ptrArea)
      end if
      if (associated(ptrMask)) then
        nullify(ptrMask)
      end if
!
      end do
!
!-----------------------------------------------------------------------
!     Collect calculated total area from PETs
!-----------------------------------------------------------------------
!
      total_global(1) = ZERO_R8
      call ESMF_VMAllReduce(vm, total_de, total_global, 1,              &
                            ESMF_REDUCE_SUM, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Calculate error per unit area 
!-----------------------------------------------------------------------
!
      error_unit = error/total_global(1)
      if (localPet == 0) then
        write(*,40) localPet, total_global(1), error_unit, trim(fname)
      end if
!
!-----------------------------------------------------------------------
!     Adjust field using error 
!-----------------------------------------------------------------------
!
      do k = 0, localDECount-1
!
!-----------------------------------------------------------------------
!     Get field pointers 
!-----------------------------------------------------------------------
!
      call ESMF_FieldGet(field, localDe=k, farrayPtr=ptrField,          &
                         computationalLBound=cLbnd,                     &
                         computationalUBound=cUbnd, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Get pointer from grid (mask item) 
!-----------------------------------------------------------------------
!
      call ESMF_GridGetItem(grid, ESMF_GRIDITEM_MASK, staggerloc=sLoc,  &
                            localDe=k, farrayPtr=ptrMask, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Add error 
!-----------------------------------------------------------------------
!
      do i = cLbnd(1), cUbnd(1)
      do j = cLbnd(2), cUbnd(2)
        if (any(ptrMask(i,j) == maskval)) then
          ptrField(i,j) = ptrField(i,j)-error_unit
        end if
      end do
      end do
!
!-----------------------------------------------------------------------
!     Nullify pointer to make sure that it does not point on a random 
!     part in the memory 
!-----------------------------------------------------------------------
!
      if (associated(ptrField)) then
        nullify(ptrField)
      end if
      if (associated(ptrMask)) then
        nullify(ptrMask)
      end if
!
      end do
!
!-----------------------------------------------------------------------
!     Format definition 
!-----------------------------------------------------------------------
!
 40   format(" PET(",I3.3,") - AVGERAGE DIFF = ",2E14.5," (",A,")")
!
      end subroutine UTIL_AdjustField
!
      subroutine UTIL_VMGlobalBroadcastI4(var, rootPet, rc)
      implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
      integer, intent(inout) :: var
      integer, intent(in) :: rootPet
      integer, intent(inout) :: rc
!
!-----------------------------------------------------------------------
!     Local variable declarations 
!-----------------------------------------------------------------------
!
      type(ESMF_VM) :: vm
      integer :: var_local(1)
!
!-----------------------------------------------------------------------
!     Get global VM 
!-----------------------------------------------------------------------
! 
      call ESMF_VMGetGlobal(vm, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Broadcast data 
!-----------------------------------------------------------------------
!
      var_local(1) = var
!
      call ESMF_VMBroadcast(vm, bcstData=var_local, count=1,            &
                            rootPet=rootPet, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      var = var_local(1)
!
      end subroutine UTIL_VMGlobalBroadcastI4
!
      end module mod_utils
