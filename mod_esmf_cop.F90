!=======================================================================
! Regional Earth System Model (RegESM)
! Copyright (c) 2013-2017 Ufuk Turuncoglu
! Licensed under the MIT License.
!=======================================================================
#define FILENAME "mod_esmf_cop.F90"
!
!-----------------------------------------------------------------------
!     COP gridded component code 
!-----------------------------------------------------------------------
!
      module mod_esmf_cop
!
!-----------------------------------------------------------------------
!     Used module declarations 
!-----------------------------------------------------------------------
!
      use ESMF
      use NUOPC
      use NUOPC_Model,                                                  &
          NUOPC_SetServices          => SetServices,                    &
          NUOPC_Label_Advance        => label_Advance,                  &
          NUOPC_Label_DataInitialize => label_DataInitialize,           &
          NUOPC_Label_SetClock       => label_SetClock
!
      use mod_types
      use mod_shared
!
      implicit none
      private
!
!-----------------------------------------------------------------------
!     Public subroutines 
!-----------------------------------------------------------------------
!
      public :: COP_SetServices
!
!-----------------------------------------------------------------------
!     Component specific variables 
!-----------------------------------------------------------------------
!
      integer :: nports
      character(255), allocatable :: input_ports(:)
      character(ESMF_MAXSTR), allocatable, save :: gridnames(:)
!
      type(ESMF_RouteHandle), allocatable :: routeHandle(:)
!
      interface
        subroutine create_grid(name, nProc, myRank, dims, lb, ub,       &
                               nPoints, lonCoord, latCoord,             &
                               levCoord) bind(C, name="create_grid")
        use iso_c_binding
        character :: name
        integer(c_int), value :: nProc
        integer(c_int), value :: myRank
        integer(c_int) :: dims(*)
        integer(c_int) :: lb(*)
        integer(c_int) :: ub(*)
        integer(c_int), value :: nPoints
        real(c_double) :: lonCoord(*)
        real(c_double) :: latCoord(*)
        real(c_double), optional :: levCoord(*)
        end subroutine
      end interface
!
      contains
!
      subroutine COP_SetServices(gcomp, rc)
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
!     Register NUOPC generic routines
!-----------------------------------------------------------------------
!
      call NUOPC_CompDerive(gcomp, NUOPC_SetServices, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Register initialize routines (Phase 1 and Phase 2)
!-----------------------------------------------------------------------
!
      call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE,    &
                                      userRoutine=COP_SetInitializeP0,  &
                                      phase=0, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      call NUOPC_CompSetEntryPoint(gcomp,                               &
                                   methodflag=ESMF_METHOD_INITIALIZE,   &
                                   phaseLabelList=(/"IPDv03p1"/),       &
                                   userRoutine=COP_SetInitializeP1,     &
                                   rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      call NUOPC_CompSetEntryPoint(gcomp,                               &
                                   methodflag=ESMF_METHOD_INITIALIZE,   &
                                   phaseLabelList=(/"IPDv03p4"/),       &
                                   userRoutine=COP_SetInitializeP4,     &
                                   rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      call NUOPC_CompSetEntryPoint(gcomp,                               &
                                   methodflag=ESMF_METHOD_INITIALIZE,   &
                                   phaseLabelList=(/"IPDv03p5"/),       &
                                   userRoutine=COP_SetInitializeP5,     &
                                   rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Attach phase independent specializing methods
!-----------------------------------------------------------------------
!
      call NUOPC_CompSpecialize(gcomp,                                  &
                                specLabel=NUOPC_Label_DataInitialize,   &
                                specRoutine=COP_DataInit, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      call NUOPC_CompSpecialize(gcomp, specLabel=NUOPC_Label_SetClock,  &
                                specRoutine=COP_SetClock, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      call NUOPC_CompSpecialize(gcomp, specLabel=NUOPC_Label_Advance,   &
                                specRoutine=COP_ModelAdvance, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      end subroutine COP_SetServices
!
      subroutine COP_SetInitializeP0(gcomp,                             &
                                     importState,                       &
                                     exportState,                       &
                                     clock,                             &
                                     rc)
      implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations
!-----------------------------------------------------------------------
!
      type(ESMF_GridComp) :: gcomp
      type(ESMF_State) :: importState, exportState
      type(ESMF_Clock) :: clock
      integer, intent(out) :: rc
!
!-----------------------------------------------------------------------
!     Local variable declarations
!-----------------------------------------------------------------------
!
      logical :: file_exists
      integer :: i, k, localPet, petCount, comm, nports, nf
      character(255), allocatable :: pipelines(:)
!
      type(ESMF_VM) :: vm
!
      rc = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!     Query component 
!-----------------------------------------------------------------------
!
      call ESMF_GridCompGet(gcomp, vm=vm, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      call ESMF_VMGet(vm, localPet=localPet, petCount=petCount,         &
                      mpiCommunicator=comm, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Switch to IPDv03 by filtering all other phaseMap entries
!-----------------------------------------------------------------------
!
      call NUOPC_CompFilterPhaseMap(gcomp, ESMF_METHOD_INITIALIZE,      &
                                    acceptStringList=(/"IPDv03p"/),     &
                                    rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Create list of input ports
!-----------------------------------------------------------------------
!
      if (.not. allocated(input_ports)) then
        k = 0
        do i = 1, nModels
          if (connectors(i,Icopro)%modActive) then
            k = k+1
          end if
        end do
!
        nports = k*2
        allocate(input_ports(nports))
        input_ports = ""
        allocate(gridnames(nports))
        gridnames = ""
!
        k = 1
        do i = 1, nModels
          if (connectors(i,Icopro)%modActive) then
            input_ports(k)   = to_lower(trim(models(i)%name))//         &
                               "_input2d"//char(0)
            input_ports(k+1) = to_lower(trim(models(i)%name))//         &
                               "_input3d"//char(0)
            k = k+2
          end if
        end do 
      end if
!
      if (localPet == models(Icopro)%petList(1)) then
        do i = 1, nports
          write(*,10) "[LOG "//trim(models(Icopro)%name)//"] - "//      &
                      "input port [", i, "] is defined as "//           &
                      trim(input_ports(i))
        end do
      end if
!
!-----------------------------------------------------------------------
!     Allocate routehandle array for halo update
!-----------------------------------------------------------------------
!
      if (.not. allocated(routeHandle)) then
        allocate(routeHandle(nports))
      end if
!
!-----------------------------------------------------------------------
!     Initialize co-processor
!-----------------------------------------------------------------------
!
      nf = ubound(coproc_fnames, dim=1)
      if (.not. allocated(pipelines)) allocate(pipelines(nf))
!
      do i = 1, nf
        inquire(file=trim(coproc_fnames(i)), exist=file_exists)
        if (file_exists) then
          pipelines(i) = trim(coproc_fnames(i))//char(0)
        else
          if (localPet == models(Icopro)%petList(1)) then
            write(*,20) "[ERROR "//trim(models(Icopro)%name)//"] - "//  &
                        trim(coproc_fnames(i))//" not found! Exiting!"
          end if
          call ESMF_Finalize(endflag=ESMF_END_ABORT)
        end if
      end do 
!
      call my_coprocessorinitializewithpython(comm, pipelines, nf,      &
                                              input_ports, nports)
!
      if (allocated(pipelines)) deallocate(pipelines)
!
!-----------------------------------------------------------------------
!     Format definition 
!-----------------------------------------------------------------------
!
  10  format(A,I02,A)
  20  format(A)
!
      end subroutine COP_SetInitializeP0
!
      subroutine COP_SetInitializeP1(gcomp,                             &
                                     importState,                       &
                                     exportState,                       &
                                     clock,                             &
                                     rc)
      implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
      type(ESMF_GridComp) :: gcomp
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      integer, intent(out) :: rc
!
!-----------------------------------------------------------------------
!     Local variable declarations 
!-----------------------------------------------------------------------
!
      integer :: i
!
      rc = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!     Set import fields 
!-----------------------------------------------------------------------
!
      do i = 1, ubound(models(Icopro)%importField, dim=1)
        call NUOPC_Advertise(importState,                               &
             StandardName=trim(models(Icopro)%importField(i)%long_name),&
             name=trim(models(Icopro)%importField(i)%short_name),       &
             TransferOfferGeomObject="cannot provide",                  &
             rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
                               line=__LINE__, file=FILENAME)) return
      end do 
!
!-----------------------------------------------------------------------
!     Set export fields 
!-----------------------------------------------------------------------
!
      do i = 1, ubound(models(Icopro)%exportField, dim=1)
        call NUOPC_Advertise(exportState,                               &
             StandardName=trim(models(Icopro)%exportField(i)%long_name),&
             name=trim(models(Icopro)%exportField(i)%short_name),       &
             TransferOfferGeomObject="cannot provide",                  &
             rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
                               line=__LINE__, file=FILENAME)) return
      end do
!
      end subroutine COP_SetInitializeP1
!
      subroutine COP_SetInitializeP4(gcomp,                             &
                                     importState,                       &
                                     exportState,                       &
                                     clock,                             &
                                     rc)
      implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations
!-----------------------------------------------------------------------
!
      type(ESMF_GridComp) :: gcomp
      type(ESMF_State) :: importState, exportState
      type(ESMF_Clock) :: clock
      integer, intent(out) :: rc
!
!-----------------------------------------------------------------------
!     Local variable declarations
!-----------------------------------------------------------------------
!
      integer :: i, j, k
      integer :: itemCount, localDeCount, dimCount, tileCount
      integer :: connectionCount, petCount, deCountPTile, extraDEs
      integer, allocatable :: minIndexPTile(:,:), maxIndexPTile(:,:)
      integer, allocatable :: regDecompPTile(:,:)
      character(ESMF_MAXSTR), allocatable :: itemNameList(:)
      character(ESMF_MAXSTR) :: gname
!
      type(ESMF_Array) :: arrX
      type(ESMF_Field) :: field
      type(ESMF_Grid) :: grid
      type(ESMF_DistGrid) :: distgrid
      type(ESMF_DistGridConnection), allocatable :: connectionList(:)
!
!-----------------------------------------------------------------------
!     Get list of import fields
!-----------------------------------------------------------------------
!
      call ESMF_StateGet(importState, itemCount=itemCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      if (.not. allocated(itemNameList)) then
        allocate(itemNameList(itemCount))
      end if
!
      call ESMF_StateGet(importState, itemNameList=itemNameList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Loop over import fields
!-----------------------------------------------------------------------
!
      do i = 1, itemCount
!
!-----------------------------------------------------------------------
!     Get field from import state
!-----------------------------------------------------------------------
!
      call ESMF_StateGet(importState, trim(itemNameList(i)),            &
                         field, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Get grid from field
!-----------------------------------------------------------------------
!
      call ESMF_FieldGet(field, grid=grid, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Access localDeCount to show this is a real Grid
!-----------------------------------------------------------------------
!
      call ESMF_GridGet(grid, localDeCount=localDeCount,                &
                        distgrid=distgrid, name=gname, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Query distgrid to get rank and tile information
!-----------------------------------------------------------------------
!
      call ESMF_DistGridGet(distgrid, dimCount=dimCount,                &
                            tileCount=tileCount,                        &
                            connectionCount=connectionCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     allocate minIndexPTile, maxIndexPTile and connectionList
!-----------------------------------------------------------------------
!
      if (.not. allocated(minIndexPTile)) then
        allocate(minIndexPTile(dimCount, tileCount),                    &
                 maxIndexPTile(dimCount, tileCount))
        allocate(connectionList(connectionCount))
      end if
!
!-----------------------------------------------------------------------
!     Get minIndex and maxIndex arrays
!-----------------------------------------------------------------------
!
      call ESMF_DistGridGet(distgrid, minIndexPTile=minIndexPTile,      &
                            maxIndexPTile=maxIndexPTile,                &
                            connectionList=connectionList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Construct a default regDecompPTile
!-----------------------------------------------------------------------
!
      if (.not. allocated(regDecompPTile)) then
        allocate(regDecompPTile(dimCount, tileCount))
      end if
!
      do k = 1, dimCount
        if (k < 3) then
          regDecompPTile(k, 1) = models(Icopro)%tile(3-k)
        else
          regDecompPTile(k, 1) = 1
        end if
      end do
!
!-----------------------------------------------------------------------
!     Create the new DistGrid with the same minIndexPTile and
!     maxIndexPTile but with a default regDecompPTile
!-----------------------------------------------------------------------
!
      distgrid = ESMF_DistGridCreate(minIndexPTile=minIndexPTile,       &
                                     maxIndexPTile=maxIndexPTile,       &
                                     regDecompPTile=regDecompPTile,     &
                                     connectionList=connectionList,     &
                                     rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Create a new Grid on the new DistGrid and swap it in the Field
!-----------------------------------------------------------------------
!
      grid = ESMF_GridCreate(distgrid, name=trim(gname), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      call ESMF_FieldEmptySet(field, grid=grid, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Deallocate temporary arrays to free the memory
!-----------------------------------------------------------------------
!
      if (allocated(minIndexPTile)) deallocate(minIndexPTile)
      if (allocated(maxIndexPTile)) deallocate(maxIndexPTile)
      if (allocated(connectionList)) deallocate(connectionList)
      if (allocated(regDecompPTile)) deallocate(regDecompPTile)
!
      end do
!
      end subroutine COP_SetInitializeP4
!
      subroutine COP_SetInitializeP5(gcomp,                             &
                                     importState,                       &
                                     exportState,                       &
                                     clock,                             &
                                     rc)
      implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations
!-----------------------------------------------------------------------
!
      type(ESMF_GridComp) :: gcomp
      type(ESMF_State) :: importState, exportState
      type(ESMF_Clock) :: clock
      integer, intent(out) :: rc
!
!-----------------------------------------------------------------------
!     Local variable declarations
!-----------------------------------------------------------------------
!
      integer :: i, j, k, localPet, rank, itemCount, localDECount
      integer :: tileX, tileY, tuw(3)
      character(ESMF_MAXSTR), allocatable :: itemNameList(:)
      real(ESMF_KIND_R8), pointer :: ptr2d(:,:)
      real(ESMF_KIND_R8), pointer :: ptr3d(:,:,:)
      logical :: hasRight, hasTop
!
      type(ESMF_VM) :: vm
      type(ESMF_Field) :: field
!
!-----------------------------------------------------------------------
!     Query component 
!-----------------------------------------------------------------------
!
      call ESMF_GridCompGet(gcomp, vm=vm, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      call ESMF_VMGet(vm, localPet=localPet, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Get list of import fields
!-----------------------------------------------------------------------
!
      call ESMF_StateGet(importState, itemCount=itemCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      if (.not. allocated(itemNameList)) then
        allocate(itemNameList(itemCount))
      end if
!
      call ESMF_StateGet(importState, itemNameList=itemNameList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Loop over import fields
!-----------------------------------------------------------------------
!
      do i = 1, itemCount
!
      k = get_varid(models(Icopro)%importField, trim(itemNameList(i)))
      rank = models(Icopro)%importField(k)%rank
!
!-----------------------------------------------------------------------
!     Get field from import state
!-----------------------------------------------------------------------
!
      call ESMF_StateGet(importState, trim(itemNameList(i)),            &
                         field, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Allocate data by complete 
!     It also adds extra halo regions to top and right
!-----------------------------------------------------------------------
!
      tileX = models(Icopro)%tile(1)
      tileY = models(Icopro)%tile(2)
!
      hasRight = .false.
      if ((localPet/tileY+1) < tileX) hasRight = .true.
!
      hasTop = .false.
      if (mod(localPet+1,tileY) /= 0) hasTop = .true.
!
      tuw = 0
      if (hasTop) tuw(1) = 1
      if (hasRight) tuw(2) = 1
!
      call ESMF_FieldEmptyComplete(field,                               &
                                   typekind=ESMF_TYPEKIND_R8,           &
                                   totalUWidth=tuw(:rank),              &
                                   rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Get local DE count 
!-----------------------------------------------------------------------
!
      call ESMF_FieldGet(field, localDECount=localDECount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Loop over decomposition elements (DEs) 
!-----------------------------------------------------------------------
!
      do j = 0, localDECount-1
      if (models(Icopro)%importField(k)%rank .eq. 2) then
!
!-----------------------------------------------------------------------
!     Get pointer from field 
!-----------------------------------------------------------------------
!
      call ESMF_FieldGet(field, localDE=j, farrayPtr=ptr2d, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Set initial value to missing 
!-----------------------------------------------------------------------
!
      ptr2d = MISSING_R8      
!
!-----------------------------------------------------------------------
!     Nullify pointer to make sure that it does not point on a random 
!     part in the memory 
!-----------------------------------------------------------------------
!
      if (associated(ptr2d)) then
        nullify(ptr2d)
      end if
!
      else if (models(Icopro)%importField(k)%rank .eq. 3) then
!
!-----------------------------------------------------------------------
!     Get pointer from field 
!-----------------------------------------------------------------------
!
      call ESMF_FieldGet(field, localDE=j, farrayPtr=ptr3d, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Set initial value to missing 
!-----------------------------------------------------------------------
!
      ptr3d = MISSING_R8
!
!-----------------------------------------------------------------------
!     Nullify pointer to make sure that it does not point on a random 
!     part in the memory 
!-----------------------------------------------------------------------
!
      if (associated(ptr3d)) then
        nullify(ptr3d)
      end if
!
      end if
!
      end do
!
      end do
!
      end subroutine COP_SetInitializeP5
!
      subroutine COP_DataInit(gcomp, rc)
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
      integer :: i, itemCount
!
      type(ESMF_State) :: importState
!
!-----------------------------------------------------------------------
!     Set attribute for data initialization
!-----------------------------------------------------------------------
!
      call NUOPC_CompAttributeSet(gcomp,                                &
                                  name="InitializeDataComplete",        &
                                  value="true", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      end subroutine COP_DataInit
!
      subroutine COP_SetClock(gcomp, rc)
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
      integer :: fac1, fac2, maxdiv
!
      type(ESMF_Clock) :: cmpClock 
      type(ESMF_TimeInterval) :: timeStep
!
      rc = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!     Get component clock
!-----------------------------------------------------------------------
!
      call ESMF_GridCompGet(gcomp, clock=cmpClock, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      call ESMF_ClockGet(cmpClock, timeStep=timeStep, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Modify component clock time step 
!-----------------------------------------------------------------------
!
      fac1 = maxval(connectors(Icopro,:)%divDT,mask=models(:)%modActive)
      fac2 = maxval(connectors(:,Icopro)%divDT,mask=models(:)%modActive)
      maxdiv = max(fac1, fac2)
!
      call ESMF_ClockSet(cmpClock, name='cop_clock',                    &
                         timeStep=timeStep/maxdiv, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      end subroutine COP_SetClock
!
      subroutine COP_ModelAdvance(gcomp, rc)
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
      integer :: i, j, k, its, tileX, tileY 
      integer :: localPet, petCount, itemCount, dimCount, tileCount
      integer :: nPoints2D, nPoints3D  
      integer :: cc2d(2), cc3d(3), lb(3), ub(3), dims(3)
      integer, allocatable :: minIndexPTile(:,:), maxIndexPTile(:,:)
      character(ESMF_MAXSTR) :: gname, rname, str1, str2
      character(ESMF_MAXSTR), allocatable :: itemNameList(:)
      logical :: hasRight, hasTop, flag
      real(ESMF_KIND_R8) :: stime, etime, dtime
      real(ESMF_KIND_R8), dimension(:,:), pointer :: ptr2X
      real(ESMF_KIND_R8), dimension(:,:), pointer :: ptr2Y
      real(ESMF_KIND_R8), dimension(:,:,:), pointer :: ptr3X
      real(ESMF_KIND_R8), dimension(:,:,:), pointer :: ptr3Y
      real(ESMF_KIND_R8), dimension(:,:,:), pointer :: ptr3Z
      real(ESMF_KIND_R8), allocatable, dimension(:) :: lon1d
      real(ESMF_KIND_R8), allocatable, dimension(:) :: lat1d
      real(ESMF_KIND_R8), allocatable, dimension(:) :: lev1d
      real(ESMF_KIND_R8), allocatable, dimension(:,:) :: lon2d
      real(ESMF_KIND_R8), allocatable, dimension(:,:) :: lat2d
      real(ESMF_KIND_R8), allocatable, dimension(:,:,:) :: lon3d
      real(ESMF_KIND_R8), allocatable, dimension(:,:,:) :: lat3d
      real(ESMF_KIND_R8), allocatable, dimension(:,:,:) :: lev3d
      real(ESMF_KIND_R8), dimension(:,:), pointer :: ptr2d
      real(ESMF_KIND_R8), dimension(:,:,:), pointer :: ptr3d
      real(ESMF_KIND_R8), allocatable, dimension(:) :: var1d
!
      type(ESMF_VM) :: vm
      type(ESMF_Grid) :: grid
      type(ESMF_Field) :: field
      type(ESMF_Array) :: arrX, arrY, arrZ
      type(ESMF_Clock) :: clock
      type(ESMF_TimeInterval) :: elapsedTime, timeStep
      type(ESMF_Time) :: startTime, currTime
      type(ESMF_DistGrid) :: distgrid
      type(ESMF_State) :: importState 
!
      rc = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!     Query component
!-----------------------------------------------------------------------
! 
      call NUOPC_ModelGet(gcomp, modelClock=clock,                      &
                          importState=importState, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=__FILE__)) return
!
      call ESMF_GridCompGet(gcomp, vm=vm, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=__FILE__)) return
!
      call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=__FILE__)) return
!
!-----------------------------------------------------------------------
!     Calculate elapsed time
!-----------------------------------------------------------------------
!
      call ESMF_ClockGet(clock, currTime=currTime,                      &
                         startTime=startTime, timeStep=timeStep, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=__FILE__)) return
!
      call ESMF_TimeGet(currTime,                                       &
                        timeStringISOFrac=str1, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      call ESMF_TimeGet(currTime+timeStep,                              &
                        timeStringISOFrac=str2, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      elapsedTime = currTime-startTime
!
      call ESMF_TimeIntervalGet(elapsedTime, s_r8=dtime, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=__FILE__)) return
!
      its = int(elapsedTime/timeStep)
!
!-----------------------------------------------------------------------
!     Get list of import fields
!-----------------------------------------------------------------------
!
      call ESMF_StateGet(importState, itemCount=itemCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=__FILE__)) return
!
      if (.not. allocated(itemNameList)) then
        allocate(itemNameList(itemCount))
      end if
!
      call ESMF_StateGet(importState, itemNameList=itemNameList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=__FILE__)) return
!
!-----------------------------------------------------------------------
!     Loop over import fields
!-----------------------------------------------------------------------
!
      j = 1
      do i = 1, itemCount
!
!-----------------------------------------------------------------------
!     Get field from import state
!-----------------------------------------------------------------------
!
      call ESMF_StateGet(importState, trim(itemNameList(i)),            &
                         field, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=__FILE__)) return
!
!-----------------------------------------------------------------------
!     Query underlying grid
!-----------------------------------------------------------------------
!
      call ESMF_FieldGet(field, grid=grid, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=__FILE__)) return
!
      call ESMF_GridGet(grid, name=gname, distgrid=distgrid, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=__FILE__)) return
!
!-----------------------------------------------------------------------
!     Query distgrid to get rank and tile information
!-----------------------------------------------------------------------
!
      call ESMF_DistGridGet(distgrid, dimCount=dimCount,                &
                            tileCount=tileCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=__FILE__)) return
!
!-----------------------------------------------------------------------
!     Change grid name to be consistent with input ports of Catalyst
!     NOTE: Component grids must be named as ocn_grid2d, ocn_grid3d, ...
!-----------------------------------------------------------------------
!
      if (dimCount == 2) then
        gname = replace_str(gname, "_grid2d", "_input2d")
      else
        gname = replace_str(gname, "_grid3d", "_input3d")
      end if
!
!-----------------------------------------------------------------------
!     allocate minIndexPTile, maxIndexPTile and connectionList
!-----------------------------------------------------------------------
!
      if (.not. allocated(minIndexPTile)) then
        allocate(minIndexPTile(dimCount, tileCount))
      end if
!
      if (.not. allocated(maxIndexPTile)) then
        allocate(maxIndexPTile(dimCount, tileCount)) 
      end if
!
!-----------------------------------------------------------------------
!     Get minIndex and maxIndex arrays
!-----------------------------------------------------------------------
!
      call ESMF_GridGetCoord(grid, coordDim=1, array=arrX, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=__FILE__)) return
!
      call ESMF_ArrayGet(arrX, minIndexPTile=minIndexPTile,             &
                         maxIndexPTile=maxIndexPTile, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=__FILE__)) return
!
!-----------------------------------------------------------------------
!     Define right and top extents
!     The data will be stored as Point in VTK and it requires overlap in
!     top and right most cells in two dimensional decomposition
!-----------------------------------------------------------------------
!
      tileX = models(Icopro)%tile(1)
      tileY = models(Icopro)%tile(2)
!
      hasRight = .false.
      if ((localPet/tileY+1) < tileX) hasRight = .true.
!
      hasTop = .false.
      if (mod(localPet+1,tileY) /= 0) hasTop = .true.
!
!-----------------------------------------------------------------------
!     Define grid in co-processor side if it is not already defined
!-----------------------------------------------------------------------
!
      if (.not. check_grid(gname)) then
!
!-----------------------------------------------------------------------
!     Collect grid coordinates to define in co-processing module
!-----------------------------------------------------------------------
!
      if (dimCount == 2) then ! 2d
!
      if (enablePerfCheck) then
        call ESMF_VMWtime(stime, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
                               line=__LINE__, file=__FILE__)) return
      end if
!
      call ESMF_GridGetCoord(grid, coordDim=1, farrayPtr=ptr2X, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=__FILE__)) return
!
      call ESMF_GridGetCoord(grid, coordDim=1, array=arrX, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=__FILE__)) return
!
      if (.not. allocated(lon2d)) then
        allocate(lon2d(minIndexPTile(1,1):maxIndexPTile(1,1),           &
                       minIndexPTile(2,1):maxIndexPTile(2,1)))
      end if
!
      do k = 1, models(Icopro)%nPets
        call ESMF_ArrayGather(arrX, farray=lon2d,                       &
                              rootPet=k-1, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
                               line=__LINE__, file=__FILE__)) return
      end do
!
      call ESMF_GridGetCoord(grid, coordDim=2, farrayPtr=ptr2Y,         &
                             computationalCount=cc2d, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=__FILE__)) return
!
      call ESMF_GridGetCoord(grid, coordDim=2, array=arrY, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=__FILE__)) return
!
      if (.not. allocated(lat2d)) then
        allocate(lat2d(minIndexPTile(1,1):maxIndexPTile(1,1),           &
                       minIndexPTile(2,1):maxIndexPTile(2,1)))
      end if
!
      do k = 1, models(Icopro)%nPets
        call ESMF_ArrayGather(arrY, farray=lat2d,                       &
                              rootPet=k-1, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
                               line=__LINE__, file=__FILE__)) return
      end do
!
!-----------------------------------------------------------------------
!     Calculate total number of 2d points in each MPI process
!-----------------------------------------------------------------------
!
      nPoints2D = 1
      if (hasTop) cc2d(1) = cc2d(1)+1
      if (hasRight) cc2d(2) = cc2d(2)+1
      do k = 1, 2
        nPoints2D = nPoints2D*cc2d(k)
      end do
!
!-----------------------------------------------------------------------
!     Serialize (2d -> 1d) coordinate arrays 
!-----------------------------------------------------------------------
!
      lb = (/ lbound(ptr2X,dim=1), lbound(ptr2X,dim=2), 0 /)
      ub = (/ ubound(ptr2X,dim=1), ubound(ptr2X,dim=2), 0 /)
!
      if (hasTop) ub(1) = ub(1)+1
      if (hasRight) ub(2) = ub(2)+1
!
      if (debugLevel > 1) then
        if (localPet == 0) then
        write(*,fmt="(A)") "---------------------------------------"
        write(*,fmt="(A)") trim(to_upper(gname))//" GRID DEFINITION"
        write(*,fmt="(A)") "---------------------------------------"
        end if
        write(*,fmt="(I3,4I5,2I8,I10,2I5,2L3)") localPet, lb(1), ub(1), &
                             lb(2), ub(2), cc2d(1), cc2d(2), nPoints2D, &
                             maxIndexPTile(1,1),maxIndexPTile(2,1),     &
                             hasRight, hasTop
      end if
!
      if (.not. allocated(lon1d)) then
        allocate(lon1d(nPoints2D))
        allocate(lat1d(nPoints2D))
      end if
!
      call ntooned_2d(lb(1:2), ub(1:2),                                 &
                      lon2d(lb(1):ub(1),lb(2):ub(2)), lon1d)
      call ntooned_2d(lb(1:2), ub(1:2),                                 &
                      lat2d(lb(1):ub(1),lb(2):ub(2)), lat1d)
!
!-----------------------------------------------------------------------
!     Define grid (2d) in co-porcessing side via Catalyst adaptor
!-----------------------------------------------------------------------
!
      dims(1) = maxIndexPTile(1,1)-minIndexPTile(1,1)+1
      dims(2) = maxIndexPTile(2,1)-minIndexPTile(2,1)+1
      dims(3) = 0
!
      flag = catalyst_create_grid(0, 0.0d0, trim(gname)//char(0),       &
                                  petCount, localPet, dims,             &
                                  lb, ub, nPoints2D,                    &
                                  lon1d, lat1d)
!
!-----------------------------------------------------------------------
!     Nullify pointer to make sure that it does not point on a random
!     part in the memory
!-----------------------------------------------------------------------
!
      if (associated(ptr2X)) nullify(ptr2X)
      if (associated(ptr2Y)) nullify(ptr2Y)
!
!-----------------------------------------------------------------------
!     Deallocate temporary arrays to free the memory
!-----------------------------------------------------------------------
!
      if (allocated(lon1d)) deallocate(lon1d)
      if (allocated(lat1d)) deallocate(lat1d)
      if (allocated(lon2d)) deallocate(lon2d)
      if (allocated(lat2d)) deallocate(lat2d)
!
!-----------------------------------------------------------------------
!     Destroy temorary arrays
!-----------------------------------------------------------------------
!
      call ESMF_ArrayDestroy(arrX, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=__FILE__)) return
      call ESMF_ArrayDestroy(arrY, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=__FILE__)) return
!
!-----------------------------------------------------------------------
!     Measure performance of creating 2d grid 
!-----------------------------------------------------------------------
!
      if (enablePerfCheck) then
        call ESMF_VMWtime(etime, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
                               line=__LINE__, file=__FILE__)) return
        if (localPet == 0) then
          write(*,fmt="(A,F15.8,A)") "[PERFLOG] :: CREATE "//           &
               trim(to_upper(gname)), etime-stime, " SEC."
        end if
      end if
!
      else if (dimCount == 3) then ! 3d
!
      if (enablePerfCheck) then
        call ESMF_VMWtime(stime, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
                               line=__LINE__, file=__FILE__)) return
      end if
!
      call ESMF_GridGetCoord(grid, coordDim=1, farrayPtr=ptr3X, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=__FILE__)) return
!
      call ESMF_GridGetCoord(grid, coordDim=1, array=arrX, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=__FILE__)) return
!
      call ESMF_ArrayGet(arrX, minIndexPTile=minIndexPTile,             &
                         maxIndexPTile=maxIndexPTile, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=__FILE__)) return
!
      if (.not. allocated(lon3d)) then
        allocate(lon3d(minIndexPTile(1,1):maxIndexPTile(1,1),           &
                       minIndexPTile(2,1):maxIndexPTile(2,1),           &
                       minIndexPTile(3,1):maxIndexPTile(3,1)))
      end if
!
      do k = 1, models(Icopro)%nPets
        call ESMF_ArrayGather(arrX, farray=lon3d,                       &
                              rootPet=k-1, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
                               line=__LINE__, file=__FILE__)) return
      end do
!
      call ESMF_GridGetCoord(grid, coordDim=2, farrayPtr=ptr3Y, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=__FILE__)) return
!
      call ESMF_GridGetCoord(grid, coordDim=2, array=arrY, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=__FILE__)) return
!
      if (.not. allocated(lat3d)) then
        allocate(lat3d(minIndexPTile(1,1):maxIndexPTile(1,1),           &
                       minIndexPTile(2,1):maxIndexPTile(2,1),           &
                       minIndexPTile(3,1):maxIndexPTile(3,1)))
      end if
!
      do k = 1, models(Icopro)%nPets
        call ESMF_ArrayGather(arrY, farray=lat3d,                       &
                              rootPet=k-1, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
                               line=__LINE__, file=__FILE__)) return
      end do
!
      call ESMF_GridGetCoord(grid, coordDim=3, farrayPtr=ptr3Z,         &
                             computationalCount=cc3d,                   &
                             rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=__FILE__)) return
!
      call ESMF_GridGetCoord(grid, coordDim=3, array=arrZ, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=__FILE__)) return
!                             
      if (.not. allocated(lev3d)) then
        allocate(lev3d(minIndexPTile(1,1):maxIndexPTile(1,1),           &
                       minIndexPTile(2,1):maxIndexPTile(2,1),           &
                       minIndexPTile(3,1):maxIndexPTile(3,1)))
      end if
!
      do k = 1, models(Icopro)%nPets
        call ESMF_ArrayGather(arrZ, farray=lev3d,                       &
                              rootPet=k-1, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
                               line=__LINE__, file=__FILE__)) return
      end do
!
!-----------------------------------------------------------------------
!     Calculate total number of 3d points in each MPI process
!-----------------------------------------------------------------------
!
      nPoints3D = 1
      if (hasRight) cc3d(2) = cc3d(2)+1
      if (hasTop) cc3d(1) = cc3d(1)+1
      do k = 1, 3
        nPoints3D = nPoints3D*cc3d(k)
      end do
!
!-----------------------------------------------------------------------
!     Serialize (3d -> 1d) coordinate arrays 
!-----------------------------------------------------------------------
!
      lb = (/ lbound(ptr3X,dim=1), lbound(ptr3X,dim=2),                 &
              lbound(ptr3X,dim=3) /)
      ub = (/ ubound(ptr3X,dim=1), ubound(ptr3X,dim=2),                 &
              ubound(ptr3X,dim=3) /)
!
      if (hasRight) ub(2) = ub(2)+1
      if (hasTop) ub(1) = ub(1)+1
!
      if (debugLevel > 1) then
        if (localPet == 0) then
        write(*,fmt="(A)") "---------------------------------------"
        write(*,fmt="(A)") trim(to_upper(gname))//" GRID DEFINITION"
        write(*,fmt="(A)") "---------------------------------------"
        end if
        write(*,fmt="(I3,6I5,3I8,I10,3I5,2L3)") localPet, lb(1), ub(1), &
                             lb(2), ub(2), lb(3), ub(3),                &
                             cc3d(1), cc3d(2), cc3d(3), nPoints3D,      &
                             maxIndexPTile(1,1),maxIndexPTile(2,1),     &
                             maxIndexPTile(3,1), hasRight, hasTop
      end if
!
      if (.not. allocated(lon1d)) then
        allocate(lon1d(nPoints3D))
        allocate(lat1d(nPoints3D))
        allocate(lev1d(nPoints3D))
      end if
!
      call ntooned_3d(lb, ub,                                           &
                      lon3d(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3)), lon1d)
      call ntooned_3d(lb, ub,                                           &
                      lat3d(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3)), lat1d)
      call ntooned_3d(lb, ub,                                           &
                      lev3d(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3)), lev1d)
!
!-----------------------------------------------------------------------
!     Define grid (3d) in co-porcessing side via Catalyst adaptor
!-----------------------------------------------------------------------
!
      dims(1) = maxIndexPTile(1,1)-minIndexPTile(1,1)+1
      dims(2) = maxIndexPTile(2,1)-minIndexPTile(2,1)+1
      dims(3) = maxIndexPTile(3,1)-minIndexPTile(3,1)+1
!
      flag = catalyst_create_grid(0, 0.0d0, trim(gname)//char(0),       &
                                  petCount, localPet, dims,             &
                                  lb, ub, nPoints3D,                    &
                                  lon1d, lat1d, lev1d)
!
!-----------------------------------------------------------------------
!     Nullify pointer to make sure that it does not point on a random
!     part in the memory
!-----------------------------------------------------------------------
!
      if (associated(ptr3X)) nullify(ptr3X)
      if (associated(ptr3Y)) nullify(ptr3Y)
      if (associated(ptr3Z)) nullify(ptr3Z)
!
!-----------------------------------------------------------------------
!     Deallocate temporary arrays to free the memory
!-----------------------------------------------------------------------
!
      if (allocated(lon1d)) deallocate(lon1d)
      if (allocated(lat1d)) deallocate(lat1d)
      if (allocated(lev1d)) deallocate(lev1d)
      if (allocated(lon3d)) deallocate(lon3d)
      if (allocated(lat3d)) deallocate(lat3d)
      if (allocated(lev3d)) deallocate(lev3d)
!
!-----------------------------------------------------------------------
!     Destroy temorary arrays
!-----------------------------------------------------------------------
!
      call ESMF_ArrayDestroy(arrX, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=__FILE__)) return
      call ESMF_ArrayDestroy(arrY, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=__FILE__)) return
      call ESMF_ArrayDestroy(arrZ, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=__FILE__)) return
!
!-----------------------------------------------------------------------
!     Measure performance of creating 3d grid 
!-----------------------------------------------------------------------
!
      if (enablePerfCheck) then
        call ESMF_VMWtime(etime, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
                               line=__LINE__, file=__FILE__)) return
        if (localPet == 0) then
          write(*,fmt="(A,F15.8,A)") "[PERFLOG] :: CREATE "//           &
               trim(to_upper(gname)), etime-stime, " SEC."
        end if
      end if
!
      end if
!
!-----------------------------------------------------------------------
!     Create routehandle for halo update 
!-----------------------------------------------------------------------
!
      call ESMF_FieldHaloStore(field, routehandle=routeHandle(j), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      rname = trim(gname)//'_uphalo'
!
      call ESMF_RouteHandleSet(routeHandle(j), name=rname, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Save defined grid name in lookup table 
!-----------------------------------------------------------------------
!
      gridnames(j) = trim(gname)
      j = j+1
!
!-----------------------------------------------------------------------
!     Debug: write out component grid in VTK format 
!-----------------------------------------------------------------------
!
      if (debugLevel > 1) then
      call ESMF_GridWriteVTK(grid,filename="coproc_"//trim(gname),rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,&
                             line=__LINE__, file=FILENAME)) return
      end if
!
      end if
!
!-----------------------------------------------------------------------
!     Perform halo update 
!-----------------------------------------------------------------------
!
      do k = 1, ubound(routeHandle, dim=1)
        if (ESMF_RouteHandleIsCreated(routeHandle(k))) then
          call ESMF_RouteHandleGet(routeHandle(k), name=rname, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,&
                                 line=__LINE__, file=__FILE__)) return
!
          if (trim(rname) == trim(gname)//'_uphalo') then
            call ESMF_FieldHalo(field, routehandle=routeHandle(k),      &
                                checkflag=.true., rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc,                        &
                                   msg=ESMF_LOGERR_PASSTHRU,            &
                                   line=__LINE__, file=__FILE__)) return
          end if
        end if
      end do
!
!-----------------------------------------------------------------------
!     Get pointer from field and serialize data to pass co-processing
!-----------------------------------------------------------------------
!
      if (enablePerfCheck) then
        call ESMF_VMWtime(stime, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
                               line=__LINE__, file=__FILE__)) return
      end if
!
      if (dimCount == 2) then ! 2d
!
      call ESMF_FieldGet(field, localDE=0, farrayPtr=ptr2d, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=__FILE__)) return
!
!-----------------------------------------------------------------------
!     Serialize (2d -> 1d) coordinate arrays 
!-----------------------------------------------------------------------
!
      lb = (/ lbound(ptr2d,dim=1), lbound(ptr2d,dim=2), 0 /)
      ub = (/ ubound(ptr2d,dim=1), ubound(ptr2d,dim=2), 0 /)
!
      nPoints2D = 1
      do k = 1, 2
        nPoints2D = nPoints2D*(ub(k)-lb(k)+1)
      end do
!
      if (.not. allocated(var1d)) then
        allocate(var1d(nPoints2D))
      end if
!
      call ntooned_2d(lb(1:2), ub(1:2), ptr2d, var1d)
!
!-----------------------------------------------------------------------
!     Add field to co-processor
!-----------------------------------------------------------------------
!
      gname = replace_str(gname, "_grid2d", "_input2d")
!
      call add_scalar(var1d, trim(itemNameList(i))//char(0), nPoints2D, &
                      petCount, localPet, trim(gname)//char(0))
!
!-----------------------------------------------------------------------
!     Nullify pointer to make sure that it does not point on a random
!     part in the memory
!-----------------------------------------------------------------------
!
      if (associated(ptr2d)) nullify(ptr2d)
!
!-----------------------------------------------------------------------
!     Deallocate temporary arrays to free the memory
!-----------------------------------------------------------------------
!
      if (allocated(var1d)) deallocate(var1d)
!
      if (enablePerfCheck) then
        call ESMF_VMWtime(etime, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
                               line=__LINE__, file=__FILE__)) return
        if (localPet == 0) then
          write(*,fmt="(A,I10,F15.8,A)") "[PERFLOG] :: ADD FIELD "//    &
          trim(to_upper(itemNameList(i))), its, etime-stime, " SEC."
        end if
      end if
!
      else if (dimCount == 3) then ! 3d     
!
      call ESMF_FieldGet(field, localDE=0, farrayPtr=ptr3d, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=__FILE__)) return
!
!-----------------------------------------------------------------------
!     Serialize (3d -> 1d) coordinate arrays 
!-----------------------------------------------------------------------
!
      lb = (/ lbound(ptr3d,dim=1), lbound(ptr3d,dim=2),                 &
              lbound(ptr3d,dim=3) /)
      ub = (/ ubound(ptr3d,dim=1), ubound(ptr3d,dim=2),                 &
              ubound(ptr3d,dim=3) /)
!
      nPoints3D = 1
      do k = 1, 3
        nPoints3D = nPoints3D*(ub(k)-lb(k)+1)
      end do
!
      if (.not. allocated(var1d)) then
        allocate(var1d(nPoints3D))
      end if
!
      call ntooned_3d(lb, ub, ptr3d, var1d)
!
!-----------------------------------------------------------------------
!     Add field to Catalyst
!-----------------------------------------------------------------------
!
      gname = replace_str(gname, "_grid3d", "_input3d")
!
      call add_scalar(var1d, trim(itemNameList(i))//char(0), nPoints3D, &
                      petCount, localPet, trim(gname)//char(0))
!
!-----------------------------------------------------------------------
!     Nullify pointer to make sure that it does not point on a random
!     part in the memory
!-----------------------------------------------------------------------
!
      if (associated(ptr3d)) nullify(ptr3d)
!
!-----------------------------------------------------------------------
!     Deallocate temporary arrays to free the memory
!-----------------------------------------------------------------------
!
      if (allocated(var1d)) deallocate(var1d)
!
      if (enablePerfCheck) then
        call ESMF_VMWtime(etime, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
                               line=__LINE__, file=__FILE__)) return
        if (localPet == 0) then
          write(*,fmt="(A,I10,F15.8,A)") "[PERFLOG] :: ADD FIELD "//    &
          trim(to_upper(itemNameList(i))), its, etime-stime, " SEC."
        end if
      end if
!
      end if
!
!-----------------------------------------------------------------------
!     Deallocate temporary arrays to free the memory
!-----------------------------------------------------------------------
!
      if (allocated(minIndexPTile)) deallocate(minIndexPTile)
      if (allocated(maxIndexPTile)) deallocate(maxIndexPTile)
!
      end do
!
!-----------------------------------------------------------------------
!     Deallocate arrays    
!-----------------------------------------------------------------------
!
      if (allocated(itemNameList)) deallocate(itemNameList)
!
!-----------------------------------------------------------------------
!     Call co-processing
!-----------------------------------------------------------------------
!
      if (enablePerfCheck) then
        call ESMF_VMWtime(stime, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
                               line=__LINE__, file=__FILE__)) return
      end if
!
      call my_requestdatadescription(its, dtime, flag)
      if (flag) then
        call my_coprocess()
      end if
!
      if (enablePerfCheck) then
        call ESMF_VMWtime(etime, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
                               line=__LINE__, file=__FILE__)) return
        if (localPet == 0) then
          write(*,fmt="(A,I10,F15.8,A)") "[PERFLOG] :: COPROC ", its,   &
               etime-stime, "SEC."
        end if
      end if
!
!-----------------------------------------------------------------------
!     Debug: write time information 
!-----------------------------------------------------------------------
!
      if (debugLevel >= 0 .and. localPet == 0) then
        write(*,40) trim(str1), trim(str2), its, dtime
      end if
!
!-----------------------------------------------------------------------
!     Formats 
!-----------------------------------------------------------------------
!
 40   format(' Running COP Component: ',A,' --> ',A,                    &
             ' Time Step:',I6,' [',F15.2,']')
!
      end subroutine COP_ModelAdvance
!
      function check_grid(gname) result(flag)
      implicit none
!     
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
      character(ESMF_MAXSTR), intent(in) :: gname
      logical :: flag
!
!-----------------------------------------------------------------------
!     Local variable declarations 
!-----------------------------------------------------------------------
!
      integer :: i
!
      flag = .false.
      do i = 1, ubound(gridnames, dim=1)
        if (trim(gname) == trim(gridnames(i))) flag = .true.
      end do
!  
      end function check_grid
!
      subroutine ntooned_2d(lb, ub, xnd, x1d)
      implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations
!-----------------------------------------------------------------------
!
      integer, intent(in) :: lb(2)
      integer, intent(in) :: ub(2)
      real*8 , intent(in) :: xnd(lb(1):ub(1),lb(2):ub(2))
      real*8 , intent(inout) :: x1d(:)
!
!-----------------------------------------------------------------------
!     Local variable declarations
!-----------------------------------------------------------------------
!
      integer :: i, j, p
!
      p = 1
      do j = lb(2), ub(2)
        do i = lb(1), ub(1)
          x1d(p) = xnd(i,j)
          p = p+1
        end do
      end do
!
      end subroutine ntooned_2d
!
      subroutine ntooned_3d(lb, ub, xnd, x1d)
      implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations
!-----------------------------------------------------------------------
!
      integer, intent(in) :: lb(3)
      integer, intent(in) :: ub(3)
      real*8 , intent(in) :: xnd(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3))
      real*8 , intent(inout) :: x1d(:)
!
!-----------------------------------------------------------------------
!     Local variable declarations
!-----------------------------------------------------------------------
!
      integer :: i, j, k, p
!
      p = 1
      do k = lb(3), ub(3)
        do j = lb(2), ub(2)
          do i = lb(1), ub(1)
            x1d(p) = xnd(i,j,k)
            p = p+1
          end do
        end do
      end do
!
      end subroutine ntooned_3d
!
      function catalyst_create_grid(step,                               &
                                    time,                               &
                                    name,                               &
                                    petCount,                           &
                                    localPet,                           &
                                    dims,                               &
                                    lb,                                 &
                                    ub,                                 &
                                    nPoints,                            &
                                    lon1d,                              &
                                    lat1d,                              &
                                    lev1d)                              &
                                    result(continueProcessing)
      implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations
!-----------------------------------------------------------------------
!
      integer, intent(in) :: step
      real*8, intent(in) :: time
      character(len=*), intent(in) :: name
      integer, intent(in) :: petCount
      integer, intent(in) :: localPet
      integer, intent(in) :: dims(:)
      integer, intent(in) :: lb(:)
      integer, intent(in) :: ub(:)
      integer, intent(in) :: nPoints
      real*8, intent(in)  :: lon1d(nPoints)
      real*8, intent(in)  :: lat1d(nPoints)
      real*8, intent(in), optional  :: lev1d(nPoints)
!
!-----------------------------------------------------------------------
!     Local variable declarations
!-----------------------------------------------------------------------
!      
      logical :: flag1, continueProcessing
      integer :: flag2
!
!-----------------------------------------------------------------------
!     Create grid in co-processing module 
!-----------------------------------------------------------------------
!
      flag1 = .false.
      flag2 = 0
      continueProcessing = .false.
!
      call my_requestdatadescription(step, time, flag1)
      if (flag1) then
        continueProcessing = .true.
        call my_needtocreategrid(flag2, name//char(0))
        if (present(lev1d)) then
          call create_grid(name//char(0), petCount, localPet,           &
                           dims, lb, ub, nPoints, lon1d, lat1d, lev1d)
        else
          call create_grid(name//char(0), petCount, localPet,           &
                           dims, lb, ub, nPoints, lon1d, lat1d)
        end if
        !write(*,fmt="(A,6I5,I8)") name//char(0), lb, ub, nPoints 
      else
        continueProcessing = .false.
      end if
!
      end function catalyst_create_grid
!
      end module mod_esmf_cop

