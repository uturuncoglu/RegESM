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
#define FILENAME "util/mod_config.F90" 
!
!-----------------------------------------------------------------------
!     Module for ESM configuration file 
!-----------------------------------------------------------------------
!
      module mod_config
!
!-----------------------------------------------------------------------
!     Used module declarations 
!-----------------------------------------------------------------------
!
      use ESMF
      use NUOPC
!
      use mod_types
!
      implicit none
      contains
!
      subroutine read_config(vm, rc)
      implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
      type(ESMF_VM), intent(in) :: vm
      integer, intent(out) :: rc 
!
!-----------------------------------------------------------------------
!     Local variable declarations 
!-----------------------------------------------------------------------
!
      integer :: time(6)
      integer :: i, j, k, p, np, dumm
      integer :: localPet, petCount, lineCount, columnCount
      integer, allocatable :: petList(:)
      logical :: file_exists
      character(100) :: fmt_123, str
!
      type(ESMF_Config) :: cf
      type(ESMF_CalKind_Flag) :: cflag
!
      rc = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!     Query gridded component
!-----------------------------------------------------------------------
!
      call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Read configuration file 
!-----------------------------------------------------------------------
!
      inquire(file=trim(config_fname), exist=file_exists)
!
      if (file_exists) then
!
      cf = ESMF_ConfigCreate(rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      call ESMF_ConfigLoadFile(cf, trim(config_fname), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Get run mode (concurrent vs. sequential) 
!-----------------------------------------------------------------------
! 
      call ESMF_ConfigFindLabel(cf, 'PETLayoutOption:', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      call ESMF_ConfigGetAttribute(cf, petLayoutOption, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      runMod = Iseq
      if (trim(petLayoutOption) == 'concurrent') runMod = Ipar
      if (localPet == 0) then
        write(*, fmt='(A12,A)') "PET Layout: ", trim(RUNNDES(runMod)) 
      end if
!
!-----------------------------------------------------------------------
!     Get number of component (or model) 
!-----------------------------------------------------------------------
!        
      nModels = ESMF_ConfigGetLen(cf, label='PETs:', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      if (nModels .gt. 0) then
        if (.not. allocated(models)) allocate(models(nModels))
      end if
!
!-----------------------------------------------------------------------
!     Set active components (if nPets > 0 active otherwise not) 
!-----------------------------------------------------------------------
!
      call ESMF_ConfigFindLabel(cf, 'PETs:', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      do i = 1, nModels
        call ESMF_ConfigGetAttribute(cf, models(i)%nPets, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
            line=__LINE__, file=FILENAME)) return
!
        ! check: force river routing model will use single PET      
        if (i == Iriver .and. models(i)%nPets /= 0) models(i)%nPets = -1
!
        if (i == Iatmos) then
          models(i)%name = "ATM"
        else if (i == Iocean) then
          models(i)%name = "OCN"
        else if (i == Iriver) then
          models(i)%name = "RTM"
        end if
!
        models(i)%modActive = .false.
        if (models(i)%nPets /= 0) models(i)%modActive = .true.
      end do
!
!-----------------------------------------------------------------------
!     Set debug level 
!-----------------------------------------------------------------------
!
      call ESMF_ConfigGetAttribute(cf, debugLevel,                      &
                                   label='DebugLevel:', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Set calendar 
!-----------------------------------------------------------------------
!
      call ESMF_ConfigGetAttribute(cf, str, label='Calendar:', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      if (trim(str) == 'gregorian') cflag = ESMF_CALKIND_GREGORIAN
      if (trim(str) == 'noleap'   ) cflag = ESMF_CALKIND_NOLEAP
      if (trim(str) == '360_day'  ) cflag = ESMF_CALKIND_360DAY
!
      esmCal = ESMF_CalendarCreate(cflag, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Set application clock 
!-----------------------------------------------------------------------
!
      call ESMF_ConfigGetAttribute(cf, time, count=6,                   &
                                   label='StartTime:', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
      call ESMF_TimeSet(esmStartTime,                                   &
                        yy=time(1), mm=time(2), dd=time(3),             &
                        h=time(4), m=time(5), s=time(6),                &
                        calkindflag=cflag, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      call ESMF_ConfigGetAttribute(cf, time, count=6,                   &
                                   label='RestartTime:', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
      call ESMF_TimeSet(esmRestartTime,                                 &
                        yy=time(1), mm=time(2), dd=time(3),             &
                        h=time(4), m=time(5), s=time(6),                &
                        calkindflag=cflag, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      call ESMF_ConfigGetAttribute(cf, time, count=6,                   &
                                   label='StopTime:', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
      call ESMF_TimeSet(esmStopTime,                                    &
                        yy=time(1), mm=time(2), dd=time(3),             &
                        h=time(4), m=time(5), s=time(6),                &
                        calkindflag=cflag, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      call ESMF_ConfigGetAttribute(cf, time, count=6,                   &
                                   label='TimeStep:', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
      call ESMF_TimeIntervalSet(esmTimeStep, calendar=esmCal,           &
                                yy=time(1), mm=time(2), d=time(3),      &
                                h=time(4), m=time(5), s=time(6),        &
                                rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
      else
        write(*,*) "[error] -- Config file '"//trim(config_fname)//"'"//&
                   " is not available! exiting ..."
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      end if
!
!-----------------------------------------------------------------------
!     Assign PETs to model components 
!     River routing model uses latest PET of the ATM component
!-----------------------------------------------------------------------
!
      select case (runMod)
      case (iseq) ! sequential
        do i = 1, nModels
          if ((i == Iatmos) .or. (i == Iocean)) then
            models(i)%nPets = petCount
          else if (i == Iriver) then
            models(i)%nPets = 1
          end if
!
          if (.not. allocated(models(i)%petList)) then
            allocate(models(i)%petList(models(i)%nPets))
          end if
!
          if ((i == Iatmos) .or. (i == Iocean)) then
            models(i)%petList = (/ (k, k = 0, petCount-1) /)
          else if (i == Iriver) then
            models(i)%petList = (/ (k, k = petCount-1, petCount-1) /)
          end if
        end do 
      case (ipar) ! concurent
        k = -1
        do i = 1, nModels
          p = models(i)%nPets
          if (p < 0) p = -p
!
          if (.not. allocated(models(i)%petList)) then
            allocate(models(i)%petList(p))
          end if
!
          do j = 1, p 
            k = k+1
            if (i .eq. Iatmos) then
              models(i)%petList(j) = k 
            else if (i .eq. Iocean) then
              models(i)%petList(j) = k 
            else if (i .eq. Iriver) then
              ! assign last PET, if negative value given
              if (models(i)%nPets < 0) then
                models(i)%petList(j) = k-1
              ! otherwise the component has its own PET
              else
                models(i)%petList(j) = k
              end if
            end if
          end do
        end do
      case default
        call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc,              &
             msg='PetLayoutOption is not supported: '//                 &
             trim(petLayoutOption))
        return
      end select
!
!-----------------------------------------------------------------------
!     Debug: write list of active components 
!-----------------------------------------------------------------------
!
      if (localPet == 0) then
        do i = 1, nModels
          if (models(i)%modActive) then
            k = ubound(models(i)%petList, dim=1)
            write(fmt_123, fmt="('(A10, L, ', I3, 'I4)')") k
            write(*, fmt=trim(fmt_123)) trim(models(i)%name)//'    ',   &
                  models(i)%modActive, models(i)%petList
          end if
        end do
      end if
!
!-----------------------------------------------------------------------
!     Assign PETs to connectors 
!-----------------------------------------------------------------------
!
      if (.not. allocated(connectors)) then
        allocate(connectors(nModels, nModels))
      end if
!
      do i = 1, nModels
        do j = 1, nModels
          connectors(i,j)%modActive = .false.
          if (models(i)%modActive .and.                                 &
              models(j)%modActive .and. (i /= j)) then
            ! set name of connector component
            connectors(i,j)%name = trim(models(i)%name)//"-TO-"//       &
                                   trim(models(j)%name)
!
            ! activate connector
            connectors(i,j)%modActive = .true.
!
            ! allocate temporary variable to store PET list
            np = size(models(i)%petList)+size(models(j)%petList) 
            if (.not. allocated(petList)) allocate(petList(np))
            petList(1:size(models(i)%petList)) = models(i)%petList
            petList(size(models(i)%petList)+1:) = models(j)%petList
!            
            ! assign PETs to connector
            connectors(i,j)%nPets = np
            if (.not. allocated(connectors(i,j)%petList)) then
              allocate(connectors(i,j)%petList(np))
            end if
            connectors(i,j)%petList = petList
!
            ! deallocate temporary variables
            if (allocated(petList)) deallocate(petList)
          end if
        end do
      end do
!
!-----------------------------------------------------------------------
!     Fix active connectors (put exceptions in here)
!     - no interaction between RTM-ATM and OCN-RTM components
!-----------------------------------------------------------------------
!
      connectors(Iriver,Iatmos)%modActive = .false.
      connectors(Iocean,Iriver)%modActive = .false.
!
!-----------------------------------------------------------------------
!     Set interface for connector
!-----------------------------------------------------------------------
!
      connectors(:,:)%modInteraction = Ioverall
!
      connectors(Iatmos,Iocean)%modInteraction = Ioverocn
      connectors(Iocean,Iatmos)%modInteraction = Ioverocn      
      connectors(Iatmos,Iriver)%modInteraction = Ioverlnd
      connectors(Iriver,Iocean)%modInteraction = Ioverlnd
!
!-----------------------------------------------------------------------
!     Initialize extrapolation option
!-----------------------------------------------------------------------
!
      connectors(:,:)%modExtrapolation = .false. 
!
!-----------------------------------------------------------------------
!     Debug: write list of active connectors
!-----------------------------------------------------------------------
!
      if (localPet == 0) then
        do i = 1, nModels
          do j = 1, nModels
            if (connectors(i,j)%modActive) then
              k = ubound(connectors(i,j)%petList, dim=1)
              write(fmt_123, fmt="('(A10, L, ', I3, 'I4)')") k
              write(*, fmt=trim(fmt_123)) connectors(i,j)%name,         &
                    connectors(i,j)%modActive, connectors(i,j)%petList
            end if
          end do
        end do
      end if
!
!-----------------------------------------------------------------------
!     Read time step divider for data exchange among models
!     Coupling interval could be different among components
!-----------------------------------------------------------------------
!
      call ESMF_ConfigFindLabel(cf, 'DividerForTStep::', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      do i = 1, nModels
        call ESMF_ConfigNextLine(cf, rc=rc)
        do j = 1, nModels      
          call ESMF_ConfigGetAttribute(cf, connectors(i,j)%divDT, rc=rc)
          if (localPet == 0 .and. connectors(i,j)%modActive) then
          write(*,20) trim(connectors(i,j)%name), connectors(i,j)%divDT
          end if
        end do
      end do
!
!-----------------------------------------------------------------------
!     Read exchange field table (based on active components)
!     - must be in same directory with executable
!-----------------------------------------------------------------------
!
      call read_field_table('exfield.tbl', localPet, rc)
!
!-----------------------------------------------------------------------
!     Read river option (only active when RTM is activated) 
!-----------------------------------------------------------------------
!
      if (models(Iriver)%modActive) then
        call ESMF_ConfigGetAttribute(cf, riverOpt,                      &
                                     label='RiverOpt:', rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
            line=__LINE__, file=FILENAME)) return
!
      end if
!
!-----------------------------------------------------------------------
!     Read river list for coupling (only active when RTM is activated) 
!-----------------------------------------------------------------------
!
      if (models(Iriver)%modActive) then  
        call ESMF_ConfigGetDim(cf, lineCount, columnCount,              &
                               label='RiverList::', rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
            line=__LINE__, file=FILENAME)) return
!
        call ESMF_ConfigFindLabel(cf, 'RiverList::', rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
            line=__LINE__, file=FILENAME)) return
!
        if (.not. allocated(rivers)) allocate(rivers(lineCount))
!
        do i = 1, lineCount
          call ESMF_ConfigNextLine(cf, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,&
              line=__LINE__, file=FILENAME)) return
!
          call ESMF_ConfigGetAttribute(cf, dumm, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,&
              line=__LINE__, file=FILENAME)) return
          rivers(i)%asIndex = .false.
          if (dumm == 0) rivers(i)%asIndex = .true. 
!
          call ESMF_ConfigGetAttribute(cf, rivers(i)%isActive, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,&
              line=__LINE__, file=FILENAME)) return
!
          call ESMF_ConfigGetAttribute(cf, rivers(i)%eRadius, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,&
              line=__LINE__, file=FILENAME)) return
!
          if (rivers(i)%asIndex) then
          call ESMF_ConfigGetAttribute(cf, rivers(i)%iindex, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,&
              line=__LINE__, file=FILENAME)) return
!
          call ESMF_ConfigGetAttribute(cf, rivers(i)%jindex, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,&
              line=__LINE__, file=FILENAME)) return
          else
          call ESMF_ConfigGetAttribute(cf, rivers(i)%lon, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,&
              line=__LINE__, file=FILENAME)) return
!
          call ESMF_ConfigGetAttribute(cf, rivers(i)%lat, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,&
              line=__LINE__, file=FILENAME)) return
          end if 
!
          call ESMF_ConfigGetAttribute(cf, rivers(i)%dir, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,&
              line=__LINE__, file=FILENAME)) return
!
          call ESMF_ConfigGetAttribute(cf, rivers(i)%npoints, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,&
              line=__LINE__, file=FILENAME)) return
!
          do j = 1, 12
          call ESMF_ConfigGetAttribute(cf, rivers(i)%monfac(j), rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,&
              line=__LINE__, file=FILENAME)) return
          end do
        end do
      end if
!
!-----------------------------------------------------------------------
!     Format definition 
!-----------------------------------------------------------------------
!
 20   format(A10,1X,'DT DIVIDER: ',I3)
!
      end subroutine read_config
!
      subroutine read_field_table(ifile, localPet, rc)
      implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
      character(len=*), intent(in) :: ifile
      integer, intent(in) :: localPet
      integer, intent(out) :: rc
!
!-----------------------------------------------------------------------
!     Local variable declarations 
!-----------------------------------------------------------------------
!
      integer :: iunit = 10
      integer :: i, j, k, l, m, n, s, ios1, ios2, pos1, pos2, nf
      logical :: extp, file_exists
      character(len=400) :: str
      character(len=200) :: dum(10)
      logical :: flag
!
      rc = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!     Read field table
!-----------------------------------------------------------------------
!
      inquire(file=trim(ifile), exist=file_exists)
!
      if (file_exists) then
        open(unit=iunit, file=trim(ifile), status='old')
        ios1 = 0
        do while (ios1 == 0)
          ! read header
          read(iunit,*,iostat=ios1) nf, str, extp
          if (ios1 /= 0) exit
          ! define gridded components for import and export 
          select case (trim(str))
            case('atm2ocn')
              i = Iatmos
              j = Iocean
            case('ocn2atm')
              i = Iocean
              j = Iatmos
            case('atm2rtm')
              i = Iatmos
              j = Iriver
            case('rtm2ocn')
              i = Iriver
              j = Iocean
            case default
              write(*,*) '[error] -- Undefined components: '//trim(str)
              call ESMF_Finalize(endflag=ESMF_END_ABORT)  
          end select   
!
          connectors(i,j)%modExtrapolation = extp
!
          if (debugLevel > 0 .and. localPet == 0) then
          write(*,fmt='(A,I2,A,L)') COMPDES(i)//' -> '//COMPDES(j)//' ',&
                                nf, ' ', extp
          end if
!
          ! loop over fields
          do k = 0, nf-1 
            ! read data line 
            read(iunit,fmt='(A)',iostat=ios2) str
            if (ios2 /= 0) exit
            ! split fields
            s = 1
            pos1 = 1
            do
              pos2 = index(str(pos1:), ':')
              if (pos2 == 0) then
                dum(10) = trim(str(pos1:))
                exit
              else
                dum(s) = trim(str(pos1:pos1+pos2-2))
              end if
              s = s+1
              pos1 = pos2+pos1
            end do
            ! check field is already added to the list or not?
            flag = .true.
            if (allocated(models(i)%exportField)) then
              do l = 1, ubound(models(i)%exportField, dim=1)
                if (trim(dum(1)) ==                                     &
                    trim(models(i)%exportField(l)%short_name)) then
                  flag = .false.
                end if
              end do
            end if
            ! add export field to the list
            if (flag) then
              call add_field(models(i)%exportField, dum, .true.)
              ! print out
              if (debugLevel > 0 .and. localPet == 0) then
              m = ubound(models(i)%exportField, dim=1)
              write(*,30) k, m,                                         &
                 adjustl(trim(models(i)%exportField(m)%short_name)),    &
                 adjustl(trim(models(i)%exportField(m)%long_name)),     &
                 adjustl(trim(models(i)%exportField(m)%units)),         &
                 adjustl(trim(models(i)%exportField(m)%export_units)),  &
                 adjustl(trim(GRIDDES(models(i)%exportField(m)%gtype))),&
                 adjustl(trim(INTPDES(models(i)%exportField(m)%itype))),&
                 models(i)%exportField(m)%scale_factor,                 &
                 models(i)%exportField(m)%add_offset,                   &
                 models(i)%exportField(m)%enable_integral_adj,          &
                 COMPDES(i)//'-EXP'
              end if
            end if
            ! check field is already added to the list or not?
            flag = .true.
            if (allocated(models(j)%importField)) then
              do l = 1, ubound(models(j)%importField, dim=1)
                if (trim(dum(1)) ==                                     &
                    trim(models(j)%importField(l)%short_name)) then
                  flag = .false.
                end if
              end do
            end if 
            ! add import field to the list
            if (flag) then
              call add_field(models(j)%importField, dum, .false.)
              ! print out
              if (debugLevel > 0 .and. localPet == 0) then 
              n = ubound(models(j)%importField, dim=1)
              write(*,30) k, n,                                         &
                 adjustl(trim(models(j)%importField(n)%short_name)),    &
                 adjustl(trim(models(j)%importField(n)%long_name)),     &
                 adjustl(trim(models(j)%importField(n)%units)),         &
                 adjustl(trim(models(j)%importField(n)%export_units)),  &
                 adjustl(trim(GRIDDES(models(j)%importField(n)%gtype))),&
                 adjustl(trim(INTPDES(models(j)%importField(n)%itype))),&
                 models(j)%importField(n)%scale_factor,                 &
                 models(j)%importField(n)%add_offset,                   &
                 models(j)%importField(n)%enable_integral_adj,          &
                 COMPDES(j)//'-IMP'
              end if
            end if
          end do
        end do
      else
        write(*,*) "[error] -- Exchange field table '"//trim(ifile)//   &
                   "' is not available! exiting ..."
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      end if
!
!-----------------------------------------------------------------------
!     Format definition 
!-----------------------------------------------------------------------
!
 30   format(2I3,1X,A6,1X,A32,1X,A10,1X,A10,1X,A10,                     &
             1X,A10,1X,2E15.4,1X,L,1X,A7)
!
      end subroutine read_field_table
!
      subroutine add_field(field, str, exflag)
      implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
      type(ESM_Field), allocatable, intent(inout) :: field(:)
      character(len=*), intent(in) :: str(:)
      logical, intent(in) :: exflag
!
!-----------------------------------------------------------------------
!     Local variable declarations 
!-----------------------------------------------------------------------
!     
      integer :: n 
      type(ESM_Field), allocatable :: dum(:)
!
!-----------------------------------------------------------------------
!     Resize input list 
!-----------------------------------------------------------------------
!
      if (allocated(field)) then
        n = size(field, dim=1)
        allocate(dum(n))
        dum = field
        deallocate(field)
        allocate(field(n+1))
        field(:n) = dum
        n = n+1
      else
        n = 1
        allocate(field(n))
      end if
!
!-----------------------------------------------------------------------
!     Add new data to the list 
!-----------------------------------------------------------------------
!
      field(n)%fid = n
      field(n)%short_name = trim(str(1))
      field(n)%long_name = trim(str(2))
      if (trim(str(3)) == 'bilinear') then
        field(n)%itype = Ibilin
      else if (trim(str(3)) == 'conserv') then
        field(n)%itype = Iconsv
      else if (trim(str(3)) == 'nearstod') then
        field(n)%itype = Instod
      else if (trim(str(3)) == 'neardtos') then
        field(n)%itype = Indtos
      else
        field(n)%itype = Inone
      end if

      if (exflag) then 
        if (trim(str(4)) == 'cross') then
          field(n)%gtype = Icross
        else if (trim(str(4)) == 'dot') then
          field(n)%gtype = Idot
        else if (trim(str(4)) == 'u') then
          field(n)%gtype = Iupoint
        else if (trim(str(4)) == 'v') then
          field(n)%gtype = Ivpoint
        else
          field(n)%gtype = Inan
        end if
      else
        if (trim(str(5)) == 'cross') then
          field(n)%gtype = Icross
        else if (trim(str(5)) == 'dot') then
          field(n)%gtype = Idot
        else if (trim(str(5)) == 'u') then
          field(n)%gtype = Iupoint
        else if (trim(str(5)) == 'v') then
          field(n)%gtype = Ivpoint
        else
          field(n)%gtype = Inan
        end if
      end if
      field(n)%units = trim(str(6))
      field(n)%export_units = trim(str(7))
      if (trim(str(8)) == 'cf1') then
        field(n)%scale_factor = cf1
      else if (trim(str(8)) == '-cf1') then
        field(n)%scale_factor = -cf1
      else if (trim(str(8)) == 'cf2') then
        field(n)%scale_factor = cf2
      else if (trim(str(8)) == '-cf2') then
        field(n)%scale_factor = -cf2
      else if (trim(str(8)) == 'cf3') then
        field(n)%scale_factor = cf3
      else if (trim(str(8)) == '-cf3') then
        field(n)%scale_factor = -cf3
      else
        read(str(8),*) field(n)%scale_factor
      end if
      read(str(9),*) field(n)%add_offset
      if (trim(str(10)) == 'T' .or. trim(str(10)) == 't') then     
        field(n)%enable_integral_adj = .true.
      else
        field(n)%enable_integral_adj = .false.
      end if
!
      end subroutine add_field
!
      subroutine set_field_dir(vm, rc)
      implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
      type(ESMF_VM), intent(in) :: vm
      integer, intent(inout) :: rc
!
!-----------------------------------------------------------------------
!     Local variable declarations 
!-----------------------------------------------------------------------
!
      integer :: i, j, nf, localPet, petCount
      logical :: isExist
      character(ESMF_MAXSTR) :: lname, sname, units
!
      rc = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!     Query gridded component
!-----------------------------------------------------------------------
!
      call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Add required fields to NUOPC field dictionary 
!-----------------------------------------------------------------------
!
      do i = 1, nModels
        nf = size(models(i)%exportField)
        if (.not. models(i)%modActive) cycle
!
        do j = 1, nf
          lname = trim(models(i)%exportField(j)%long_name)
          sname = trim(models(i)%exportField(j)%short_name)
          units = trim(models(i)%exportField(j)%units)
!
          isExist = NUOPC_FieldDictionaryHasEntry(trim(lname), rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,&
                             line=__LINE__, file=FILENAME)) return
!
          if (.not. isExist) then
          call NUOPC_FieldDictionaryAddEntry(trim(lname),               &
               canonicalUnits=trim(units), rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,&
              line=__LINE__, file=FILENAME)) return
          if (localPet == 0) then
          write(*,40) "ADD TO FIELD DICTIONARY -> ", i, trim(lname)
          end if
          end if
        end do
!
        nf = size(models(i)%importField)
        do j = 1, nf
          lname = trim(models(i)%importField(j)%long_name)
          sname = trim(models(i)%importField(j)%short_name)
          units = trim(models(i)%importField(j)%units)
!
          isExist = NUOPC_FieldDictionaryHasEntry(trim(lname), rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,&
                             line=__LINE__, file=FILENAME)) return
!
          if (.not. isExist) then
          call NUOPC_FieldDictionaryAddEntry(trim(lname),               &
               canonicalUnits=trim(units), rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,&
              line=__LINE__, file=FILENAME)) return
          if (localPet == 0) then
          write(*,40) "ADD TO FIELD DICTIONARY -> ", i, trim(lname)
          end if
          end if
        end do
      end do
!
!-----------------------------------------------------------------------
!     Format definition 
!-----------------------------------------------------------------------
!
 40   format(A27, I3, " ", A)
!
      end subroutine set_field_dir
!
      end module mod_config
