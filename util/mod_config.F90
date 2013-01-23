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
      integer :: i, j, k, localPet, petCount
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
        cf = ESMF_ConfigCreate(rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
            line=__LINE__, file=FILENAME)) return
!
        call ESMF_ConfigLoadFile(cf, trim(config_fname), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
            line=__LINE__, file=FILENAME)) return
!        
        nModels = ESMF_ConfigGetLen(cf, label='PETs:', rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
            line=__LINE__, file=FILENAME)) return
        if (nModels .gt. 0) then
          if (.not. allocated(models)) allocate(models(nModels))
        end if
!
        call ESMF_ConfigFindLabel(cf, 'PETs:', rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
            line=__LINE__, file=FILENAME)) return

        do i = 1, nModels
          call ESMF_ConfigGetAttribute(cf, models(i)%nPets, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,&
              line=__LINE__, file=FILENAME)) return

          if (i .eq. Iatmos) then
            models(i)%name = "ATM"
          else if (i .eq. Iocean) then
            models(i)%name = "OCN"
          else if (i .eq. Iriver) then
            models(i)%name = "RTM"
          end if

          models(i)%modActive = .false.
          if (models(i)%nPets > 0) models(i)%modActive = .true.
        end do

        call ESMF_ConfigFindLabel(cf, 'PETLayoutOption:', rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
            line=__LINE__, file=FILENAME)) return

        call ESMF_ConfigGetAttribute(cf, petLayoutOption, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
            line=__LINE__, file=FILENAME)) return
        if (localPet == 0) then
          write(*, fmt='(A12,A)') "PET Layout: ", trim(petLayoutOption)
        end if
!
        call ESMF_ConfigGetAttribute(cf, debugLevel,                    &
                                     label='DebugLevel:', rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
            line=__LINE__, file=FILENAME)) return
!
        call ESMF_ConfigGetAttribute(cf, str, label='Calendar:', rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
            line=__LINE__, file=FILENAME)) return
        if (trim(str) == 'gregorian') cflag = ESMF_CALKIND_GREGORIAN
        if (trim(str) == 'noleap'   ) cflag = ESMF_CALKIND_NOLEAP
        if (trim(str) == '360_day'  ) cflag = ESMF_CALKIND_360DAY
!
        call ESMF_ConfigGetAttribute(cf, time, count=6,                 &
                                     label='StartTime:', rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
            line=__LINE__, file=FILENAME)) return
        call ESMF_TimeSet(esmStartTime,                                 &
                          yy=time(1), mm=time(2), dd=time(3),           &
                          h=time(4), m=time(5), s=time(6),              &
                          calkindflag=cflag, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
            line=__LINE__, file=FILENAME)) return
!
        call ESMF_ConfigGetAttribute(cf, time, count=6,                 &
                                     label='StopTime:', rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
            line=__LINE__, file=FILENAME)) return
        call ESMF_TimeSet(esmStopTime,                                  &
                          yy=time(1), mm=time(2), dd=time(3),           &
                          h=time(4), m=time(5), s=time(6),              &
                          calkindflag=cflag, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
            line=__LINE__, file=FILENAME)) return
!
        call ESMF_ConfigGetAttribute(cf, time, count=6,                 &
                                     label='TimeStep:', rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
            line=__LINE__, file=FILENAME)) return
        esmCal = ESMF_CalendarCreate(cflag, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
            line=__LINE__, file=FILENAME)) return
        call ESMF_TimeIntervalSet(esmTimeStep, calendar=esmCal,         &
                                  yy=time(1), mm=time(2), d=time(3),    &
                                  h=time(4), m=time(5), s=time(6),      &
                                  rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
            line=__LINE__, file=FILENAME)) return
      end if
!
!-----------------------------------------------------------------------
!     Assign PETs to model components 
!-----------------------------------------------------------------------
!
      select case (trim(petLayoutOption))
      case ('sequential')
        do i = 1, nModels
          models(i)%nPets = petCount
          if (.not. allocated(models(i)%petList)) then
            allocate(models(i)%petList(models(i)%nPets))
          end if
!
          models(i)%petList = (/ (k, k = 0, petCount-1) /)
        end do 
      case ('concurrent')
        do i = 1, nModels
          if (.not. allocated(models(i)%petList)) then
            allocate(models(i)%petList(models(i)%nPets))
          end if
!
          do j = 1, models(i)%nPets
            if (i .eq. Iatmos) then
              models(i)%petList(j) = j-1
            else if (i .eq. Iocean) then
              k = ubound(models(Iocean-1)%petList, dim=1)
              models(i)%petList(j) = models(Iocean-1)%petList(k)+j
            else if (i .eq. Iriver) then
              k = ubound(models(Iriver-1)%petList, dim=1)
              models(i)%petList(j) = models(Iriver-1)%petList(k)+j
              !if (models(i)%nPets == 1) then
              !  models(i)%petList(j) = models(Iriver-1)%petList(k)
              !else
              !  models(i)%petList(j) = models(Iriver-1)%petList(k)+j
              !end if
            end if
          end do
        end do
      case default
        call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc,              &
             msg='PetLayoutOption is not supported: '//                 &
             trim(petLayoutOption))
        return
      end select

      ! print model PETs
      if (localPet == 0) then
        do i = 1, nModels
          if (models(i)%modActive) then
            k = ubound(models(i)%petList, dim=1)
            write(fmt_123, fmt="('(A7, L, ', I3, 'I4)')") k
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
            connectors(i,j)%name = trim(models(i)%name)//"-"//          &
                                   trim(models(j)%name)
            connectors(i,j)%modActive = .true.
            connectors(i,j)%nPets = petCount
            if (.not. allocated(connectors(i,j)%petList)) then
              allocate(connectors(i,j)%petList(petCount))
            end if
            connectors(i,j)%petList = (/ (k, k = 0, petCount-1) /)
          end if
        end do
      end do
!
      ! fix active connectors (no exchange RTM->ATM and OCN->RTM)
      connectors(Iriver,Iatmos)%modActive = .false.
      connectors(Iocean,Iriver)%modActive = .false.
!
      if (localPet == 0) then
        do i = 1, nModels
          do j = 1, nModels
            if (connectors(i,j)%modActive) then
              k = ubound(connectors(i,j)%petList, dim=1)
              write(fmt_123, fmt="('(A7, L, ', I3, 'I4)')") k
              write(*, fmt=trim(fmt_123)) connectors(i,j)%name,         &
                    connectors(i,j)%modActive, connectors(i,j)%petList
            end if
          end do
        end do
      end if
!
!-----------------------------------------------------------------------
!     Read exchange field table 
!-----------------------------------------------------------------------
!
      call read_field_table('util/exfield0.tbl', localPet, rc)
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
      integer :: i, j, k, s, ios, pos1, pos2, nf
      logical :: file_exists
      character(len=400) :: str, dum
!
      rc = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!     Read exchange field table
!-----------------------------------------------------------------------
!
      inquire(file=trim(ifile), exist=file_exists)
!
      if (file_exists) then
        open(unit=iunit, file=trim(ifile), status='old')
        ios = 0
        do while (ios == 0)
          read(iunit,*,iostat=ios) nf, str
!
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
          end select          
          write(*,fmt='(A,I2)') COMPDES(i)//' -> '//COMPDES(j)//' ', nf
!
          ! allocate import and export field arrays
          if (.not. allocated(models(i)%exportField)) then
            allocate(models(i)%exportField(nf))
          end if
          if (.not. allocated(models(j)%importField)) then
            allocate(models(j)%importField(nf))
          end if
!
          ! loop over fields
          do k = 1, nf
            ! read whole line
            read(iunit,fmt='(A)',iostat=ios) str
            models(i)%exportField(k)%fid = k
            models(j)%importField(k)%fid = k
            ! split fields and fill field data type
            s = 1
            pos1 = 1
            do
              pos2 = index(str(pos1:), ':')
              if (pos2 == 0) then
                dum = trim(str(pos1:))
                read(dum,*) models(i)%exportField(k)%add_offset
                read(dum,*) models(j)%importField(k)%add_offset
                exit
              end if
              dum = trim(str(pos1:pos1+pos2-2))
              if (s == 1) then
                models(i)%exportField(k)%short_name = trim(dum)
                models(j)%importField(k)%short_name = trim(dum)
              else if (s == 2) then
                models(i)%exportField(k)%long_name = trim(dum) 
                models(j)%importField(k)%long_name = trim(dum) 
              else if (s == 3) then
                if (trim(dum) == 'bilinear') then
                  models(i)%exportField(k)%itype = Ibilin
                  models(j)%importField(k)%itype = Ibilin
                else if (trim(dum) == 'conservative') then
                  models(i)%exportField(k)%itype = Iconsv
                  models(j)%importField(k)%itype = Iconsv
                else
                  models(i)%exportField(k)%itype = Inone
                  models(j)%importField(k)%itype = Inone
                end if
              else if (s == 4) then
                if (trim(dum) == 'cross') then
                  models(i)%exportField(k)%gtype = Icross 
                  models(j)%importField(k)%gtype = Icross 
                else if (trim(dum) == 'dot') then
                  models(i)%exportField(k)%gtype = Idot
                  models(j)%importField(k)%gtype = Idot
                else if (trim(dum) == 'u') then
                  models(i)%exportField(k)%gtype = Iupoint
                  models(j)%importField(k)%gtype = Iupoint
                else if (trim(dum) == 'v') then
                  models(i)%exportField(k)%gtype = Ivpoint
                  models(j)%importField(k)%gtype = Ivpoint
                else 
                  models(i)%exportField(k)%gtype = Inan
                  models(j)%importField(k)%gtype = Inan
                end if
              else if (s == 5) then
                models(i)%exportField(k)%units = trim(dum)
                models(j)%importField(k)%units = trim(dum)
              else if (s == 6) then
                models(i)%exportField(k)%export_units = trim(dum)
                models(j)%importField(k)%export_units = trim(dum)
              else if (s == 7) then
                if (trim(dum) == 'cf1') then
                  models(i)%exportField(k)%scale_factor = cf1
                  models(j)%importField(k)%scale_factor = cf1
                else if (trim(dum) == '-cf1') then
                  models(i)%exportField(k)%scale_factor = -cf1
                  models(j)%importField(k)%scale_factor = -cf1
                else if (trim(dum) == 'cf2') then
                  models(i)%exportField(k)%scale_factor = cf2
                  models(j)%importField(k)%scale_factor = cf2
                else if (trim(dum) == '-cf2') then
                  models(i)%exportField(k)%scale_factor = -cf2
                  models(j)%importField(k)%scale_factor = -cf2
                else
                  read(dum,*) models(i)%exportField(k)%scale_factor
                  read(dum,*) models(j)%importField(k)%scale_factor
                end if
              end if
              s = s+1
              pos1 = pos2+pos1
            end do
!
            ! print out
            if (debugLevel > 0 .and. localPet == 0) then 
            write(*,fmt='(I2,A," ",2E15.4)') k,                         &
                  ' '//trim(models(i)%exportField(k)%short_name)//      &
                  ' '//trim(models(i)%exportField(k)%long_name)//       &
                  ' '//trim(models(i)%exportField(k)%units)//           &
                  ' '//trim(models(i)%exportField(k)%export_units)//    &
                  ' '//trim(GRIDDES(models(i)%exportField(k)%gtype))//  &
                  ' '//trim(INTPDES(models(i)%exportField(k)%itype)),   &
                  models(i)%exportField(k)%scale_factor,                &
                  models(i)%exportField(k)%add_offset
            end if
          end do
        end do        
      else
        write(*,*) 'Exchange field table is not available:'//trim(ifile)
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      end if
!
      end subroutine read_field_table
!
      end module mod_config
