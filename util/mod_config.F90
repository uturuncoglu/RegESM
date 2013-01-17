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
      integer :: i, j, k, localPet, petCount
      logical :: file_exists
      character(100) :: fmt_123
!
      type(ESMF_Config) :: cf
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
        write(*, fmt='(A12,A)') "PET Layout: ", trim(petLayoutOption)
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
      do i = 1, nModels
        k = ubound(models(i)%petList, dim=1)
        write(fmt_123, fmt="('(A7, L, ', I3, 'I4)')") k
        write(*, fmt=trim(fmt_123)) trim(models(i)%name)//'    ',      &
              models(i)%modActive, models(i)%petList
      end do
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
      do i = 1, nModels
        do j = 1, nModels
          if (connectors(i,j)%modActive) then
            k = ubound(connectors(i,j)%petList, dim=1)
            write(fmt_123, fmt="('(A7, L, ', I3, 'I4)')") k
            write(*, fmt=trim(fmt_123)) connectors(i,j)%name,           &
                  connectors(i,j)%modActive, connectors(i,j)%petList
          end if
        end do
      end do
!
      end subroutine read_config
!
      end module mod_config
