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
!
#define FILENAME "util/mod_types.F90" 
!
!-----------------------------------------------------------------------
!     Module for user defined types 
!-----------------------------------------------------------------------
!
      module mod_types
!
!-----------------------------------------------------------------------
!     Used module declarations 
!-----------------------------------------------------------------------
!
      use ESMF
      use NUOPC
!
      implicit none
!
!-----------------------------------------------------------------------
!     RTM river point data type 
!-----------------------------------------------------------------------
!
      type RTM_River
        real*8 :: lat, lon
        integer :: dir
        integer :: npoints
        real*8 :: monfac(12)
        integer :: iindex, jindex
        integer :: rootPet
      end type RTM_River
!
!-----------------------------------------------------------------------
!     ESM generic field data type 
!-----------------------------------------------------------------------
!
      type ESM_Field
        integer :: fid
        integer :: gtype        
        integer :: itype
        character(len=100) :: short_name
        character(len=100) :: long_name
        character(len=100) :: units
        character(len=100) :: export_units
        real*8 :: scale_factor
        real*8 :: add_offset
      end type ESM_Field
!
!-----------------------------------------------------------------------
!     ESM generic mesh data type 
!-----------------------------------------------------------------------
!
      type ESM_Mesh
        integer :: gid
        integer :: gtype
      end type ESM_Mesh
!
!-----------------------------------------------------------------------
!     ESM high-level generic data type 
!-----------------------------------------------------------------------
!
      type ESM_Model
        character(len=7) :: name
        integer :: divDT 
        integer :: nPets
        logical :: modActive
        integer, allocatable :: petList(:) 
        type(ESM_Mesh), allocatable :: mesh(:) 
        type(ESM_Field), allocatable :: importField(:)
        type(ESM_Field), allocatable :: exportField(:)
        type(ESMF_Grid) :: grid
      end type ESM_Model
!
!-----------------------------------------------------------------------
!     ESM grided component (models) holder
!-----------------------------------------------------------------------
!
      type(ESM_Model), allocatable, target :: models(:) 
!
!-----------------------------------------------------------------------
!     ESM connector (coupler) holder
!-----------------------------------------------------------------------
!
      type(ESM_Model), allocatable, target :: connectors(:,:)
!
!-----------------------------------------------------------------------
!     Number of gridded component or model
!-----------------------------------------------------------------------
!
      integer :: nModels
!
!-----------------------------------------------------------------------
!     ESM model indices
!-----------------------------------------------------------------------
!
      character(len=3) :: COMPDES(3) = (/'ATM','OCN','RTM'/)
      integer, parameter :: Iatmos = 1 
      integer, parameter :: Iocean = 2
      integer, parameter :: Iriver = 3
!
!-----------------------------------------------------------------------
!     Staggered grid point indices
!     d --------- d   d --- v --- d  
!     |           |   |           |
!     |     c     |   u     c     u
!     |           |   |           |
!     d --------- d   d --- v --- d     
!     Arakawa - B     Arakawa - C
!     RegCM           ROMS (c = rho, d = psi)
!-----------------------------------------------------------------------
!
      character(len=6) :: GRIDDES(0:4) = (/'N/A','CROSS','DOT','U','V'/)
      integer, parameter :: Inan    = 0
      integer, parameter :: Icross  = 1
      integer, parameter :: Idot    = 2
      integer, parameter :: Iupoint = 3
      integer, parameter :: Ivpoint = 4
!
!-----------------------------------------------------------------------
!     Interpolation type        
!-----------------------------------------------------------------------
!
      character(len=4) :: INTPDES(0:2) = (/'NONE','BLIN','CONS'/)
      integer, parameter :: Inone  = 0 
      integer, parameter :: Ibilin = 1 
      integer, parameter :: Iconsv = 2
!
!-----------------------------------------------------------------------
!     Running mode        
!-----------------------------------------------------------------------
!
      character(len=10) :: RUNNDES(2) = (/'SEQUENTIAL','CONCURENT'/)
      integer, parameter :: Iseq = 1
      integer, parameter :: Ipar = 2
!
!-----------------------------------------------------------------------
!     ESM connector (coupler) holder
!-----------------------------------------------------------------------
!
      type(RTM_River), allocatable, target :: rivers(:)
!
!-----------------------------------------------------------------------
!     ESM model parameters
!-----------------------------------------------------------------------
!
      character(ESMF_MAXSTR) :: config_fname="namelist.rc"
      character(ESMF_MAXSTR) :: petLayoutOption
      type(ESMF_Time) :: esmStartTime
      type(ESMF_Time) :: esmRestartTime
      type(ESMF_Time) :: esmStopTime
      type(ESMF_TimeInterval) :: esmTimeStep
      type(ESMF_Calendar) :: esmCal
      type(ESMF_Clock) :: esmClock
!
      integer :: runMod
      integer :: debugLevel
      logical :: restarted
!
!-----------------------------------------------------------------------
!     Constants 
!-----------------------------------------------------------------------
!
      real*8, parameter :: cp = 3985.0d0
      real*8, parameter :: rho0 = 1025.0d0
      real*8, parameter :: cf1 = rho0*cp
      real*8, parameter :: cf2 = 1.0d0/cf1
      real*8, parameter :: day2s = 1.0d0/86400.0d0
      real*8, parameter :: mm2m = 1.0d0/1000.0d0
!
      real*8, parameter :: MISSING_R8 = 1.0d20
      real  , parameter :: MISSING_R4 = 1.0e20
!
      real*8, parameter :: ZERO_R8 = 0.0d0
!
      contains
!
      integer function get_varid(list, key)
      implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
      type(ESM_Field), intent(in) :: list(:)
      character(len=*) :: key
!
!-----------------------------------------------------------------------
!     Local variable declarations 
!-----------------------------------------------------------------------
!
      integer :: i
!     
!-----------------------------------------------------------------------
!     Find index of specified field
!-----------------------------------------------------------------------
!
      do i = 1, size(list, dim=1)
        if (trim(list(i)%short_name) == trim(key)) then
          get_varid = i
          return
        end if
      end do
      get_varid = -1 
      return
      end function get_varid
!
      end module mod_types
