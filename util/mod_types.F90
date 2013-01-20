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
!     ESM high-level generic data type 
!-----------------------------------------------------------------------
!
      type ESM_Model
        character(len=7) :: name
        integer :: nPets
        logical :: modActive
        integer, allocatable :: petList(:) 
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
!     ESM model indices
!-----------------------------------------------------------------------
!
      integer, parameter :: Iatmos = 1
      integer, parameter :: Iocean = 2
      integer, parameter :: Iriver = 3
!
!-----------------------------------------------------------------------
!     Number of gridded component or model
!-----------------------------------------------------------------------
!
      integer :: nModels
!
!-----------------------------------------------------------------------
!     ESM model parameters
!-----------------------------------------------------------------------
!
      character(ESMF_MAXSTR) :: config_fname="namelist.rc"
      character(ESMF_MAXSTR) :: petLayoutOption
      type(ESMF_Time) :: startTime
      type(ESMF_Time) :: stopTime
      type(ESMF_TimeInterval) :: timeStep
      type(ESMF_Calendar) :: cal

      end module mod_types
