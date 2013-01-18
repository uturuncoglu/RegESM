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
!     Extended generic NUOPC InternalStateStruct 
!-----------------------------------------------------------------------
!
      character(*), parameter :: ESM_Label_InternalState =              &
                                 "DrvAtmOcnRtm_InternalState"
      character(*), parameter :: ESM_Label_SetModelPetLists =           &
                                 "DrvAtmOcnRtm_SetModelPetLists"
      character(*), parameter :: ESM_Label_SetModelServices =           &
                                 "DrvAtmOcnRtm_SetModelServices"
      character(*), parameter :: ESM_Label_Finalize =                   &
                                 "DrvAtmOcnRtm_Finalize"
  
      type ESM_Type_InternalStateStruct
        integer, pointer :: atmPetList(:)
        integer, pointer :: ocnPetList(:)
        integer, pointer :: rtmPetList(:)
        integer, pointer :: atm2ocnPetList(:)
        integer, pointer :: ocn2atmPetList(:)
        integer, pointer :: atm2rtmPetList(:)
        integer, pointer :: rtm2ocnPetList(:)
        type(ESMF_State) :: atmIS, atmES
        type(ESMF_State) :: ocnIS, ocnES
        type(ESMF_State) :: rtmIS, rtmES
        type(ESMF_GridComp) :: atm
        type(ESMF_GridComp) :: ocn
        type(ESMF_GridComp) :: rtm
        type(ESMF_CplComp) :: atm2ocn, atm2rtm
        type(ESMF_CplComp) :: ocn2atm
        type(ESMF_CplComp) :: rtm2ocn
        type(NUOPC_RunSequence), pointer :: runSeq(:)
      end type

      type ESM_Type_InternalState
        type(ESM_Type_InternalStateStruct), pointer :: wrap
      end type
!
!-----------------------------------------------------------------------
!     ESM model parameters
!-----------------------------------------------------------------------
!
      character(ESMF_MAXSTR) :: config_fname="namelist.rc"
!
!-----------------------------------------------------------------------
!     ESM model holder
!-----------------------------------------------------------------------
!
      type(ESM_Model), allocatable, target :: models(:) 
!
!-----------------------------------------------------------------------
!     ESM connector holder
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
!     ESM model variables
!-----------------------------------------------------------------------
!
      integer :: nModels
!
      character(ESMF_MAXSTR) :: petLayoutOption

      end module mod_types
