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
#define FILENAME "mod_esmf_ocn_void.F90"
!
!-----------------------------------------------------------------------
!     OCN gridded component code 
!-----------------------------------------------------------------------
!
      module mod_esmf_ocn
!
!-----------------------------------------------------------------------
!     Used module declarations 
!-----------------------------------------------------------------------
!
      use ESMF
      use NUOPC
      use NUOPC_Model, only :                                           &
          NUOPC_SetServices          => routine_SetServices,            &
          NUOPC_Label_Advance        => label_Advance,                  &
          NUOPC_Label_DataInitialize => label_DataInitialize,           &
          NUOPC_Model_Type_IS        => type_InternalState,             &
          NUOPC_Model_Label_IS       => label_InternalState,            &
          NUOPC_Label_SetClock       => label_SetClock,                 &
          NUOPC_Label_CheckImport    => label_CheckImport
!
      implicit none
      private
!
!-----------------------------------------------------------------------
!     Public subroutines 
!-----------------------------------------------------------------------
!
      public :: OCN_SetServices
!
      contains
!
      subroutine OCN_SetServices(gcomp, rc)
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
      end subroutine OCN_SetServices
!
      end module mod_esmf_ocn

