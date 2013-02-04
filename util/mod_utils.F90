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
      implicit none
!
!-----------------------------------------------------------------------
!     Global module variables 
!-----------------------------------------------------------------------
!
      contains
!
      subroutine print_matrix_r8(inp, iskip, jskip, pet, id, header)
      implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
      real*8, intent(in) :: inp(:,:)
      integer, intent(in) ::  iskip, jskip, pet, id
      character(len=*), intent(in) :: header
!
!-----------------------------------------------------------------------
!     Local variable declarations 
!-----------------------------------------------------------------------
!
      integer :: i, j, imin, imax, jmin, jmax
      character(100) :: fmt_123
!
!-----------------------------------------------------------------------
!     Write data 
!-----------------------------------------------------------------------
!
      imin = lbound(inp, dim=1)
      imax = ubound(inp, dim=1)
      jmin = lbound(inp, dim=2)
      jmax = ubound(inp, dim=2)
!
      write(6, fmt="('PET(',I2,') - ',A)") pet, trim(header)
!
      write(fmt_123, fmt="('(/, 5X, ', I3, 'I10)')") (imax-imin)+1
      write(id, fmt=trim(fmt_123))  (i, i=imin, imax, iskip)
!   
      write(fmt_123, fmt="('(I5, ', I3, 'F10.2)')") imax
      do j=jmin, jmax, jskip
        write(id, fmt=trim(fmt_123)) j, (inp(i,j),i=imin, imax, iskip)
      end do
!
      return
      end subroutine print_matrix_r8      
!
      subroutine print_size_r8 (field, localPet, header)
      implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
      real*8, intent(in) :: field(:,:) 
      integer, intent(in) :: localPet
      character(len=*), intent(in) :: header
!
!-----------------------------------------------------------------------
!     Local variable declarations 
!-----------------------------------------------------------------------
!
      write(*,40) localPet, trim(adjustl(header)),                      &
                  lbound(field, dim=1), ubound(field, dim=1),           &
                  lbound(field, dim=2), ubound(field, dim=2)
!
!-----------------------------------------------------------------------
!     Formats 
!-----------------------------------------------------------------------
!
 40   format(" PET(",I3,") - ", A20, " : ", 4I8)
      end subroutine print_size_r8
!
      end module mod_utils
