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
      use mod_types, only : debugLevel
!
      implicit none
!
!-----------------------------------------------------------------------
!     Global module variables 
!-----------------------------------------------------------------------
!
      contains
!
      real*8 function calc_integral(vm, field, frac, area, rc)
      implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
      type(ESMF_VM), intent(in) :: vm
      type(ESMF_Field), intent(in) :: field
      type(ESMF_Field), intent(in) :: frac
      type(ESMF_Field), intent(in) :: area
      integer, intent(inout) :: rc
!
!-----------------------------------------------------------------------
!     Local variable declarations 
!-----------------------------------------------------------------------
!
      integer :: cLbnd(2), cUbnd(2)
      integer :: i, j, k, localDECount, localPet, petCount
      real(ESMF_KIND_R8), pointer :: ptrField(:,:) 
      real(ESMF_KIND_R8), pointer :: ptrFrac(:,:)
      real(ESMF_KIND_R8), pointer :: ptrArea(:,:)      
      real*8 :: total_de(1), total_global(1)
      character(ESMF_MAXSTR) :: fname
!
      type(ESMF_Grid) :: grid
!
      calc_integral = 0.0d0
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
      call ESMF_FieldGet(field, grid=grid, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      call ESMF_GridGet(grid, localDECount=localDECount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
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
      call ESMF_FieldGet(frac, localDe=k, farrayPtr=ptrFrac, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      call ESMF_FieldGet(area, localDe=k, farrayPtr=ptrArea, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Calculate integral for each local DE and PET 
!-----------------------------------------------------------------------
!
      total_de(1) = 0.0d0
      do i = clbnd(1), cubnd(1)
      do j = clbnd(2), cubnd(2)      
        total_de(1) = total_de(1)+                                      &
                      ptrField(i,j)*ptrArea(i,j)*ptrFrac(i,j)
      end do
      end do
!
!-----------------------------------------------------------------------
!     Debug: write sum of each PETs    
!-----------------------------------------------------------------------
!
      if (debugLevel > 1) then
        call ESMF_FieldGet(field, name=fname, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
                               line=__LINE__, file=FILENAME)) return
        write(*,20) localPet, k, total_de(1), trim(fname)
        call ESMF_VMBarrier(vm, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
                               line=__LINE__, file=FILENAME)) return
      end if
!
      end do
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
      calc_integral = total_global(1)
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
             ") - INTEGRAL = ",E12.5," (",A,")")
 30   format(" PET(",I3.3,") - GLOBAL INTEGRAL   = ",E12.5," (",A,")")
!
      end function calc_integral
!
      subroutine adjust_field()
      end subroutine adjust_field
!
      subroutine print_matrix_r8(inp, imin, imax, jmin, jmax,           &
                                 iskip, jskip, pet, id, header)
      implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
      integer, intent(in) :: imin, imax, jmin, jmax
      real*8 , intent(in) :: inp(imin:imax,jmin:jmax)
      integer, intent(in) :: iskip, jskip, pet, id
      character(len=*), intent(in) :: header
!
!-----------------------------------------------------------------------
!     Local variable declarations 
!-----------------------------------------------------------------------
!
      integer :: i, j
      character(100) :: fmt_123
!
!-----------------------------------------------------------------------
!     Write data 
!-----------------------------------------------------------------------
!
      write(id, fmt="('PET(',I2,') - ',A)") pet, trim(header)
!
      write(fmt_123, fmt="('(/, 5X, ', I3, 'I10)')") (imax-imin)+1
      write(id, fmt=trim(fmt_123))  (i, i=imin, imax, iskip)
!   
      write(fmt_123, fmt="('(I5, ', I3, 'F10.4)')") imax
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
