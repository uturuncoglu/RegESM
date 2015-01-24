!-----------------------------------------------------------------------
!
!     This file is part of ITU RegESM.
!
!     ITU RegESM is free software: you can redistribute it and/or
!     modify
!     it under the terms of the GNU General Public License as published
!     by
!     the Free Software Foundation, either version 3 of the License, or
!     (at your option) any later version.
!
!     ITU RegESM is distributed in the hope that it will be useful,
!     but WITHOUT ANY WARRANTY; without even the implied warranty of
!     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!     GNU General Public License for more details.
!
!     You should have received a copy of the GNU General Public License
!     along with ITU RegESM.  If not, see
!     <http://www.gnu.org/licenses/>.
!
!-----------------------------------------------------------------------
#define FILENAME "util/mod_shared.F90" 
!
!-----------------------------------------------------------------------
!     Module file for generic utilities
!-----------------------------------------------------------------------
!
      module mod_shared
!
!-----------------------------------------------------------------------
!     Used module declarations 
!-----------------------------------------------------------------------
!
      use mod_types
!
      implicit none
!
!-----------------------------------------------------------------------
!     Interfaces 
!-----------------------------------------------------------------------
!
      interface gc_latlon 
        module procedure gc_latlon_r8
      end interface gc_latlon
!
      contains
!
      subroutine gc_latlon_r8(plon, plat, imin, imax, jmin, jmax,       &
                         lon, lat, distance)
      implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
      real*8, intent(in) :: plon
      real*8, intent(in) :: plat
      integer, intent(in) :: imin, imax, jmin, jmax
      real*8, intent(in) :: lon(imin:imax,jmin:jmax)
      real*8, intent(in) :: lat(imin:imax,jmin:jmax)
      real*8, intent(inout) :: distance(imin:imax,jmin:jmax)
!
!-----------------------------------------------------------------------
!     Local variable declarations 
!-----------------------------------------------------------------------
!
      integer :: i, j
      real*8 :: dlon, dlat, a, c, r 
!
!-----------------------------------------------------------------------
!     Calculate distance between grid points and given origin (plon,plat) 
!     Haversine Formula (from R.W. Sinnott, "Virtues of the Haversine",
!     Sky and Telescope, vol. 68, no. 2, 1984, p. 159) - km
!-----------------------------------------------------------------------
!
      do i = imin, imax
        do j = jmin, jmax
          dlon = lon(i,j)-plon
          dlat = lat(i,j)-plat
          a = sin(dlat*D2R/2.0d0)**2+cos(plat*D2R)*                     &
              cos(lat(i,j)*D2R)*sin(dlon*D2R/2.0d0)**2
          c = 2.0d0*asin(min(1.0,a**0.5))
          r = 6378.0d0-21.0d0*sin(lat(i,j)*D2R)
          distance(i,j) = r*c
        end do
      end do
!
      return
      end subroutine gc_latlon_r8
!
      subroutine print_matrix(inp, imin, imax, jmin, jmax,              &
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
      end subroutine print_matrix
!
      subroutine get_ij(vm, plon, plat, i, j, rc)
      implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
      type(ESMF_VM), intent(in) :: vm
      real*8, intent(in) :: plon, plat
      integer, intent(inout) :: i, j 
      integer, intent(inout) :: rc 
!
!-----------------------------------------------------------------------
!     Local variable declarations 
!-----------------------------------------------------------------------
!
      integer :: k, ii, jj, localPet, petCount, sendData(2)
      integer :: imin, imax, jmin, jmax
      real*8, allocatable :: distance(:,:)
      real*8 :: mdistance
!
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
!     Query grid index belongs to cross points and upper and lower 
!     limit of the coordinate  arrays
!-----------------------------------------------------------------------
!
      k = minloc(models(Iocean)%mesh(:)%gtype, dim=1,                   &
                 mask=(models(Iocean)%mesh(:)%gtype == Icross))
!
      imin = lbound(models(Iocean)%mesh(k)%glon, dim=1)
      imax = ubound(models(Iocean)%mesh(k)%glon, dim=1)
      jmin = lbound(models(Iocean)%mesh(k)%glon, dim=2)
      jmax = ubound(models(Iocean)%mesh(k)%glon, dim=2)
!
!-----------------------------------------------------------------------
!     Allocate variables. 
!-----------------------------------------------------------------------
!
      if (.not. allocated(distance)) then
        allocate(distance(imin:imax,jmin:jmax))
      end if
!
!-----------------------------------------------------------------------
!     Calculate distance (in km) between given point and grids. 
!-----------------------------------------------------------------------
!
      call gc_latlon(plon, plat, imin, imax, jmin, jmax,                &
                     models(Iocean)%mesh(k)%glon,                       &
                     models(Iocean)%mesh(k)%glat, distance)
!
!-----------------------------------------------------------------------
!     Get local minimum distance and location. 
!-----------------------------------------------------------------------
!
      mdistance = minval(distance, mask=(models(Iocean)%mesh(k)%gmsk == &
                         models(Iocean)%isLand))
!
      i = 0
      j = 0
      do jj = jmin, jmax
        do ii = imin, imax
!          write(*,fmt='(A,3I5,2F10.4)') "turuncu - ", localPet, ii, jj, models(Iocean)%mesh(k)%glon(ii,jj),models(Iocean)%mesh(k)%glat(ii,jj) 
          if (distance(ii,jj) == mdistance) then
            i = ii
            j = jj
            exit
          end if
        end do
      end do
!      print*, "turuncu 2 - ", i, j
!
!-----------------------------------------------------------------------
!     Broadcast grid indices of grid point that has the mininum distance
!     to the diven point across the PETs 
!-----------------------------------------------------------------------
!
      sendData = ZERO_I4
      sendData(1) = i
      sendData(2) = j
      call ESMF_VMBroadcast(vm, bcstData=sendData, count=2,             &
                            rootPet=0, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
      i = sendData(1)
      j = sendData(2)
!
!-----------------------------------------------------------------------
!     Deallocated temporary variables 
!-----------------------------------------------------------------------
!
      if (allocated(distance)) deallocate(distance)
!    
      end subroutine get_ij
!
      subroutine get_ll(vm, i, j, plon, plat, rc)
      implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
      type(ESMF_VM), intent(in) :: vm
      integer, intent(in) :: i, j
      real*8, intent(inout) :: plon, plat
      integer, intent(inout) :: rc 
!
!-----------------------------------------------------------------------
!     Local variable declarations 
!-----------------------------------------------------------------------
!
      integer :: k, ii, jj, localPet, petCount
      integer :: imin, imax, jmin, jmax
      real*8 :: sendData(2)
!
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
!     Query grid index belongs to cross points and upper and lower 
!     limit of the coordinate  arrays
!-----------------------------------------------------------------------
!
      k = minloc(models(Iocean)%mesh(:)%gtype, dim=1,                   &
                 mask=(models(Iocean)%mesh(:)%gtype == Icross))
!
      imin = lbound(models(Iocean)%mesh(k)%glon, dim=1)
      imax = ubound(models(Iocean)%mesh(k)%glon, dim=1)
      jmin = lbound(models(Iocean)%mesh(k)%glon, dim=2)
      jmax = ubound(models(Iocean)%mesh(k)%glon, dim=2)
!
!-----------------------------------------------------------------------
!     Get latitude and longitude values of specified grid point 
!-----------------------------------------------------------------------
!
      plon = MISSING_R8 
      plat = MISSING_R8
!
      do jj = jmin, jmax
        do ii = imin, imax
          if (i == ii .and. j == jj) then
            plon = models(Iocean)%mesh(k)%glon(ii,jj)
            plat = models(Iocean)%mesh(k)%glat(ii,jj)
            exit
          end if
        end do
      end do
!
!-----------------------------------------------------------------------
!     Broadcast coordinate pair using global VM 
!-----------------------------------------------------------------------
!
      sendData = ZERO_R8
      sendData(1) = plon
      sendData(2) = plat
      call ESMF_VMBroadcast(vm, bcstData=sendData, count=2,             &
                            rootPet=0, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
      plon = sendData(1)
      plat = sendData(2)
!    
      end subroutine get_ll
!
      subroutine map_rivers(vm, rc)
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
      integer :: i, j, k, r, np, localPet, petCount, nRiver
      integer :: imin, imax, jmin, jmax, sendData2(1)
      real*8, dimension(:,:), allocatable :: distance
      real*8, dimension(:), allocatable :: sendData1 
      integer :: pos(2)
      real*8 :: totalArea
!
      rc = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!     Map closest ocean grids to the river points
!     The effective radius is used to find the set of ocean grids 
!-----------------------------------------------------------------------
!
      ! get number of specified rivers
      nRiver = size(rivers, dim=1)
!
      ! get index of used grid stencile (algorithm uses cross points)
      k = minloc(models(Iocean)%mesh(:)%gtype, dim=1,                   &
                 mask=(models(Iocean)%mesh(:)%gtype == Icross))
!
      ! get limits 
      imin = lbound(models(Iocean)%mesh(k)%glon, dim=1)
      imax = ubound(models(Iocean)%mesh(k)%glon, dim=1)
      jmin = lbound(models(Iocean)%mesh(k)%glon, dim=2)
      jmax = ubound(models(Iocean)%mesh(k)%glon, dim=2)
!
      if (.not. allocated(distance)) then
        allocate(distance(imin:imax,jmin:jmax))
      end if 
!
      if (.not. allocated(sendData1)) then
        allocate(sendData1(MAX_MAPPED_GRID))
      end if
!
      do r = 1, nRiver 
        if (rivers(r)%isActive > 0) then
          ! calculate distance to river point
          distance = ZERO_R8
          call gc_latlon(rivers(r)%lon, rivers(r)%lat,                  &
                         imin, imax, jmin, jmax,                        &
                         models(Iocean)%mesh(k)%glon,                   &
                         models(Iocean)%mesh(k)%glat, distance)
!
          ! find closest ocean model grid
          pos = minloc(distance, mask=(models(Iocean)%mesh(k)%gmsk ==   &
                       models(Iocean)%isOcean))
!
          ! check position indices and skip loop
          if (any(pos == 0)) then
            rivers(r)%mapSize = ZERO_I4
            rivers(r)%mapArea = ZERO_R8
            rivers(r)%mapTable(:,:) = MISSING_R8
            cycle 
          end if
!
          ! calculate distance to closest ocean model grid
          distance = ZERO_R8
          call gc_latlon(models(Iocean)%mesh(k)%glon(pos(1),pos(2)),    &
                         models(Iocean)%mesh(k)%glat(pos(1),pos(2)),    &
                         imin, imax, jmin, jmax,                        &
                         models(Iocean)%mesh(k)%glon,                   &
                         models(Iocean)%mesh(k)%glat, distance)
!
          ! find list of grid indices
          np = ZERO_I4
          totalArea = ZERO_R8
          rivers(r)%mapTable(:,:) = MISSING_R8
!
          do i = imin, imax
            do j = jmin, jmax
              if ((distance(i,j) <= rivers(r)%eRadius .and.             &
                  (models(Iocean)%mesh(k)%gmsk(i,j) ==                  &
                   models(Iocean)%isOcean))) then
                np = np+1
!
                ! check for size
                if (np > MAX_MAPPED_GRID) then 
                  write(*,fmt='(A,I5,A)') "[error] - Try to reduce "//  &
                        "effective radius for river [", r, "]"
                  write(*,fmt='(A)') "[error] - Number of effected "//  &
                        "ocean grid points greater than ",              &
                        MAX_MAPPED_GRID 
                  call ESMF_Finalize(endflag=ESMF_END_ABORT)
                end if
!
                ! calculate total area of mapped ocean grid points
                totalArea = totalArea+models(Iocean)%mesh(k)%gare(i,j)
!
                rivers(r)%mapSize = np
                rivers(r)%mapArea = totalArea
                rivers(r)%mapTable(1,np) = i
                rivers(r)%mapTable(2,np) = j
                rivers(r)%mapTable(3,np) = models(Iocean)%mesh(k)%gare(i,j)
              end if
            end do
          end do
!
          where (rivers(r)%mapTable(3,:) /= MISSING_R8)
            rivers(r)%mapTable(3,:) = rivers(r)%mapTable(3,:)/totalArea
          end where
        end if
!
!-----------------------------------------------------------------------
!     Broadcast map data 
!-----------------------------------------------------------------------
!
        do i = 1, 3
          sendData1(:) = rivers(r)%mapTable(i,:)
          call ESMF_VMBroadcast(vm, bcstData=sendData1,                 &
                                count=MAX_MAPPED_GRID, rootPet=0, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,&
                                 line=__LINE__, file=FILENAME)) return
          rivers(r)%mapTable(i,:) = sendData1(:)
        end do
!
        sendData2(1) = rivers(r)%mapSize
        call ESMF_VMBroadcast(vm, bcstData=sendData2, count=1,          &
                              rootPet=0, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
                               line=__LINE__, file=FILENAME)) return
        rivers(r)%mapSize = sendData2(1)
!
        sendData1(1) = rivers(r)%mapArea
        call ESMF_VMBroadcast(vm, bcstData=sendData1, count=1,          &
                              rootPet=0, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
                               line=__LINE__, file=FILENAME)) return
        rivers(r)%mapArea = sendData1(1)
!
      end do
!
!-----------------------------------------------------------------------
!     Deallocate temporary arrays. 
!-----------------------------------------------------------------------
!
      if (allocated(distance)) then
        deallocate(distance)
        deallocate(sendData1)
      end if
!
      end subroutine map_rivers
!
      end module mod_shared
