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
        distance = ZERO_R8
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
          if (distance(ii,jj) == mdistance) then
            i = ii
            j = jj
            exit
          end if
        end do
      end do
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
      integer :: imin, imax, jmin, jmax, pos(2), ibuffer(1)
      real*8, dimension(:,:), allocatable :: distance
      real*8, dimension(:), allocatable :: dbuffer2 
      real*8 :: totalArea, dbuffer1(1)
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
!     Map closest ocean grids to the river points
!     The effective radius is used to find the set of ocean grids 
!     The algorithm runs on PET = 0 beacuse only root PET 
!     has global view of grid coordinates 
!-----------------------------------------------------------------------
!
      ! get number of specified rivers
      nRiver = size(rivers, dim=1)
!
      if (localPet == 0) then
        ! get index of used grid stencile (algorithm uses cross points)
        k = minloc(models(Iocean)%mesh(:)%gtype, dim=1,                 &
                   mask=(models(Iocean)%mesh(:)%gtype == Icross))
!
        ! get limits 
        imin = lbound(models(Iocean)%mesh(k)%glon, dim=1)
        imax = ubound(models(Iocean)%mesh(k)%glon, dim=1)
        jmin = lbound(models(Iocean)%mesh(k)%glon, dim=2)
        jmax = ubound(models(Iocean)%mesh(k)%glon, dim=2)
!
        ! allocate temporary distance array
        if (.not. allocated(distance)) then
          allocate(distance(imin:imax,jmin:jmax))
        end if 
!
        do r = 1, nRiver 
          if (rivers(r)%isActive > 0) then
            ! calculate distance to river point
            distance = ZERO_R8
            call gc_latlon(rivers(r)%lon, rivers(r)%lat,                &
                           imin, imax, jmin, jmax,                      &
                           models(Iocean)%mesh(k)%glon,                 &
                           models(Iocean)%mesh(k)%glat, distance)
!
            ! find closest ocean model grid
            pos = minloc(distance, mask=(models(Iocean)%mesh(k)%gmsk == &
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
            call gc_latlon(models(Iocean)%mesh(k)%glon(pos(1),pos(2)),  &
                           models(Iocean)%mesh(k)%glat(pos(1),pos(2)),  &
                           imin, imax, jmin, jmax,                      &
                           models(Iocean)%mesh(k)%glon,                 &
                           models(Iocean)%mesh(k)%glat, distance)
!
            ! find list of grid indices
            np = ZERO_I4
            totalArea = ZERO_R8
            rivers(r)%mapTable(:,:) = MISSING_R8
!
            do i = imin, imax
              do j = jmin, jmax
                if ((distance(i,j) <= rivers(r)%eRadius .and.           &
                    (models(Iocean)%mesh(k)%gmsk(i,j) ==                &
                     models(Iocean)%isOcean))) then
                  np = np+1
!
                  ! check for size
                  if (np > MAX_MAPPED_GRID) then 
                    write(*,fmt='(A,I5,A)') "[error] - Try to reduce "//&
                          "effective radius for river [", r, "]"
                    write(*,fmt='(A)') "[error] - Number of effected "//&
                          "ocean grid points greater than ",            &
                          MAX_MAPPED_GRID 
                    call ESMF_Finalize(endflag=ESMF_END_ABORT)
                  end if
!
                  ! calculate total area of mapped ocean grid points
                  totalArea = totalArea+models(Iocean)%mesh(k)%gare(i,j)
!
                  rivers(r)%mapTable(1,np) = i
                  rivers(r)%mapTable(2,np) = j
                  rivers(r)%mapTable(3,np) =                            &
                                        models(Iocean)%mesh(k)%gare(i,j)
                end if
              end do
            end do
!
            rivers(r)%mapSize = np
            rivers(r)%mapArea = totalArea
!
            ! calculate weights
            do i = 1, np
              rivers(r)%mapTable(3,i) = rivers(r)%mapTable(3,i)/totalArea
            end do
          end if
        end do 
!
        ! deallocate temporary distance array
        if (allocated(distance)) then
          deallocate(distance)
        end if
      else
        do r = 1, nRiver 
          rivers(r)%mapSize = ZERO_I4
          rivers(r)%mapArea = ZERO_R8
          rivers(r)%mapTable(:,:) = MISSING_R8
        end do
      end if
!
!-----------------------------------------------------------------------
!     Broadcast map data 
!-----------------------------------------------------------------------
!
      do r = 1, nRiver 
        if (rivers(r)%isActive > 0) then
          ! broadcast number of grid effected by each river 
          ibuffer(1) = rivers(r)%mapSize
          call ESMF_VMBroadcast(vm, bcstData=ibuffer, count=1,          &
                                rootPet=0, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,&
                                 line=__LINE__, file=FILENAME)) return
          rivers(r)%mapSize = ibuffer(1)
!
          ! broadcast total surface area effected by each river
          dbuffer1(1) = rivers(r)%mapArea
          call ESMF_VMBroadcast(vm, bcstData=dbuffer1, count=1,         &
                                rootPet=0, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,&
                                 line=__LINE__, file=FILENAME)) return
          rivers(r)%mapArea = dbuffer1(1)
!
          ! allocate send buffer for river
          np = rivers(r)%mapSize
          if (.not. allocated(dbuffer2)) then
            allocate(dbuffer2(np))
          end if
!
          ! broadcast info of effected grid points (i, j and weight)
          do i = 1, 3
            dbuffer2 = rivers(r)%mapTable(i,1:np)
            call ESMF_VMBroadcast(vm, bcstData=dbuffer2,                &
                                  count=np, rootPet=0, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc,                        &
                                   msg=ESMF_LOGERR_PASSTHRU,            &
                                   line=__LINE__, file=FILENAME)) return
            rivers(r)%mapTable(i,1:np) = dbuffer2
          end do
!
          ! deallocate buffer
          if (allocated(dbuffer2)) deallocate(dbuffer2)
        end if
      end do
!
      end subroutine map_rivers
!
      subroutine print_timestamp(field, ctime, localPet, str, rc)
      implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
      type(ESMF_Field), intent(in) :: field
      type(ESMF_Time),  intent(in) :: ctime
      integer, intent(in) :: localPet
      character(*), intent(in) :: str
      integer, intent(inout) :: rc
!
!-----------------------------------------------------------------------
!     Local variable declarations 
!-----------------------------------------------------------------------
!
      integer :: vl1(9), vl2(9)
      character(ESMF_MAXSTR) :: str1, str2
!
      rc = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!     Get field TimeStamp attribute 
!-----------------------------------------------------------------------
!
      call ESMF_AttributeGet(field, name="TimeStamp",                   &
                             valueList=vl1, convention="NUOPC",         &
                             purpose="Instance", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      write(str1,10) vl1(1), vl1(2), vl1(3), vl1(4), vl1(5), vl1(6)
!
!-----------------------------------------------------------------------
!     Get current time 
!-----------------------------------------------------------------------
!
      call ESMF_TimeGet(ctime, yy=vl2(1), mm=vl2(2), dd=vl2(3),         &
                        h=vl2(4), m=vl2(5), s=vl2(6), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      write(str2,10) vl2(1), vl2(2), vl2(3), vl2(4), vl2(5), vl2(6)
!
!-----------------------------------------------------------------------
!     Print out
!-----------------------------------------------------------------------
!
      if (localPet == 0) then
        if (trim(str1) /= trim(str2)) then
          print*, trim(str), ": TIMESTAMP = ", trim(str1), " /= ",      &
                  trim(str2), " ERROR !!!" 
        else
          print*, trim(str), ": TIMESTAMP = ", trim(str1)
        end if
      end if
!
!-----------------------------------------------------------------------
!     Format definition 
!-----------------------------------------------------------------------
!
10    format(I4,'-',I2.2,'-',I2.2,'_',I2.2,':',I2.2,':',I2.2)
!
      end subroutine print_timestamp
!
      pure function to_upper (str) result (string)
      implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations
!-----------------------------------------------------------------------
!
      character(*), intent(in) :: str
      character(len(str)) :: string
!
!-----------------------------------------------------------------------
!     Local variable declarations
!-----------------------------------------------------------------------
!
      integer :: ic, i
      character(26), parameter :: cap = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
      character(26), parameter :: low = 'abcdefghijklmnopqrstuvwxyz'
!
!-----------------------------------------------------------------------
!     capitalize each letter if it is lowecase
!-----------------------------------------------------------------------
!
      string = str
      do i = 1, len_trim(str)
        ic = index(low, str(i:i))
        if (ic > 0) string(i:i) = cap(ic:ic)
      end do
!
      end function to_upper
!
      pure function to_lower (str) result (string)
      implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations
!-----------------------------------------------------------------------
!
      character(*), intent(in) :: str
      character(len(str)) :: string
!
!-----------------------------------------------------------------------
!     Local variable declarations
!-----------------------------------------------------------------------
!
      integer :: ic, i
      character(26), parameter :: cap = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
      character(26), parameter :: low = 'abcdefghijklmnopqrstuvwxyz'
!
!-----------------------------------------------------------------------
!     capitalize each letter if it is lowecase
!-----------------------------------------------------------------------
!
      string = str
      do i = 1, len_trim(str)
        ic = index(cap, str(i:i))
        if (ic > 0) string(i:i) = low(ic:ic)
      end do
!
      end function to_lower
!
      function replace_str (str, text, repl) result (string)
      implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations
!-----------------------------------------------------------------------
!
      character(*), intent(in) :: str
      character(*), intent(in) :: text
      character(*), intent(in) :: repl
      character(len(str)-len(text)+len(repl)) :: string
!
!-----------------------------------------------------------------------
!     Local variable declarations
!-----------------------------------------------------------------------
!
      integer :: ind, s1, s2
!
!-----------------------------------------------------------------------
!     Replace text with replacement in the given string
!-----------------------------------------------------------------------
!
      string = str
      s1 = len(text)
      s2 = len(repl)
      do
        ind = index(string, text(1:s1))
        if (ind == 0) then
          exit
        else
          string = string(1:ind-1)//repl(1:s2)//string(ind+s1:)
        end if 
      end do
! 
      end function replace_str
!
      end module mod_shared
