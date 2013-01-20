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
#define FILENAME "util/mod_fields.F90" 
!
!-----------------------------------------------------------------------
!     Module for ESM exchange fields 
!-----------------------------------------------------------------------
!
      module mod_fields
!
!-----------------------------------------------------------------------
!     Used module declarations 
!-----------------------------------------------------------------------
!
      use ESMF
      use NUOPC  
!
      use mod_types
!
      implicit none
      contains
!
      subroutine set_field_dir()
!
!-----------------------------------------------------------------------
!     Local variable declarations 
!-----------------------------------------------------------------------
!
      integer :: rc
!
!-----------------------------------------------------------------------
!     Add required fields to NUOPC field dictionary 
!-----------------------------------------------------------------------
!
      call NUOPC_FieldDictionaryAddEntry('sea_ice_thickness',           &
           canonicalUnits='mm', defaultLongName='N/A',                  &
           defaultShortName='sit', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      call NUOPC_FieldDictionaryAddEntry('surface_air_temperature',     &
           canonicalUnits='K', defaultLongName='N/A',                   &
           defaultShortName='tsfc', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      call NUOPC_FieldDictionaryAddEntry('surface_specific_humidity',   &
           canonicalUnits='1', defaultLongName='N/A',                   &
           defaultShortName='qsfc', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return

      end subroutine set_field_dir 
!
      end module mod_fields
