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
      call NUOPC_FieldDictionaryAddEntry('surface_air_specific_humidity',&
           canonicalUnits='1', defaultLongName='N/A',                   &
           defaultShortName='qsfc', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      call NUOPC_FieldDictionaryAddEntry('shortwave_radiation',         &
           canonicalUnits='W m-2', defaultLongName='N/A',               &
           defaultShortName='swrd', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      call NUOPC_FieldDictionaryAddEntry('longwave_radiation',          &
           canonicalUnits='W m-2', defaultLongName='N/A',               &
           defaultShortName='lwrd', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      call NUOPC_FieldDictionaryAddEntry('downward_longwave_radiation', &
           canonicalUnits='W m-2', defaultLongName='N/A',               &
           defaultShortName='dlwr', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      call NUOPC_FieldDictionaryAddEntry('latent_heat_flux',            &
           canonicalUnits='W m-2', defaultLongName='N/A',               &
           defaultShortName='lhfx', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      call NUOPC_FieldDictionaryAddEntry('sensible_heat_flux',          &
           canonicalUnits='W m-2', defaultLongName='N/A',               &
           defaultShortName='shfx', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      call NUOPC_FieldDictionaryAddEntry('total_precipitation',         &
           canonicalUnits='kg m-2 s-1', defaultLongName='N/A',          &
           defaultShortName='prec', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      call NUOPC_FieldDictionaryAddEntry('evaporation',                 &
           canonicalUnits='kg m-2 s-1', defaultLongName='N/A',          &
           defaultShortName='evap', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      call NUOPC_FieldDictionaryAddEntry('surface_runoff',              &
           canonicalUnits='kg m-2 s-1', defaultLongName='N/A',          &
           defaultShortName='roff', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return

      end subroutine set_field_dir 
!
      end module mod_fields
