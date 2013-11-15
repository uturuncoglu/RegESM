!-----------------------------------------------------------------------
! This folder includes example exchange field tables for coupled
! model. The exchange table basically used to represent the exchange 
! fields between components.
!
! The fields that are used in this table must be defined in the
! ESMF side. So, it is better to keep the tables as it is. They 
! basically allows to define interpolation and unit conversion easily.
!
! User have to select one of the table which is sutable for its 
! application and copy or link it to the run directory by modifying
! its name as 'exfield.tbl'
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
! exfield_000.tbl
!-----------------------------------------------------------------------

  - 2 component configuration: RegCM4 + ROMS_Ice
  - ROMS_Ice uses following CPP options

    #define BULK_FLUXES
    #define EMINUSP
    #define EMINUSP_SSH
    #define SHORTWAVE
    #define SPECIFIC_HUMIDITY
    #define COOL_SKIN    
   
!-----------------------------------------------------------------------
! exfield_001.tbl
!-----------------------------------------------------------------------

  - 3 component configuration: RegCM4 + ROMS_Ice + HD
  - ROMS_Ice uses same configuration options with table '000' and

    #define UV_PSOURCE
    #define TS_PSOURCE

  - The order of the rivers in the namelist.rc must be same with the 
    river forcing input file

!-----------------------------------------------------------------------
! exfield_002.tbl
!-----------------------------------------------------------------------

  - Th exchange field table for MIT-gcm

!-----------------------------------------------------------------------
! Field Table
!-----------------------------------------------------------------------

  - Following short names can be used to define the field table.

  - Atmospheric component (ATM) 

    * The units between the parentheses indicates the unit in the RegCM. So,
      the user must provide the correct unit conversion parameters using
      scale_factor and add_offset in the exchange field table.
 
    taux - zonal surface wind stress (N/m^2 or Pa)
    tauy - meridional surface wind stress (N/m^2 or Pa)
    wndu - zonal wind component (m/s)
    wndv - meridional wind component (m/s)
    wspd - wind speed (m/s)
    psfc - surface pressure (hPa or mb)
    tsfc - 2 meter surface temperature (K)
    qsfc - 2 meter specific humidity (kg/kg)
    lwrd - net longwave radiation (W/m^2)
    swrd - net shortwave radiation (W/m^2)
    dlwr - downward longwave radiation (W/m^2)
    dswr - downward shortwave radiation (W/m^2)
    lhfx - latent heat flux (W/m^2)
    shfx - sensible heat flux (W/m^2)
    nflx - net heat flux, latent+sensible+longwave-shortwave (W/m^2)
    prec - total precipitation, P (m/s)
    evap - evaporation, E (m/s)
    sflx - net freshwater flux, E-P (m/s)
    rnof - surface runoff (m/s, just over land)
    snof - sub-surface runoff (m/s, just over land)

  - Ocean component (OCN)

    sst  - sea surface temperature (degC in ROMS and MIT-gcm)    
    sit  - sea ice thickness (m in ROMS)
    msk  - dynamic masking, wet-dry algorithm (unitless in ROMS)

  - River routing component (RTM)

    rdis - river discharge (m^3/s)
