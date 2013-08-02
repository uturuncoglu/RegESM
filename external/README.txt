!-----------------------------------------------------------------------
! This folder includes example exchange field tables for coupled
! model. The exchange table basically used to represent the exchange 
! fields between components.
!
! The fileds that are used in this table must be defined in the
! ESMF side. So, it is better to keep the tables as it is. They 
! basically allows to define interpolation and unit conversion easily.
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
! exfield_000.tbl
!-----------------------------------------------------------------------

  - 3 component configuration: RegCM4 + ROMS_Ice + HD
  - ROMS_Ice uses same configuration options with table '000' and

    #define UV_PSOURCE
    #define TS_PSOURCE

  - The order of the rivers in the namelist.rc must be same with the 
    river forcing input file

