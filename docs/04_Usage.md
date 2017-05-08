#Chapter 4: Usage

To run the RegESM model, user needs to modify two driver configuration files in addition to the configuration files of individual model components:
* Exchange field table ([exfield.tbl](https://github.com/uturuncoglu/RegESM/blob/master/external/exfield_000.tbl))* Driver configuration file ([namelist.rc](https://github.com/uturuncoglu/RegESM/blob/master/namelist.rc))Then, user can run the RegESM coupled modeling system. Also note that both file (exchange field definition and the driver configuration) must be placed with the same directory as RegESM executable file.

## 4.1 Exchange Field Table (exfield.tbl)

The exchange field table mainly designed to keep the definition of the exchange fields and attributes (description, units, grid location etc.) associated with them. A set of example field table can be seen as following.

**ATM-OCN:**

```
6 atm2ocn T
taux:eastward_10m_wind_stress:2d:bilinear:cross:u:N/m2:m2/s2:cf3:0.0:F
tauy:northward_10m_wind_stress:2d:bilinear:cross:v:N/m2:m2/s2:cf3:0.0:F
psfc:surface_air_pressure:2d:bilinear:cross:cross:mb:mb:1.0:0.0:F
swrd:shortwave_radiation:2d:bilinear:cross:cross:W/m^2:Cm/s:cf2:0.0:T
sflx:water_flux_into_sea_water:2d:bilinear:cross:cross:kg/m^2s:m/s:0.001:0.0:T
nflx:surface_heat_flux:2d:bilinear:cross:cross:W/m^2:Cm/s:cf2:0.0:T
1 ocn2atm T
sst:sea_surface_temperature:2d:bilinear:cross:cross:C:K:1.0:273.16:F
```

**ATM-WAV:**

```
2 atm2wav T
wndu:eastward_10m_wind:bilinear:cross:cross:m/s:m/s:1.0:0.0:F
wndv:northward_10m_wind:bilinear:cross:cross:m/s:m/s:1.0:0.0:F
1 wav2atm T
zo:roughness_length:bilinear:cross:cross:m:m:1.0:0.0:F
```

```
2 atm2wav T
wndu:eastward_10m_wind:bilinear:cross:cross:m/s:m/s:1.0:0.0:F
wndv:northward_10m_wind:bilinear:cross:cross:m/s:m/s:1.0:0.0:F
2 wav2atm T
zo:roughness_length:bilinear:cross:cross:m:m:1.0:0.0:F
ustar:friction_velocity:bilinear:cross:cross:m/s:m/s:1.0:0.0:F
```

```
2 atm2wav T
wdir:wind_direction:bilinear:cross:cross:m/s:m/s:1.0:0.0:F
ustr:friction_velocity:bilinear:cross:cross:m/s:m/s:1.0:0.0:F
2 wav2atm T
zo:roughness_length:bilinear:cross:cross:m:m:1.0:0.0:F
```

**ATM-OCN-RTM:**

```
6 atm2ocn T
taux:eastward_10m_wind_stress:bilinear:cross:u:N/m2:m2/s2:cf3:0.0:F
tauy:northward_10m_wind_stress:bilinear:cross:v:N/m2:m2/s2:cf3:0.0:F
psfc:surface_air_pressure:bilinear:cross:cross:mb:mb:1.0:0.0:F
swrd:shortwave_radiation:bilinear:cross:cross:W/m^2:Cm/s:cf2:0.0:T
sflx:water_flux_into_sea_water:bilinear:cross:cross:kg/m^2s:m/s:0.001:0.0:T
nflx:surface_heat_flux:bilinear:cross:cross:W/m^2:Cm/s:cf2:0.0:T
1 ocn2atm T
sst:sea_surface_temperature:bilinear:cross:cross:C:K:1.0:273.16:F
2 atm2rtm F
rnof:surface_runoff:bilinear:cross:cross:mm/s:m/s:0.001:0.0:F
snof:subsurface_runoff:bilinear:cross:cross:mm/s:m/s:0.001:0.0:F
1 rtm2ocn F
rdis:river_discharge:nearstod:cross:cross:m^3:m^3:1.0:0.0:F
```

**ATM-OCN-COP:**

```
14 atm2cop T
topo:surface_topography:2d:bilinear:cross:cross:m:m:1.0:0.0:F
mask:land_sea_mask:2d:bilinear:cross:cross:fraction:fraction:1.0:0.0:F
tsfc:surface_air_temperature:2d:bilinear:cross:cross:K:C:1.0:-273.16:F
psfc:surface_air_pressure:2d:bilinear:cross:cross:mb:mb:1.0:0.0:F
wndu:eastward_10m_wind:2d:bilinear:cross:cross:m/s:m/s:1.0:0.0:F
wndv:northward_10m_wind:2d:bilinear:cross:cross:m/s:m/s:1.0:0.0:F
prec:total_precipitation:2d:bilinear:cross:cross:kg/m^2s:mm/day:86400.0:0.0:F
tlev:air_temperature:3d:bilinear:cross:cross:K:C:1.0:-273.16:F
qlev:air_specific_humidity:3d:bilinear:cross:cross:kg/kg:kg/kg:1.0:0.0:F
ulev:air_wind_u:3d:bilinear:cross:cross:m/s:m/s:1.0:0.0:F
vlev:air_wind_v:3d:bilinear:cross:cross:m/s:m/s:1.0:0.0:F
wlev:air_wind_w:3d:bilinear:cross:cross:m/s:m/s:1.0:0.0:F
cldfrc:cloud_fraction:3d:bilinear:cross:cross:m/s:m/s:1.0:0.0:F
cldlwc:cloud_liquid_water_content:3d:bilinear:cross:cross:m/s:m/s:1.0:0.0:F
4 ocn2cop T
sst:sea_surface_temperature:2d:bilinear:cross:cross:C:K:1.0:273.16:F
ssh:sea_surface_height:2d:bilinear:cross:cross:m:m:1.0:0.0:F
usfc:x_component_of_surface_current:2d:bilinear:cross:cross:m/s:m/s:1.0:0.0:F
vsfc:y_component_of_surface_current:2d:bilinear:cross:cross:m/s:m/s:1.0:0.0:F
6 atm2ocn T
taux:eastward_10m_wind_stress:2d:bilinear:cross:u:N/m2:m2/s2:cf3:0.0:F
tauy:northward_10m_wind_stress:2d:bilinear:cross:v:N/m2:m2/s2:cf3:0.0:F
psfc:surface_air_pressure:2d:bilinear:cross:cross:mb:mb:1.0:0.0:F
swrd:shortwave_radiation:2d:bilinear:cross:cross:W/m^2:Cm/s:cf2:0.0:T
sflx:water_flux_into_sea_water:2d:bilinear:cross:cross:kg/m^2s:m/s:0.001:0.0:T
nflx:surface_heat_flux:2d:bilinear:cross:cross:W/m^2:Cm/s:cf2:0.0:T
1 ocn2atm T
sst:sea_surface_temperature:2d:bilinear:cross:cross:C:K:1.0:273.16:F
```

**ATM-OCN-RTM-WAV:**

```
6 atm2ocn T
taux:eastward_10m_wind_stress:bilinear:cross:u:N/m2:m2/s2:cf3:0.0:F
tauy:northward_10m_wind_stress:bilinear:cross:v:N/m2:m2/s2:cf3:0.0:F
psfc:surface_air_pressure:bilinear:cross:cross:mb:mb:1.0:0.0:F
swrd:shortwave_radiation:bilinear:cross:cross:W/m^2:Cm/s:cf2:0.0:T
sflx:water_flux_into_sea_water:bilinear:cross:cross:kg/m^2s:m/s:0.001:0.0:T
nflx:surface_heat_flux:bilinear:cross:cross:W/m^2:Cm/s:cf2:0.0:T
1 ocn2atm T
sst:sea_surface_temperature:bilinear:cross:cross:C:K:1.0:273.16:F
2 atm2rtm F
rnof:surface_runoff:bilinear:cross:cross:mm/s:m/s:0.001:0.0:F
snof:subsurface_runoff:bilinear:cross:cross:mm/s:m/s:0.001:0.0:F
1 rtm2ocn F
rdis:river_discharge:nearstod:cross:cross:m^3:m^3:1.0:0.0:F
2 atm2wav T
wdir:wind_direction:bilinear:cross:cross:m/s:m/s:1.0:0.0:F
ustr:friction_velocity:bilinear:cross:cross:m/s:m/s:1.0:0.0:F
2 wav2atm T
zo:roughness_length:bilinear:cross:cross:m:m:1.0:0.0:F

```

As it can be seen from examples, the files have a header section for each coupling direction. In this case the number of field defined in the header must be same with the number of field given after header. Currently, the following coupling directions are supported by **RegESM**.

* **atm2ocn** - sending fields from ATM to OCN (i.e. heat, momentum and fresh water fluxes)
* **atm2rtm** - sending fields from ATM to RTM (surface and sub-surface runoff)
* **atm2wav** - sending fields from ATM to WAV (i.e. surface wind components)
* **atm2cop** - sending fields from ATM to COP (all available 2d/3d fields)
* **ocn2atm** - sending fields from OCN to ATM (sea surface temperature, ice thickness, dynamic masking)
* **ocn2cop** - sending fields from OCN to COP (all available 2d fields)
* **rtm2ocn** - sending fields from RTM to OCN (river discharge)
* **rtm2cop** - sending fields from RTM to COP (all available fields)
* **wav2atm** - sending fields from WAV to ATM (surface roughness etc.)
* **wav2cop** - sending fields from WAV to COP (all available fields)

The next parameter in the header section enables (T) or disables (F) the support of extrapolation (two step interpolation: bilinear + nearest neighbour) for the unmapped grid points. In this case, activating the extrapolation support is suggested. In the future release of ESMF library, there is a plan to support extrapolation through the coastlines or unmapped grid points out-of-the-box.

The detail information about the field definitions after the header section can be found in the following table.

| # | Column Name | Description |
|:---:|:---:|:---:|
| **1** | Short Name | The short name for exchange field. See the available exchange field names from following table. |
| **2** | Standard Name | The standard name for exchange field |
| **3** | Dimension of Variable | **2d** for two-dimensional and **3d** for three-dimensional fields |
| **4** | Interpolation Type | The type of interpolation that is used to transfer data form source to destination grid. It can be defined as **bilinear**, **conserv**, **nearstod**, **neardtos** and **none**. When transferring data from RTM component to the ocean model the interpolation type must be selected as **nearstod**. The **bilinear** type interpolation type must be use to apply conservation correction to the field (bilinear + conservation)|
| **5** | Source Grid Point | This field basically used to define the location of the source field in the computational grid definition (i.e. Arakawa types). It can be defined as **cross**, **dot**, **u** and **v**. |
| **6** | Destination Grid Point | Same with previous but for destination grid |
| **7** | Source Field Unit | It is just for information. The model does not use this field. The space character is not allowed! |
| **8** | Destination Field Unit | Same with previous but for output field. |
| **9** | Scale Factor | Used for the unit conversion of the exchange field. The source field is multiplied by this scale factor after interpolation. The user can define scale factor as a numerical value or can use following predefined values: **cf1** - rho0*cp (rho0 = 1025.0 kg/m^3 is water density, and cp = 3985.0 J/kg/K is heat capacity ), **cf2** - 1/cf1, **cf3** - 1/rho0. It is also possible to use negative symbol (-) along with the shortcuts such as **–cf1**. |
| **10** | Add Offset | It is used for unit conversion of the exchange filed. The value is added to the source field after applying scale factor. The combination of the **scale factor** and **add offset** can be used to convert the units. |
| **11** | Conservation| It can be **T** or **F**. If it is defined as **True** (**T**), then driver applies the difference of source and destination field integral to the destination field. It can be applied in field basis. In general, it can be activated for heat and mass fluxes. Also note that the custom conservation code is uses global conservation.|

|  | ATM | OCN | RTM | WAV | COP | 
|:---:|:---:|:---:|:---:|:---:|:---:|
| **ATM** | - | **psfc** (Surface Pressure, mb), **tsfc** (Air Temperature at 2 meter, K), qsfc (Specific Humidity at 2 meter, kg/kg), **lwrd** (Net Longwave Radiation, W/m^2), **dlwr** (Downward Longwave Radiation, W/m^2), **lhfx** (Latent Heat Flux, W/m^2), **shfx** (Sensible Heat Flux, W/m^2), **prec** (Total Precipitation, m/s), **wndu** (Zonal Wind Component at 10 meter, m/s), **wndv** (Meridional Wind Component at 10 meter, m/s), **swrd** (Net Shortwave Radiation, W/m^2), **dswr** (Downward Shortwave Radiation, W/m^2), **taux** (Zonal Surface Wind Stress Component at 10 meter, N/m^2 or Pa), **tauy** (Meridional Surface Wind Stress Component at 10 meter, N/m^2 or Pa), **wspd** (Surface Wind Speed at 10 meter, m/s), **wdir** (Surface Wind Direction at 10 meter, Radian), **ustr** (Surface Frictional Velocity at 10 meter, m/s), **nflx** (Net Surface Heat Flux, W/m^2), **sflx** (Net Surface Freshwater Flux E-P, m/s), **snow** (Liquid Water Equivalent of Snow Thickness, kg/m^2) | **rnof** (Surface Runoff, m/s), **snof** (Sub-surface Runoff, m/s) | **wndu** (Zonal Wind Component at 10 meter, m/s), **wndv** (Meridional Wind Component at 10 meter, m/s), **wspd** (Surface Wind Speed at 10 meter, m/s), **wdir** (Surface Wind Direction at 10 meter, Radian), **ustr** (Surface Frictional Velocity at 10 meter, m/s) | ALL + **topo** (Topography, m), **mask** (Lan-sea mask, unitless), **tlev** (Air Temperature in Height Levels, K), **qlev** (Relative Humidity in Height Levels, 0-1), **ulev** (Zonal Wind Component in Height Levels, m/s), **vlev** (Meridional Wind Component in Height Levels, m/s), **wlev** (Vertical Wind Component in Height Levels, m/s), **cldfrc** (Cloud Fraction, 0-1), **cldlwc** (Cloud Liquid Water Content, kg/kg) |
| **OCN** | **sst** (Sea Surface Temperature, degC), **sit** (Sea Ice Thickness, m), **msk** (Dynamic Land-Sea Mask, 0-1) | - | - | - | ALL + **depth** (Bathymetry, m), **mask**, (Land-Sea Mask, 0-1), **ssh** (Sea Surface Height, m), **usfc** (Zonal Component of Surface Current, m/s), **vsfc** (Meridional Component of Surface Current, m/s) |
| **RTM** | - | **rdis** (River Discharge, m^3/s or m/s as surface boundary condition) | - | - | ALL |
| **WAV** | **zo** (Surface Roughness Length, m), **ustar** (Surface Frictional Velocity at 10 meter, m/s), **tauw** (Unknown) | - | - | - | ALL |
| **COP** | - | - | - | - | - |

## 4.2 Driver Configuration File (namelist.rc)

The upper level configuration file ([namelist.rc](../namelist.rc)) is manly responsible form:
 
* Definition of required computing resource (PETs in ESMF convention) for the model components 
	* Number of CPU or cores
	* PET Layout: **sequential** / **concurrent**
	* Coupling Type: **explicit** / **semi-implicit**
* Activating extrapolation support
* Changing debug level
* Enabling addtional performance metrics
* Definition of start, stop, restart time and calendar
* Definition of coupling time step both fast and slow time steps through the use of matrix of coupling time step multiplier to calculate fast and slow time steps for data exchange among the components
* Type of river boundary condition (used only when RTM is activated)
* List of active rivers (along with coordinates, number of source point and monthly correction factors) that is used for the RTM coupling (used only when RTM is activated)
* Name of co-processing Python script (used only when COP is activated) 
* Co-processor component 2d decomposition configuration (used only when COP is activated)
* Definition of height/depth levels for vertical interpolation of 3d fields (used only when COP is activated)  

The detailed explanation of each configuration option can be found in following table:

| # | Option | Description |
|:---:|:---:|:---:|
| **1** | **PETLayoutOption** | It is used to define the execution mode of the coupled model. It can be set as **sequential** or **concurrent**. If **sequential** is selected then model components are triggered by sequential fashion (one after another). In this case, the driver assigns same number of processor to all components using **PETs** options of the configuration file except RTM (sequential code and must be 1) component and physically same PETs will be used by the components with order. If **concurrent** option is selected, then model component run in parallel (each component uses own computing resource or PETs). In this case, PETs are not overlapped and it allows parallel execution of the components. The user also note that the informative output of the model components (basically standard output, stdout) can be mixed due to the race conditions among the independent PETs.|
| **2** | **CouplingType** | It is used to set coupling type: **explicit** and **semi-implicit**. The semi-implicit is only valid for **ATM-OCN** type of coupling. |
| **3** | **PETs** | It is used ot define PET distribution of the model components. The number of PETs must be given in a specific order with a space between them. Currently, the order is **ATM**, **OCN**, **RTM**, **WAV** and **COP** respectively. If **sequential** PET layout selected then same number of PET must be given to all components. In contrast to **sequential** execution mode, user might select the **concurrent** type execution in case of running model components in the same time. The number of PETS is assigned to the components based on the given number of processor in the option. The total usage of the PETs will be the sum of PETs defined for components. The **RTM** component is sequential and uses only single processor. If the number of processor sets as -1, then coupler assigns the last PET to the RTM component. The total number of processor used by the job is the sum of **ATM+OCN+WAV+COP**. If the number of processor sets as 1, then coupler uses extra resource also for the RTM component. In this case, total number of processor used by the job is the sum of **ATM+OCN+WAV+COP+1** |
| **4** | **UnmappedFill** | It is used to activate extrapolation for unmapped grid points. It basically aims to handle unaligned land-sea mask among the model components automatically by performing nearest-neighbor type interpolation. It is strongly suggested to keep it activated. Otherwise, user must manually edit land-sea masks of the model components to handle unaligned land-sea masks|
| **5** | **DebugLevel** | It is used to increase level of diagnostic output created by driver. It basically helps to find the source of the possible errors (i.e. wrong grid representation, artifacts in the exchange fields, upper and lower limits of the decomposition elements etc.). Actually, there are five level of debug option and can be in the range of 0-4. **0)** no debug output, **1)** enables minimal debug output from driver component and model only prints informative messages related with the upper and lower bounds of the decomposition elements, name of running components etc., **2)** enables writing grid information of the model components in [VTK](http://www.vtk.org) format. The VTK files can be used to create visualization of the computational grids of the model components. In this case, [ParaView](http://www.paraview.org) and [VisIt](https://visit.llnl.gov) can be used, **3)** enables writing exchange fields to a netCDF file. In this case, files are written to disk in each coupling time step. This option must be use for short runs to check the correctness of the exchange fields. It must be used with caution because it creates lots of file (depends on coupling time step, number of active component and number of exchange field). The user also notes that the exchange fields might be in wrong dimension order (transposed) and this is not a bug or error. It is just related to way of storing arrays in ESMF side (ESMF uses right-hand coordinate system). Also note that in this stage, the unit conversion is not applied to the written files, **4)** it is same with the previous level but in this case the exchange fields are written in ASCII format. Due to the limitation of the keeping track of the unit numbers in Fortran, this level might produce corrupted files and it must be used with caution. The user also notes that all the levels also include the previous level of debug output |
| **6** | **EnablePerfCheck** | It enables extra information about driver performance and can be set as **0** (no performance information) and **1** (performance information) |
| **7** | **Calendar** | This configuration option defines the global calendar type used in the time synchronization among the components. The coupled model currently supports three different calendar option: **gregorian**, **360_day** and **julian**. The driver component basically uses this information to check the components calendars to have a consistent view of the time among the model components. |
| **8** | **StartTime** | It is used to define start time of simulation. It must be consistent among the model components otherwise the driver component triggers an error message and kills all processes |
| **9** | **RestartTime** | It is used to define restart time for the simulation. If start time and restart time differs then driver tries to run the model components in restart mode. It must be consistent among the model components otherwise driver triggers an error message and kills all processes. In this case, [restart.sh](tools/restart.sh) script can be used to organize the configuration files of the model components. Also note that this script does not modify the [namelist.rc](../namelist.rc) and user must need to modify it manually |
| **10** | **StopTime** | It is used to define stop time of simulation. It must be consistent among the model components otherwise the driver component triggers an error message and kills all processes |
| **11** | **TimeStep** | This configuration of option is used to set the coupling interval (the slowest one) for model coupling. The model components might interact with each other with different coupling interval (fast vs. slow time step – asynchronous coupling). In this case, **DividerForTStep** configuration option is used to calculate coupling interval among the components |
| **12** | **DividerForTStep** | It is used to calculate coupling interval among the model components. By using this option, user might define different coupling interval for each coupling direction. This is basically required for asynchronous coupling due to the different response time of the model components and their time resolution limitations. For example, **RTM** component runs in daily time scale and there is no reason to couple **RTM** component with **ATM** and **OCN** with time scale less than one day but **ATM** and **OCN** component might be coupled with 3-hour interval. The matrix is used to divide the time step (defined in **TimeStep** option) to calculate the coupling time step for specified directions. For example, if **Time Step** is defined as 1-day and divider matrix is set to 8 for **ATM-OCN** and **OCN-ATM** direction then the coupling step for **ATM-OCN** and **OCN-ATM** interactions will be 3-hours. Also not that **TimeStep** must be defied as slowest time interval in the configuration |
| **13** | **RiverOpt** | It is used to define the type of river discharge handling. The latest version of the code supports two different options: **1)** the rivers are defined as point sources. In this case, the required configuration must be done in the ocean model side, **2)** the rivers can be defined as surface boundary condition (SBC). In this case, the river discharge data coming from **RTM** component is distributed to the ocean surface using effective radius defined in the **RiverList** parameter |
| **14** | **RiverList** | It is basically required for coupling with **RTM** component. The detailed description of each column in the table is given as follows respectively in the following table. |
| **15** | **CoProcessorScript** | It is used to define visualization pipeline defined by Python script, which is created by [ParaView](http://www.paraview.org) Co-processing plugin |
| **16** | **CoProcessorTile** | It is used to define 2d computational decomposition parameter for **COP**. The number of processor defined in this section must be consistent with the number of processor assgined to **COP** component using **PETs** configuration option |
| **17** | **AtmLevs** | It defines the height levels for vertical interpolation (from sigma to height) of 3d exchange fields coming from **ATM** component |
| **18** | **OcnLevs** | It defines the depth levels for vertical interpolation (from sigma to depth) of 3d exchange fields coming from **OCN** component. This option is not used currently and reserved for future use |

| Column | Description |
|:---:|:---:|
| **1** | The **river position** index (I,J) or geographic coordinate (LON,LAT) pair. The driver is able to calculate indexes from geographic coordinate |
| **2** | The **river type** can be assgined as **0)** not active, **1)** active via RTM and **2)** active but constant monthly values are used from the table | 
| **3** | The **effective radius** is used along with **RiverOpt = 2** and the unit is in kilometer. It controls the area of extent of the river discharge. || **4** | The **river coordinate** is used if **River Type = 0**, then I, J pair must be given. Otherwise LON, LAT is expected. The algorithm finds the closest ocean model grid and distributes the river discharge by using effective radius. |
| **5** | The **river direction** is used only in **ROMS** coupling and **RiverOpt = 1** |
| **6** | The **number of source point for specific river mouth** is used to distribute the river discharge along the source point for same river. The driver basically divides the river discharge to given number to find the discharge of the each individual source point. It is used only in **ROMS** coupling and **RiverOpt = 1** || **7** | The **monthly discharge data** is used if **River type = 1**, then these values are assumed as correction factors or weights (one for each river – total count is 12) for simplified bias correction. The weights must be between 0 and 1 to correct seasonal distribution of river discharge. The correction factors can be calculated by comparing river discharges calculated by **RTM** component and observations. If **River type = 2**, then these values are used as monthly river discharge for specified river. In this case, **RTM** component does not provide discharge for these rivers. This option currently only works with the **RegCM+ROMS+HD** configuration and still testing for **RegCM+MITgcm+HD**.

## 4.3 Running RegESM

Running of the coupled model is very similar to running any other standalone model component. In this case, all the configuration files of individual model components must be in the same directory with the executable.

**PBS script (two component case, RegCM-ROMS):**

```
#!/bin/bash#PBS -N test#PBS -l walltime=24:00:00 #PBS -l nodes=72#PBS -q esp
# Load required modules. /etc/profile.d/modules.shmodule purgemodule load intel/2013module load openmpi/1.6.5/intel/2013

# ESMF specific environment variablessetenv PROGS /home/netapp/clima-users/users/uturunco/progssetenv XERCES $PROGS/xerces-c-3.1.1setenv PNETCDF $PROGS/parallel-netcdf-1.3.1setenv NETCDF $PROGS/netcdf-4.3.0setenv HDF5 $PROGS/hdf5-1.8.11setenv PATH $NETCDF/bin:$PNETCDF/bin:$PATHsetenv LD_LIBRARY_PATH $NETCDF/lib:$PNETCDF/lib:$XERCES/lib:$HDF5/lib:$PROGS/zlib- 1.2.8/lib:$LD_LIBRARY_PATHsetenv ESMF_OS Linuxsetenv ESMF_TESTMPMD OFFsetenv ESMF_TESTHARNESS_ARRAY RUN_ESMF_TestHarnessArray_default setenv ESMF_TESTHARNESS_FIELD RUN_ESMF_TestHarnessField_default setenv ESMF_DIR $PROGS/esmf-6.2.0setenv ESMF_TESTWITHTHREADS OFFsetenv ESMF_INSTALL_PREFIX ${ESMF_DIR}/install_dirsetenv ESMF_COMM openmpisetenv ESMF_TESTEXHAUSTIVE ONsetenv ESMF_BOPT Osetenv ESMF_OPENMP OFFsetenv ESMF_SITE defaultsetenv ESMF_ABI 64setenv ESMF_COMPILER intelsetenv ESMF_PIO internalsetenv ESMF_PNETCDF "standard"setenv ESMF_PNETCDF_INCLUDE ${PNETCDF}/includesetenv ESMF_PNETCDF_LIBPATH ${PNETCDF}/libsetenv ESMF_NETCDF "split"setenv ESMF_NETCDF_INCLUDE ${NETCDF}/includesetenv ESMF_NETCDF_LIBPATH ${NETCDF}/lib
setenv ESMF_XERCES "standard"setenv ESMF_XERCES_INCLUDE ${XERCES}/include setenv ESMF_XERCES_LIBPATH ${XERCES}/libsetenv ESMF_LIB "${ESMF_INSTALL_PREFIX}/lib/lib${ESMF_BOPT}/${ESMF_OS}.${ESMF_COMPILER}.${ESMF_ABI }.${ESMF_COMM}.${ESMF_SITE}"setenv ESMFMKFILE "${ESMF_LIB}/esmf.mk"setenv PATH "${ESMF_DIR}/apps/apps${ESMF_BOPT}/${ESMF_OS}.${ESMF_COMPILER}.${ESMF_ABI}.${ESM F_COMM}.${ESMF_SITE}:${PATH}"setenv LD_LIBRARY_PATH ${ESMF_LIB}:${LD_LIBRARY_PATH}setenv PATH ${ESMF_INSTALL_PREFIX}/bin/bin${ESMF_BOPT}/${ESMF_OS}.${ESMF_COMPILER}.${ESMF_AB I}.${ESMF_COMM}.${ESMF_SITE}:${PATH}
# Run coupled modelcd /home/netapp/clima-users/users/uturunco/MED/RegESM/run2 ulimit -s unlimitedmpirun -v ./regesm.x regcm.in_MED50km >& regesmout.txt
```

**LSF Script (two component case, RegCM-ROMS):**

```
#!/bin/bash#BSUB -P avktis#BSUB -J cpl##BSUB -q mid#BSUB -q deci9#BSUB -m karadeniz_prace #BSUB -o %J.out#BSUB -e %J.err#BSUB -a intelmpi#BSUB -n 32
mpirun.lsf ./regesm.x regcm.in_MED50km med12.in > regesmout.txt
```

**SLURM Script (three component case, RegCM+ROMS+COP):**

```
#!/bin/bash
#SBATCH -A avktis 
#SBATCH -J "katrina_co" 
#SBATCH -p gpuq
#SBATCH -n 56
#SBATCH --output=%j.out
#SBATCH --error=%j.err

export PVDIR=/okyanus/users/uturuncoglu/progs/paraview-5.3.0rc2
export ESMF_LIB="${ESMF_INSTALL_PREFIX}/lib/lib${ESMF_BOPT}/${ESMF_OS}.${ESMF_COMPILER}.${ESMF_ABI}.${ESMF_COMM}.${ESMF_SITE}"
export LD_LIBRARY_PATH=${ESMF_LIB}:${PVDIR}/lib:${LD_LIBRARY_PATH}
export DYLD_LIBRARY_PATH=${ESMF_LIB}:${PVDIR}/lib:${DYLD_LIBRARY_PATH}

ulimit -s unlimited

mpirun time ./regesm.x regcm.in_d2 ocean_gom03_N60.in >&regesmout_r2_${SLURM_NTASKS}.txt
```

