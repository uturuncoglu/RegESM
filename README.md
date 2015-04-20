RegESM
======

Regional Earth System Model

Supported Components
====================

* Atmosphere (ATM):
    * RegCM (4.4.5.4) - http://gforge.ictp.it/gf/project/regcm/
* Ocean (OCN): 
    * ROMS (3.7) - http://www.myroms.org or for ice branch https://github.com/kshedstrom/roms
    * MIT-gcm (MITgcm_c63s) - http://mitgcm.org/download/
* River Routing (RTM): 
    * HD (1.0.2) - - slightly modified version for coupling
      http://www.mpimet.mpg.de/en/science/the-land-in-the-earth-system/terrestrial-hydrology/hd-model.html
      http://www.mpimet.mpg.de/en/science/models/model-distribution.html
* Wave (WAV):
    * WAM (Cycle_4.5.3_MPI)

Prerequisites
=============

* RegESM itself (as a driver)
* Earth System Modeling Framework (ESMF, >= esmf-7.0.0b38) Library - http://www.earthsystemmodeling.org
* Model Components (patched and installed with coupling support)
