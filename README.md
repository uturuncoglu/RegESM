RegESM
======

Regional Earth System Model

Supported Components
====================

* ATM
    * RegCM (core) - http://gforge.ictp.it/gf/project/regcm/
* OCN: 
    * ROMS (3.5) - http://www.myroms.org or for ice branch https://github.com/kshedstrom/roms
    * MIT-gcm (MITgcm_c63s) - http://mitgcm.org/download/
* RTM: 
    * HD (1.0.2) - http://www.mpimet.mpg.de/en/science/the-land-in-the-earth-system/terrestrial-hydrology/hd-model.html and for license: http://www.mpimet.mpg.de/en/science/models/model-distribution.html

Prerequisites
=============

* RegESM itself (as a driver)
* Earth System Modeling Framework (ESMF, > 6.3.0r - 6.3.0r could be also used after fixed) Library - http://www.earthsystemmodeling.org
* Model Components (patched and installed with coupling support)
