RegESM
======

Regional Earth System Model

[![Build Status](https://travis-ci.org/uturuncoglu/RegESM.svg?branch=master)](https://travis-ci.org/uturuncoglu/RegESM)

Supported Components
====================

* Atmosphere (ATM):
    * RegCM (4.4.5.10 and 4.5) - http://gforge.ictp.it/gf/project/regcm/
* Ocean (OCN): 
    * ROMS (3.7) - http://www.myroms.org or for ice branch https://github.com/kshedstrom/roms
    * MITgcm (MITgcm_c63s) - http://mitgcm.org/download/
* River Routing (RTM): 
    * HD (1.0.2)
      http://www.mpimet.mpg.de/en/science/the-land-in-the-earth-system/terrestrial-hydrology/hd-model.html
      http://www.mpimet.mpg.de/en/science/models/model-distribution.html
* Wave (WAV):
    * WAM (Cycle_4.5.3_MPI)

Prerequisites
=============

* RegESM itself (as a driver)
    * The RegESM version 1.0.0-beta.5 is compatible with the RegCM version < 4.5 
    * If you plan to use RegCM 4.5 (includes new non-hydrostatic core) then please use RegESM version 1.0.0
* Earth System Modeling Framework (ESMF, == esmf-7.0.0) Library
  http://www.earthsystemmodeling.org/download/data/releases.shtml#v7_0_0
* Model Components (patched and installed with coupling support - see user guide under doc/ directory)

Tutorial
========

A tutorial for two component coupling (RegCM+ROMS) over Black Sea can be found in [here](https://github.com/uturuncoglu/summer_school-resm_2016). This repository is the same one that is used in Regional Earth Sistem Modeling Summer School 2016 held in ITU, Turkey.

References
=============

Please cite following publications in case of using RegESM coupled modeling system: 

* Turuncoglu, U.U., Sannino, G., 2016. Validation of newly designed regional earth system model (RegESM) for Mediterranean Basin, Climate Dynamics, doi:10.1007/s00382-016-3241-1

Previous publications with early version of the coupled model:

* Turuncoglu, U. U., Giuliani, G., Elguindi, N., and Giorgi, F., 2013. Modelling the Caspian Sea and its catchment area using a coupled regional atmosphere-ocean model (RegCM4-ROMS): model design and preliminary results, Geosci. Model Dev., 6, 283-299, doi:10.5194/gmd-6-283-2013
