RegESM
======

Regional Earth System Model

Supported Components
====================

* Atmosphere (ATM):
    * RegCM (4.4.5.X) - http://gforge.ictp.it/gf/project/regcm/
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
    * The RegESM version 1.0.0 is compatible with the RegCM version < 4.5 
    * If you plan to use RegCM 4.5 then please use RegESM version 1.1.0
* Earth System Modeling Framework (ESMF, == esmf-7.0.0b38) Library - http://www.earthsystemmodeling.org
* Model Components (patched and installed with coupling support - see user guide under doc/ directory)

References
=============

When using the RegESM coupled modeling system in a paper, the following is the correct citation to use:

* Turuncoglu, U. U., Giuliani, G., Elguindi, N., and Giorgi, F., 2013. Modelling the Caspian Sea and its catchment area using a coupled regional atmosphere-ocean model (RegCM4-ROMS): model design and preliminary results, Geosci. Model Dev., 6, 283-299, doi:10.5194/gmd-6-283-2013

Also note that this reference belongs to old version of the coupled model and the new paper is on the way.
