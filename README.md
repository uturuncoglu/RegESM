RegESM: **Reg**ional **E**arth **S**ystem **M**odel
======

[![DOI](https://zenodo.org/badge/7665906.svg)](https://zenodo.org/badge/latestdoi/7665906)
[![Build Status](https://travis-ci.org/uturuncoglu/RegESM.svg?branch=master)](https://travis-ci.org/uturuncoglu/RegESM) still under testing

Supported Components
====================

* Atmosphere (ATM):
    * [RegCM](http://gforge.ictp.it/gf/project/regcm/) version >4.5 (it needs extra patch for co-processing)
    * [WRF](http://www2.mmm.ucar.edu/wrf/users/download/get_source.html) version 3.8.1
* Ocean (OCN): 
    * [ROMS](http://www.myroms.org) revision 809
    * [ROMS-Ice](https://github.com/kshedstrom/roms): The driver is not tested with new CICE implementation !!!
    * [MITgcm](http://mitgcm.org/download/) version c63s 
* River Routing (RTM): 
    * [HD](http://www.mpimet.mpg.de/en/science/the-land-in-the-earth-system/terrestrial-hydrology/hd-model.html) version 1.0.2
* Wave (WAV):
    * [WAM](http://journals.ametsoc.org/doi/pdf/10.1175/1520-0485(1988)018%3C1775:TWMTGO%3E2.0.CO%3B2) version Cycle\_4.5.3\_MPI
* Co-processing (COP):
    * [ParaView](http://www.paraview.org) version 5.4.1: It requires [Catalyst](http://www.paraview.org/in-situ/) module and also GPU with graphics mode enabled.

Prerequisites
=============

* RegESM itself (as a driver)
* Earth System Modeling Framework (ESMF) Library: version [7.1.0](http://www.earthsystemmodeling.org/download/data/releases.shtml#v7_1_0)
* Model Components (patched and installed with coupling support - see documentation for more information)

Documentation
=============
**Table of Contents:**

1. [Model Design](docs/01_Model_Design.md)
2. [Benchmark](docs/02_Benchmark.md)
3. [Installation](docs/03_Installation.md)
4. [Usage](docs/04_Usage.md)
5. [Co-processing](docs/05_Co_Processing.md)
6. [Limitations](docs/06_Limitations.md)
7. [Known Issues](docs/07_Known_Issues.md)

Tutorial
========

A tutorial for two component coupling (RegCM+ROMS) over Black Sea can be found in [here](https://github.com/uturuncoglu/summer_school-resm_2016). This repository is the same one that is used in First Regional Earth Sistem Modeling Summer School 2016 held in ITU, Turkey.

Talks
=====

* Turuncoglu, U.U. and Mahendra Roppa, 2018. A New Modeling Framework for In-Situ Visualization Integrated in Earth System Models, [GTC2018 GPU Technology Conference](https://2018gputechconf.smarteventscloud.com/connect/search.ww#loadSearch-searchPhrase=&searchType=session&tc=0&sortBy=abbreviationSort&p=), March 26-29, 2018, San Jose, CA,
USA. [pdf](https://drive.google.com/file/d/1b1jJodf-FB9GkjKNaZzqvHYe9_FcLJFm/view?usp=sharing)

* Turuncoglu, U.U., 2017. Towards in situ visualization integrated model coupling framework for earth system science, [Fourth Workshop on Coupling Technologies for Earth System Models](https://www.earthsystemcog.org/projects/cw2017/), Princeton, NJ, USA. [pdf](https://drive.google.com/open?id=0B9HveJUZae21MHhkMjB0al93SDQ)

Co-processing Component Generated Movies
========================================

* [Vimeo Site](https://vimeo.com/user63897523)

References
=============

Please cite following publications in case of using RegESM coupled modeling system or any variants of it:

* Turuncoglu, U.U., Onol, B., and Ilicak, M., 2019. A new approach for in situ analysis in fully coupled earth system models. In Proceedings of the Workshop on In Situ Infrastructures for Enabling Extreme-Scale Analysis and Visualization (ISAV '19). ACM, New York, NY, USA, 6-11, https://doi.org/10.1145/3364228.3364231. [link](https://dl.acm.org/citation.cfm?id=3364231)

* Turuncoglu, U.U., 2019. Toward modular in situ visualization in Earth system models: the regional modeling system RegESM 1.1, Geosci. Model Dev., 12, 233-259, https://doi.org/10.5194/gmd-12-233-2019. [link](https://www.geosci-model-dev.net/12/233/2019/)

* Turuncoglu, U.U., 2017. Integration of ParaView Catalyst with Regional Earth System Model, Kitware Source (Software Developer's Quarterly), 42, 7-10. [link](https://blog.kitware.com/integration-of-paraview-catalyst-with-regional-earth-system-model/) 

* Turuncoglu, U.U., Sannino, G., 2016. Validation of newly designed regional earth system model (RegESM) for Mediterranean Basin, Climate Dynamics, 48(9), 2919–2947, doi:10.1007/s00382-016-3241-1 [link](http://link.springer.com/article/10.1007/s00382-016-3241-1)

Previous publications with early version of the two-component (atmosphere-ocean) coupled model:

* Turuncoglu, U. U., Giuliani, G., Elguindi, N., and Giorgi, F., 2013. Modelling the Caspian Sea and its catchment area using a coupled regional atmosphere-ocean model (RegCM4-ROMS): model design and preliminary results, Geosci. Model Dev., 6, 283-299, doi:10.5194/gmd-6-283-2013 [link](http://www.geosci-model-dev.net/6/283/2013/)

Release Notes
=============
## Updates

### New in Version 1.2
* The COP component is optimized for very-high resolution applications

### New in Version 1.1
* New co-processing component is introduced to allow in situ visualization applications for multi-component earth system model configurations
* Updated and revisited model documentation (under doc/ directory)
