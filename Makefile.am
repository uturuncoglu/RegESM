#-----------------------------------------------------------------------
#
#     This file is part of ICTP RegESM.
#
#     ICTP RegESM is free software: you can redistribute it and/or modify
#     it under the terms of the GNU General Public License as published by
#     the Free Software Foundation, either version 3 of the License, or
#     (at your option) any later version.
#
#     ICTP RegESM is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#     GNU General Public License for more details.
#
#     You should have received a copy of the GNU General Public License
#     along with ICTP RegESM.  If not, see <http://www.gnu.org/licenses/>.
#
#-----------------------------------------------------------------------
vpath %.o util:.

SUBDIRS = util

UTILLIBPTH = util
LIBS = -L$(UTILLIBPTH) -lesmutil #-L/home/uturunco/progs/parallel-netcdf-1.3.1/lib -lpnetcdf
CPPFLAGS += -I$(UTILLIBPTH)

if DO_COMPILE_LIBESMF
LIBS += $(ESMFLIBPTH)
CPPFLAGS += $(ESMFINCPTH)
endif

noinst_LIBRARIES =

if DO_COMPILE_LIBATM
LIBS += -L${PWD} -latm
CPPFLAGS += -I$(ATM_PREFIX)/Main -I$(ATM_PREFIX)/Main/mpplib \
            -I$(ATM_PREFIX)/Main/batslib -I$(ATM_PREFIX)/Share
noinst_LIBRARIES += libatm.a
libatm_a_SOURCES = 
libatm_a_LIBADD = $(wildcard $(ATM_PREFIX)/Main/*.o)     \
                  $(wildcard $(ATM_PREFIX)/Main/*/*.o)   \
                  $(wildcard $(ATM_PREFIX)/Main/*/*/*.o) \
                  $(wildcard $(ATM_PREFIX)/Share/*.o)
endif

if DO_COMPILE_LIBOCN
LIBS += -L${PWD} -locn
CPPFLAGS += -I$(OCN_PREFIX)/Build
noinst_LIBRARIES += libocn.a
libocn_a_SOURCES =
skip_files  = $(OCN_PREFIX)/Build/master.o
libocn_a_LIBADD = $(filter-out $(skip_files), $(wildcard $(OCN_PREFIX)/Build/*.o))
endif

bin_PROGRAMS = regesm.x

regesm_x_SOURCES = regesm.F90 mod_esmf_esm.F90 mod_esmf_atm.F90 \
                   mod_esmf_ocn.F90 mod_esmf_rtm.F90 mod_esmf_cpl.F90

FCLD = $(MPIFC)

%.o: %.f90
	$(FC) $(CPPFLAGS) $(FCFLAGS) -c $<

%.o: %.F90
	$(FC) $(CPPFLAGS) $(SVNDEF) $(FCFLAGS) -c $<

distclean-local:
	rm -f *.mod

clean-local:
	rm -f *.mod

# ----------------------------------------------------------------------
# DO NOT DELETE THIS LINE -- make depend depends on it.

#
# RegESM Main Program
#
mod_esmf_atm.o: mod_esmf_atm.F90 mod_types.o
mod_esmf_ocn.o: mod_esmf_ocn.F90 mod_types.o
mod_esmf_rtm.o: mod_esmf_rtm.F90 mod_types.o
mod_esmf_cpl.o: mod_esmf_cpl.F90 mod_types.o
mod_esmf_esm.o: mod_esmf_esm.F90 mod_types.o mod_esmf_atm.o \
                mod_esmf_ocn.o mod_esmf_rtm.o mod_esmf_cpl.o
regesm.o: regesm.F90 mod_esmf_esm.o mod_esmf_atm.o mod_esmf_ocn.o \
          mod_esmf_rtm.o mod_esmf_cpl.o 
	$(MPIFC) $(CPPFLAGS) $(FCFLAGS) -c $<
