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
LIBS = -L$(UTILLIBPTH) -lesmutil -L/home/uturunco/progs/parallel-netcdf-1.3.1/lib -lpnetcdf
CPPFLAGS += -I$(UTILLIBPTH) -I/home/uturunco/progs/parallel-netcdf-1.3.1/include

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

SFILES = ""
if DO_COMPILE_LIBOCN
LIBS += -L${PWD} -locn
CPPFLAGS += -I$(OCN_PREFIX)
noinst_LIBRARIES += libocn.a
libocn_a_SOURCES =

if DO_COMPILE_ROMS
  SFILES += $(OCN_PREFIX)/master.o
endif
if DO_COMPILE_MIT 
  SFILES += $(OCN_PREFIX)/main.o
endif

OFILES = $(filter-out $(SFILES), $(wildcard $(OCN_PREFIX)/*.o))
libocn_a_LIBADD = $(OFILES)
endif

if DO_COMPILE_LIBRTM
LIBS += -L${PWD} -lrtm
CPPFLAGS += -I$(RTM_PREFIX)
noinst_LIBRARIES += librtm.a
librtm_a_SOURCES =
skip_files = $(RTM_PREFIX)/hdmain.o
librtm_a_LIBADD = $(filter-out $(skip_files), $(wildcard $(RTM_PREFIX)/*.o))
endif

bin_PROGRAMS = regesm.x

regesm_x_SOURCES = regesm.F90 mod_esmf_esm.F90 mod_esmf_cpl.F90

if DO_COMPILE_LIBATM
regesm_x_SOURCES += mod_esmf_atm.F90
else
regesm_x_SOURCES += mod_esmf_atm_void.F90
endif

if DO_COMPILE_LIBOCN
if DO_COMPILE_ROMS
regesm_x_SOURCES += mod_esmf_ocn_roms.F90
endif
if DO_COMPILE_MIT
regesm_x_SOURCES += mod_esmf_ocn_mit.F90
endif
else
regesm_x_SOURCES += mod_esmf_ocn_void.F90
endif

if DO_COMPILE_LIBRTM
regesm_x_SOURCES += mod_esmf_rtm.F90
else
regesm_x_SOURCES += mod_esmf_rtm_void.F90
endif

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
OBJ=
if DO_COMPILE_LIBATM
mod_esmf_atm.o: mod_esmf_atm.F90 mod_types.o
OBJ+=mod_esmf_atm.o
else
mod_esmf_atm_void.o: mod_esmf_atm_void.F90 mod_types.o
OBJ+=mod_esmf_atm_void.o
endif

if DO_COMPILE_LIBOCN
if DO_COMPILE_ROMS
mod_esmf_ocn_roms.o: mod_esmf_ocn_roms.F90 mod_types.o
OBJ+=mod_esmf_ocn_roms.o
endif
if DO_COMPILE_MIT
#SIZE_F90.h: $(OCN_PREFIX)/SIZE.h
#	cat $< | awk -f util/awk.prog > $@
#CPP_EEMACROS_F90.h: $(OCN_PREFIX)/CPP_EEMACROS.h
#	cat $< | awk -f util/awk.prog > $@
#EEPARAMS_F90.h: $(OCN_PREFIX)/EEPARAMS.h CPP_EEMACROS_F90.h
#	cat $< | awk -f util/awk.prog > $@
#GRID_F90.h: $(OCN_PREFIX)/GRID.h GRID_MACROS_F90.h
#	cat $< | awk -f util/awk.prog > $@
#GRID_MACROS_F90.h: $(OCN_PREFIX)/GRID_MACROS.h
#	cat $< | awk -f util/awk.prog > $@
#mit_code.FXX: SIZE_F90.h EEPARAMS_F90.h GRID_F90.h
#	touch $@
#mit_code.F90: mit_code.FXX
#	cpp $< > mit_code.FX1
#	cat mit_code.FX1 | sed -e 's/\([0-9]\) d \([0-9]\)/\1d\2/g' > $@	
#	rm -f mit_code.FX1
#mod_esmf_ocn_mit.o: mod_esmf_ocn_mit.F90 mit_code.F90 mod_types.o
mod_esmf_ocn_mit.o: mod_esmf_ocn_mit.F90 mod_types.o mod_mit_gcm.o
mod_mit_gcm.o: mod_mit_gcm.F90
OBJ+=mod_esmf_ocn_mit.o
endif
else
mod_esmf_ocn_void.o: mod_esmf_ocn_void.F90 mod_types.o
OBJ+=mod_esmf_ocn_void.o
endif

if DO_COMPILE_LIBRTM
mod_esmf_rtm.o: mod_esmf_rtm.F90 mod_types.o
OBJ+=mod_esmf_rtm.o
else
mod_esmf_rtm_void.o: mod_esmf_rtm_void.F90 mod_types.o
OBJ+=mod_esmf_rtm_void.o
endif

mod_esmf_cpl.o: mod_esmf_cpl.F90 mod_types.o

mod_esmf_esm.o: mod_esmf_esm.F90 mod_types.o $(OBJ) mod_esmf_cpl.o

regesm.o: regesm.F90 $(OBJ) mod_esmf_esm.o mod_esmf_cpl.o 
	$(MPIFC) $(CPPFLAGS) $(FCFLAGS) -c $<
