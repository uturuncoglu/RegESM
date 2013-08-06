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

noinst_LIBRARIES = libesmutil.a
libesmutil_a_SOURCES = mod_types.F90 mod_config.F90 mod_utils.F90

CPPFLAGS += -I.
if DO_COMPILE_LIBESMF
LIBS += $(ESMFLIBPTH)
CPPFLAGS += $(ESMFINCPTH)
endif

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

mod_types.o: mod_types.F90
mod_utils.o: mod_utils.F90
mod_config.o: mod_config.F90 mod_types.o
