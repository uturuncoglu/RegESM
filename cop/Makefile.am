#-----------------------------------------------------------------------
#
#     This file is part of ITU RegESM.
#
#     ITU RegESM is free software: you can redistribute it and/or modify
#     it under the terms of the GNU General Public License as published by
#     the Free Software Foundation, either version 3 of the License, or
#     (at your option) any later version.
#
#     ITU RegESM is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#     GNU General Public License for more details.
#
#     You should have received a copy of the GNU General Public License
#     along with ITU RegESM.  If not, see <http://www.gnu.org/licenses/>.
#
#-----------------------------------------------------------------------
include $(ESMF_LIB)/esmf.mk

if DO_COMPILE_PARAVIEW
noinst_LIBRARIES = libcop.a
libcop_a_SOURCES = adaptor.cpp \
                   grid.cpp

CXXFLAGS += -I$(PVSRC)/VTK/Common/Core \
            -I$(PVSRC)/VTK/Utilities/KWIML \
            -I$(PVSRC)/VTK/Common/DataModel \
            -I$(PVSRC)/CoProcessing/Catalyst \
            -I$(PVSRC)/CoProcessing/PythonCatalyst \
            -I$(PVSRC)/VTK/Common/DataModel \
            -I$(PVSRC)/VTK/Parallel/MPI \
            -I$(PVDIR)/VTK/Common/Core \
            -I$(PVDIR)/VTK/Utilities/KWIML \
            -I$(PVDIR)/VTK/Common/DataModel \
            -I$(PVDIR)/CoProcessing/Catalyst \
            -I$(PVDIR)/ParaViewCore/ServerManager/SMApplication \
            -I$(PVDIR)/ParaViewCore/ServerManager/Core \
            -I$(PVDIR)/ParaViewCore/ServerImplementation/Core \
            -I$(PVDIR)/ParaViewCore/ClientServerCore/Core \
            -I$(PVDIR)/VTK/Filters/Extraction \
            -I$(PVDIR)/VTK/Filters/Core \
            -I$(PVDIR)/VTK/Filters/General \
            -I$(PVDIR)/VTK/Filters/Statistics \
            -I$(PVDIR)/VTK/Filters/Parallel \
            -I$(PVDIR)/VTK/Filters/Geometry \
            -I$(PVDIR)/VTK/Filters/Modeling \
            -I$(PVDIR)/VTK/Filters/Sources \
            -I$(PVDIR)/VTK/Rendering/Core \
            -I$(PVDIR)/ParaViewCore/VTKExtensions/Core \
            -I$(PVDIR)/CoProcessing/PythonCatalyst \
            -I$(PVDIR)/VTK/Parallel/MPI \
            -I$(PVDIR)/VTK/IO/Image \
            -I.
endif

%.o: %.cpp
	$(ESMF_CXXCOMPILER) $(ESMF_CXXCOMPILEPATHS) $(CXXFLAGS) -c $<

distclean-local:
	rm -f *.o *.a

clean-local:
	rm -f *.o *.a

# ----------------------------------------------------------------------
# DO NOT DELETE THIS LINE -- make depend depends on it.

if DO_COMPILE_PARAVIEW
adaptor.o: adaptor.cpp grid.o
grid.o: grid.cpp grid.h
endif
