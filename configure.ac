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
AC_INIT([RegESM], [1.2.0], [ufuk.turuncoglu@itu.edu.tr])

AC_SUBST([PACKAGE_URL], ["https://github.com/uturuncoglu/RegESM"])

#AC_PREREQ([2.64])

LT_INIT

AM_INIT_AUTOMAKE([foreign])

AC_PROG_RANLIB
AC_PROG_CC
AC_PROG_CXX
AC_PROG_FC
AC_PROG_SED
AC_PROG_GREP
AC_PROG_AWK

AC_LANG([Fortran])

# check ESMF
AC_ARG_WITH([esmf],
  AC_HELP_STRING([--with-esmf],
            [Path to ESMF installation (default: ESMF_LIB environment)]),
  [ESMF_PREFIX="$withval"], [ESMF_PREFIX="${ESMF_LIB}"]
)
AM_CONDITIONAL(DO_COMPILE_LIBESMF, [test -d "$ESMF_PREFIX"])

# check ATM
AC_ARG_WITH([atm],
  AC_HELP_STRING([--with-atm],
            [Path to ATM model installation (default: none)]),
  [ATM_PREFIX="$withval"], [ATM_PREFIX="none"]
)

AM_CONDITIONAL(DO_COMPILE_LIBATM, [test -d "$ATM_PREFIX"])
AC_SUBST(ATM_PREFIX)

if eval "test x$ATM_PREFIX != xnone"; then
  if eval "! test -d $ATM_PREFIX"; then
    AC_MSG_ERROR([check atmosphere model installation path $ATM_PREFIX])
  fi

  # check RegCM model is installed with coupling support or not?
  NO_RCM_SUPPORT=1
  AC_CHECK_FILE([$ATM_PREFIX/Main/mod_update.o],
                [NO_RCM_SUPPORT=0; AC_MSG_NOTICE([compiling with atmosphere model - RegCM - support])])

  # check WRF model is installed with coupling support or not?
  NO_WRF_SUPPORT=1
  AC_CHECK_FILE([$ATM_PREFIX/main/real_em.o],
                [NO_WRF_SUPPORT=0; AC_MSG_NOTICE([compiling with atmosphere model - WRF - support])])

  if eval "test x$NO_RCM_SUPPORT = x1 -a x$NO_WRF_SUPPORT = x1"; then
    AC_MSG_ERROR([required source files 'mod_update.o' or 'real_em.o' are not found! atmosphere model should be installed with coupling support.])
  fi
fi

AM_CONDITIONAL(DO_COMPILE_RCM, [test x"$NO_RCM_SUPPORT" = x0])
AM_CONDITIONAL(DO_COMPILE_WRF, [test x"$NO_WRF_SUPPORT" = x0])

# check parallel I/O for WRF
if eval "test x$DO_COMPILE_WRF != x0"; then
  WRF_PNETCDF_SUPPORT=0
  AC_CHECK_FILE([$ATM_PREFIX/external/io_pnetcdf/wrf_io.o],
                [WRF_PNETCDF_SUPPORT=1; AC_MSG_NOTICE([compiling WRF with pnetcdf support])])
fi

AM_CONDITIONAL(WRF_PNETCDF_SUPPORT, [test x"$WRF_PNETCDF_SUPPORT" = x1])

# check OCN
AC_ARG_WITH([ocn],
  AC_HELP_STRING([--with-ocn],
            [Path to OCN model installation (default: none)]),
  [OCN_PREFIX="$withval"], [OCN_PREFIX="none"]
)
AM_CONDITIONAL(DO_COMPILE_LIBOCN, [test -d "$OCN_PREFIX"])
AC_SUBST(OCN_PREFIX)

if eval "test x$OCN_PREFIX != xnone"; then
  # check installation path exist or not?  
  if eval "! test -d $OCN_PREFIX"; then
    AC_MSG_ERROR([check ocean model installation path $RTM_PREFIX])
  fi

  # check ROMS model is installed with coupling support or not?
  NO_ROMS_SUPPORT=1
  AC_CHECK_FILE([$OCN_PREFIX/roms_export.f90],
                [NO_ROMS_SUPPORT=0; AC_MSG_NOTICE([compiling with ocean model - ROMS - support])])

  # check MITgcm model is installed with coupling support or not?
  NO_MIT_SUPPORT=1
  AC_CHECK_FILE([$OCN_PREFIX/MOM_COMMON_OPTIONS.h],
                [NO_MIT_SUPPORT=0; AC_MSG_NOTICE([compiling with ocean model - MITgcm - support])])

  if eval "test x$NO_ROMS_SUPPORT = x1 -a x$NO_MIT_SUPPORT = x1"; then
    AC_MSG_ERROR([required source files 'roms_export.f90' or 'MOM_COMMON_OPTIONS.h' are not found! ocean model should be installed with coupling support.])
  fi

  # check ice model support
  if eval "test x$NO_ROMS_SUPPORT = x0"; then
    AC_CHECK_FILE([$OCN_PREFIX/seaice.o],
                  [AM_CPPFLAGS="-DOCNICE $AM_CPPFLAGS"; AC_MSG_NOTICE([compiling ocean model with ice model])],
                   AC_MSG_WARN([compiling ocean model without ice model support !!!]))
  fi

  # check wetting-drying support
  #strlib="`nm $OCN_PREFIX/libNLM.a | grep wetdry_tile`"
  #AM_CONDITIONAL([DEF_WETDRY], [test -n "$strlib"])
  #AM_COND_IF([DEF_WETDRY],
  #           [AM_CPPFLAGS="-DOCNWETDRY $AM_CPPFLAGS"; AC_MSG_NOTICE([compiling ocean model with wetting-drying support])],
  #            AC_MSG_WARN([compiling ocean model without wetting-drying support !!!]))
fi

AM_CONDITIONAL(DO_COMPILE_MIT, [test x"$NO_MIT_SUPPORT" = x0])  
AM_CONDITIONAL(DO_COMPILE_ROMS, [test x"$NO_ROMS_SUPPORT" = x0])

# check RTM
AC_ARG_WITH([rtm],
  AC_HELP_STRING([--with-rtm],
            [Path to RTM model installation (default: none)]),
  [RTM_PREFIX="$withval"], [RTM_PREFIX="none"]
)
AM_CONDITIONAL(DO_COMPILE_LIBRTM, [test -d "$RTM_PREFIX"])
AC_SUBST(RTM_PREFIX)

if eval "test x$RTM_PREFIX != xnone"; then
  if eval "! test -d $RTM_PREFIX"; then
    AC_MSG_ERROR([check river routing model installation path $RTM_PREFIX])
  fi

  NO_RTM_SUPPORT=1
  AC_CHECK_FILE([$RTM_PREFIX/hdmain.o],
                [NO_RTM_SUPPORT=0; AC_MSG_NOTICE([compiling with river routing model - HD - support])])
  if eval "test x$NO_RTM_SUPPORT = x1"; then
    AC_MSG_ERROR([required source file 'hdmain.o' is not found! river routing model should be installed with coupling support.])
  fi
fi

# check WAV
AC_ARG_WITH([wav],
  AC_HELP_STRING([--with-wav],
            [Path to WAV model installation (default: none)]),
  [WAV_PREFIX="$withval"], [WAV_PREFIX="none"]
)
AM_CONDITIONAL(DO_COMPILE_LIBWAV, [test -d "$WAV_PREFIX"])
AC_SUBST(WAV_PREFIX)

if eval "test x$WAV_PREFIX != xnone"; then
  if eval "! test -d $WAV_PREFIX"; then
    AC_MSG_ERROR([check wave model installation path $WAV_PREFIX])
  fi

  NO_WAV_SUPPORT=1
  AC_CHECK_FILE([$WAV_PREFIX/wam_user_interface.o],
                [NO_WAV_SUPPORT=0; AC_MSG_NOTICE([compiling with wave model - WAM - support])])
  if eval "test x$NO_WAV_SUPPORT = x1"; then
  AC_MSG_ERROR([required source file 'wam_user_interface.o' is not found! wave model should be installed with coupling support.])
  fi
fi

# check PARAVIEW
AC_ARG_WITH([paraview],
  AC_HELP_STRING([--with-paraview],
            [Path to ParaView installation (default: none)]),
  [PVDIR="$withval"], [PVDIR="none"]
)
AM_CONDITIONAL(DO_COMPILE_PARAVIEW, [test -d "$PVDIR"])
AC_SUBST(PVDIR)

if eval "test x$PVDIR != xnone"; then
  NO_PARAVIEW_SUPPORT=1
  AC_CHECK_FILE([$PVDIR/Makefile],
                [NO_PARAVIEW_SUPPORT=0; AC_MSG_NOTICE([compiling with co-processing support])])
  if eval "test x$NO_PARAVIEW_SUPPORT = x1"; then
    AC_MSG_ERROR([required file $PVDIR/Makefile is not found!])
  else
    AC_SUBST(PVSRC)
    PVSRC="`cat $PVDIR/Makefile | grep CMAKE_SOURCE_DIR | head -1 | awk -F= '{print $2}' | awk '{$1=$1};1'`"
  fi
fi

# check DEBUG
DEBUG="0"
AC_ARG_ENABLE(debug,
  AC_HELP_STRING([--enable-debug],
                 [Enable debugging flags and per processor log file]),
  [DEBUG="1"]
)
if eval "test x$DEBUG = x1"; then
  AM_CPPFLAGS="-DDEBUG $AM_CPPFLAGS"
  AC_SUBST(AM_CPPFLAGS)
fi

# Set debug/production flags
if eval "test x$DEBUG = x1"; then
  if eval "test x"$FC" = xg95"; then
    FCFLAGS="-O0 -g -Wall -Wextra -fbounds-check -ftrace=full $FCFLAGS"
    AC_SUBST(FCFLAGS)
  fi
  if eval "test x"$FC" = xgfortran"; then
    FCFLAGS="-O0 -g -ffpe-trap=zero,invalid,overflow,underflow -fbacktrace \
            -fdump-core -Wall -pedantic -Wconversion -Wsurprising -Waliasing \
            -Wampersand -Warray-bounds -Wcharacter-truncation \
            -Wline-truncation -Wintrinsics-std -Wno-tabs -Wunderflow \
            -Wunused-parameter -Wno-align-commons $FCFLAGS -fbounds-check \
            -fno-range-check"
    AC_SUBST(FCFLAGS)
  fi
  if eval "test x"$CXX" = xg++"; then
    CXXFLAGS="-O0 -g"
    AC_SUBST(CXXFLAGS)
  fi
  if eval "test x"$FC" = xifort"; then
    FCFLAGS="-O0 -g \
             -check arg_temp_created,bounds,format,output_conversion,uninit \
             -traceback -warn all -fp-model precise -fp-model source -ftz \
             -fimf-arch-consistency=true $FCFLAGS"
    CXXFLAGS="-O0 -g"
    AC_SUBST(FCFLAGS)
    AC_SUBST(CXXFLAGS)
  fi
  if eval "test x"$CXX" = xicpc"; then
    CXXFLAGS="-O0 -g -Wall -Wcheck"
    AC_SUBST(CXXFLAGS)
  fi
  if eval "test x"$FC" = xxlf2003"; then
    FCFLAGS="-O0 -qsuppress=1501-245 -bnoquiet -qcheck -qffltrap=nanq,overflow \
             -qmaxmem=-1 -qzerosize $FCFLAGS"
    AC_SUBST(FCFLAGS)
  fi
  if eval "test x"$FC" = xsunf95"; then
    FCFLAGS="-g -O0 -C -w3 -ansi -xfilebyteorder=big16:%all \
             -D__DATE__="'`date +%D`'" -D__TIME__="'`date +%T`'" $FCFLAGS"
    AC_SUBST(FCFLAGS)
  fi
  if eval "test x"$FC" = xpgf90 -o x"$FC" = xpgf95"; then
    FCFLAGS="-Mbounds -Mcache_align -Mchkfpstk -Mchkstk -Mdalign -Mdclchk \
             -Mdepchk -Miomutex -Mrecursive -Msave -Ktrap=fp -O0 -g $FCFLAGS"
    AC_SUBST(FCFLAGS)
  fi
else
  if eval "test x"$FC" = xg95"; then
    FCFLAGS="-O3 $FCFLAGS"
    AC_SUBST(FCFLAGS)
  fi
  if eval "test x"$FC" = xgfortran"; then
    FCFLAGS="-O3 -mtune=native $FCFLAGS"
    AC_SUBST(FCFLAGS)
  fi
  if eval "test x"$FC" = xifort"; then
    FCFLAGS="-O3 -fp-model precise -fp-model source -ip $FCFLAGS"
    AC_SUBST(FCFLAGS)
  fi
  if eval "test x"$FC" = xxlf2003"; then
    FCFLAGS="-qsuppress=1501-245 -bnoquiet -O3 -qstrict \
             -qmaxmem=-1 -qzerosize $FCFLAGS"
    AC_SUBST(FCFLAGS)
  fi
  if eval "test x"$FC" = xsunf95"; then
    FCFLAGS="-O3 -xfilebyteorder=big16:%all \
             -D__DATE__="'`date +%D`'" -D__TIME__="'`date +%T`'" $FCFLAGS"
    AC_SUBST(FCFLAGS)
  fi
  if eval "test x"$FC" = xpgf90 -o x"$FC" = xpgf95"; then
    FCFLAGS="-fastsse $FCFLAGS"
    AC_SUBST(FCFLAGS)
  fi
fi

# add ESMF directories
AC_SUBST(ESMFINCPTH)
AC_SUBST(ESMFLIBPTH)
if test -z "$ESMF_LIB"; then
   AC_MSG_WARN([ESMF library is required to compile RegESM!])
else
  tmp_inc="`cat $ESMF_LIB/esmf.mk | grep ESMF_F90COMPILEPATHS | awk -F= '{print $2}'`"
  tmp_lib="`cat $ESMF_LIB/esmf.mk | grep ESMF_F90LINKPATHS | awk -F= '{print $2}'`"
  tmp_lib="$tmp_lib `cat $ESMF_LIB/esmf.mk | grep ESMF_F90ESMFLINKLIBS | awk -F= '{print $2}'`"
  ESMFINCPTH="$tmp_inc"
  ESMFLIBPTH="$tmp_lib"
fi

CPPFLAGS="$AM_CPPFLAGS"
AC_SUBST(CPPFLAGS)

# add MPI compiler
ACX_MPI

REGESM_ROOT=${PWD}
AC_SUBST(REGESM_ROOT)
AC_PREFIX_DEFAULT(${PWD})

AC_CONFIG_HEADERS([config.h])
AC_CONFIG_FILES([Makefile \
                 cop/Makefile \
                 util/Makefile])
AC_OUTPUT
