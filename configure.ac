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
AC_INIT([RegESM], [1.0.0], [ufuk.turuncoglu@itu.edu.tr])

AC_SUBST([PACKAGE_URL], ["https://github.com/uturuncoglu/RegESM"])

AC_PREREQ([2.64])

LT_INIT

AM_INIT_AUTOMAKE([foreign])

AC_PROG_RANLIB
AC_PROG_CC
AC_PROG_FC

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
            [Path to ATM model (RegCM) installation (default: yes)]),
  [ATM_PREFIX="$withval"], [ATM_PREFIX="${PWD}/src/atm"]
)
AM_CONDITIONAL(DO_COMPILE_LIBATM, [test -d "$ATM_PREFIX"])
AC_SUBST(ATM_PREFIX)

# check OCN
AC_ARG_WITH([ocn],
  AC_HELP_STRING([--with-ocn],
            [Path to OCN model (ROMS or MITgcm) installation (default: yes)]),
  [OCN_PREFIX="$withval"], [OCN_PREFIX="${PWD}/src/ocn"]
)
AM_CONDITIONAL(DO_COMPILE_LIBOCN, [test -d "$OCN_PREFIX"])
AC_SUBST(OCN_PREFIX)

# check OCN model type
OCN_ROMS=0
OCN_MIT=0
AC_CHECK_FILE([$OCN_PREFIX/roms_export.f90],
              [OCN_ROMS=1; AC_MSG_NOTICE([Compiling with support of ROMS ocean model])],
              AC_MSG_WARN([Required source file is not found! Do not activate ROMS ocean model ...]))
if eval "test x$OCN_ROMS = x1"; then
  AC_CHECK_FILE([$OCN_PREFIX/seaice.o],
                [AM_CPPFLAGS="-DOCNICE $AM_CPPFLAGS"; AC_MSG_NOTICE([Compiling with support of ICE model])],
                 AC_MSG_WARN([Compiling without support of ICE model]))
  strlib="`nm $OCN_PREFIX/libNLM.a | grep wetdry_tile`"
#  AM_CONDITIONAL([DEF_WETDRY], [test -n "$strlib"])
#  AM_COND_IF([DEF_WETDRY],
#             [AM_CPPFLAGS="-DOCNWETDRY $AM_CPPFLAGS"; AC_MSG_NOTICE([Compiling with support of WET-DRY])],
#             AC_MSG_WARN([Compiling without support of WET-DRY]))
fi
AC_CHECK_FILE([$OCN_PREFIX/MOM_COMMON_OPTIONS.h],
              [OCN_MIT=1; AC_MSG_NOTICE([Compiling with support of MITgcm ocean model])],
              AC_MSG_WARN([Required source file is not found! Do not activate MITgcm ocean model ...]))
AM_CONDITIONAL(DO_COMPILE_ROMS, [test x"$OCN_ROMS" = x1])
AM_CONDITIONAL(DO_COMPILE_MIT, [test x"$OCN_MIT" = x1])

# check RTM
AC_ARG_WITH([rtm],
  AC_HELP_STRING([--with-rtm],
            [Path to RTM model (HD) installation (default: yes)]),
  [RTM_PREFIX="$withval"], [RTM_PREFIX="${PWD}/src/rtm"]
)
AM_CONDITIONAL(DO_COMPILE_LIBRTM, [test -d "$RTM_PREFIX"])
AC_SUBST(RTM_PREFIX)

# check WAV
AC_ARG_WITH([wav],
  AC_HELP_STRING([--with-wav],
            [Path to WAV model installation (default: yes)]),
  [WAV_PREFIX="$withval"], [WAV_PREFIX="${PWD}/src/wav"]
)
AM_CONDITIONAL(DO_COMPILE_LIBWAV, [test -d "$WAV_PREFIX"])
AC_SUBST(WAV_PREFIX)

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
  if eval "test x"$FC" = xifort"; then
    FCFLAGS="-O0 -g \
             -check arg_temp_created,bounds,format,output_conversion,uninit \
             -traceback -warn all -fp-model precise -fp-model source -ftz \
             -fimf-arch-consistency=true $FCFLAGS"
    AC_SUBST(FCFLAGS)
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
    FCFLAGS="-O3 -fp-model fast=2 -ip $FCFLAGS"
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
                 util/Makefile])
AC_OUTPUT
