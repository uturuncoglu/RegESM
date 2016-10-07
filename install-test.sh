#!/bin/bash
set -ex

PROGS=$1
SRC_LINK=$2

# parameters
OMPI_VER="1.10.2"
CATM_VER="4.5.0"

# source third-party libraries
source $PROGS/env_progs

# install atm model
cd ${PROGS}
if [ "${CATM_VER}" == "4.5.0-rc2" ]; then
  wget "https://gforge.ictp.it/gf/download/frsrelease/250/1555/RegCM-4.5.0-rc2.tar.gz"
fi
if [ "${CATM_VER}" == "4.5.0" ]; then
  wget "https://gforge.ictp.it/gf/download/frsrelease/252/1580/RegCM-4.5.0.tar.gz"
fi
tar -zxvf RegCM-${CATM_VER}.tar.gz > extract.log
rm -f RegCM-${CATM_VER}.tar.gz
mv RegCM-${CATM_VER} atm
cd atm
./configure --prefix=${PROGS}/atm --enable-cpl CC=${CC} FC="${FC} -mno-avx" MPIFC="${PROGS}/openmpi-${OMPI_VER}/bin/mpif90 -mno-avx"
make > make.log
make install >> make.log

# get ocn, rtm and wav sources
cd ${PROGS}
wget "${SRC_LINK}"
tar -zxvf src.tar.gz > extract.log

# install ocn model
cd ${PROGS}
tar -zxvf ocn.tar.gz > extract.log
cd ocn
cat roms-r783/Compilers/Linux-gfortran.mk | sed "s/__NETCDFLIB__/${NETCDF////\/}\/lib/g" | sed "s/__NETCDFINC__/${NETCDF////\/}\/include/g" > tmp
mv tmp roms-r783/Compilers/Linux-gfortran.mk
./build.sh > make.log

# install rtm model
cd ${PROGS}
tar -zxvf rtm.tar.gz > extract.log
cd rtm
cat Makefile | sed "s/__FC__/${FC} -O3 -DCPL/g" | sed "s/__NETCDF__/${NETCDF////\/}/g" > tmp
mv tmp Makefile
make install > make.log

# install wav model
cd ${PROGS}
tar -zxvf wav.tar.gz > extract.log
cd wav
cat mk/.dirset | sed "s/__PRODADMDIR__/${PROGS////\/}\/wav/g" | sed "s/__FCFLAGS__/-O3 -DCPL/g" | sed "s/__NETCDFLIB__/${NETCDF////\/}\/lib/g" | sed "s/__NETCDFINC__/${NETCDF////\/}\/include/g" > tmp
mv tmp mk/.dirset 
cd mk
./create_binaries > make.log
