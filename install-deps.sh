#!/bin/bash
set -ex

PROGS=$1

# parameters
ZLIB_VER="1.2.8"
HDF5_VER="1.8.16"
NCCC_VER="4.4.0"
NCXX_VER="4.2"
NCFC_VER="4.4.3"
XERC_VER="3.1.3"
OMPI_VER="1.10.2"
ESMF_VER="7_0_0"
CATM_VER="4.5.0-rc2"

CC=gcc
FC=gfortran
CXX=g++

# install zlib
cd ${PROGS}
wget "http://zlib.net/zlib-${ZLIB_VER}.tar.gz"
tar -zxvf zlib-${ZLIB_VER}.tar.gz
rm -f zlib-${ZLIB_VER}.tar.gz
cd zlib-${ZLIB_VER}
./configure --prefix=${PROGS}/zlib-${ZLIB_VER}
make > make.log 
make install >> make.log
export LD_LIBRARY_PATH=${PROGS}/zlib-${ZLIB_VER}/lib:${LD_LIBRARY_PATH}
echo "LD_LIBRARY_PATH=${PROGS}/zlib-${ZLIB_VER}/lib:${LD_LIBRARY_PATH}" >> ~/.bashrc

# install hdf5
cd ${PROGS}
wget "https://www.hdfgroup.org/ftp/HDF5/releases/hdf5-${HDF5_VER}/src/hdf5-${HDF5_VER}.tar.gz" 
tar -zxvf hdf5-${HDF5_VER}.tar.gz
rm -f hdf5-${HDF5_VER}.tar.gz
cd hdf5-${HDF5_VER}
./configure --prefix=${PROGS}/hdf5-${HDF5_VER} --with-zlib=${PROGS}/zlib-${ZLIB_VER} --enable-fortran --enable-cxx CC=${CC} FC=${FC} CXX=${CXX}
make > make.log
make install >> make.log
export HDF5=${PROGS}/hdf5-${HDF5_VER}
export PATH=${HDF5}/bin:${PATH}
export LD_LIBRARY_PATH=${HDF5}/lib:${LD_LIBRARY_PATH}
echo "export HDF5=${PROGS}/hdf5-${HDF5_VER}" >> ~/.bashrc 
echo "export PATH=${HDF5}/bin:${PATH}"
echo "export LD_LIBRARY_PATH=${HDF5}/lib:${LD_LIBRARY_PATH}" >> ~/.bashrc

# install netcdf
cd ${PROGS}
mkdir netcdf-${NCCC_VER}
cd netcdf-${NCCC_VER}
wget "ftp://ftp.unidata.ucar.edu/pub/netcdf/netcdf-${NCCC_VER}.tar.gz"
tar -zxvf netcdf-${NCCC_VER}.tar.gz
rm -f netcdf-${NCCC_VER}.tar.gz
mv netcdf-${NCCC_VER} src
cd src
./configure --prefix=${PROGS}/netcdf-${NCCC_VER} CC=${CC} FC=${FC} LDFLAGS="-L${PROGS}/zlib-${ZLIB_VER}/lib -L${PROGS}/hdf5-${HDF5_VER}/lib" CPPFLAGS="-I${PROGS}/zlib-${ZLIB_VER}/include -I${PROGS}/hdf5-${HDF5_VER}/include"
make > make.log
make install >> make.log
export NETCDF=${PROGS}/netcdf-${NCCC_VER}
export PATH=${NETCDF}/bin:${PATH}
export LD_LIBRARY_PATH=${NETCDF}/lib:${LD_LIBRARY_PATH}
echo "export NETCDF=${PROGS}/netcdf-${NCCC_VER}" >> ~/.bashrc
echo "export PATH=${NETCDF}/bin:${PATH}" >> ~/.bashrc
echo "export LD_LIBRARY_PATH=${NETCDF}/lib:${LD_LIBRARY_PATH}" >> ~/.bashrc

cd ${PROGS}
mkdir netcdf-cxx-${NCXX_VER}
cd netcdf-cxx-${NCXX_VER}
wget "ftp://ftp.unidata.ucar.edu/pub/netcdf/netcdf-cxx-${NCXX_VER}.tar.gz"
tar -zxvf netcdf-cxx-${NCXX_VER}.tar.gz
rm -f netcdf-cxx-${NCXX_VER}.tar.gz
mv netcdf-cxx-${NCXX_VER} src
cd src
./configure --prefix=${PROGS}/netcdf-cxx-${NCXX_VER} CC=${CC} CXX=${CXX} LDFLAGS="-L${PROGS}/zlib-${ZLIB_VER}/lib -L${PROGS}/hdf5-${HDF5_VER}/lib -L${PROGS}/netcdf-${NCCC_VER}/lib" CPPFLAGS="-I${PROGS}/zlib-${ZLIB_VER}/include -I${PROGS}/hdf5-${HDF5_VER}/include -I${PROGS}/netcdf-${NCCC_VER}/include"
make > make.log
make install >> make.log

cd ${PROGS} 
mkdir netcdf-fortran-${NCFC_VER}
cd netcdf-fortran-${NCFC_VER}
wget "ftp://ftp.unidata.ucar.edu/pub/netcdf/netcdf-fortran-${NCFC_VER}.tar.gz"
tar -zxvf netcdf-fortran-${NCFC_VER}.tar.gz
rm -f netcdf-fortran-${NCFC_VER}.tar.gz
mv netcdf-fortran-${NCFC_VER} src 
cd src
./configure --prefix=${PROGS}/netcdf-fortran-${NCFC_VER} CC=${CC} FC=${FC} LDFLAGS="-L${PROGS}/zlib-${ZLIB_VER}/lib -L${PROGS}/hdf5-${HDF5_VER}/lib -L${PROGS}/netcdf-${NCCC_VER}/lib" CPPFLAGS="-I${PROGS}/zlib-${ZLIB_VER}/include -I${PROGS}/hdf5-${HDF5_VER}/include -I${PROGS}/netcdf-${NCCC_VER}/include"
make > make.log
make install >> make.log

cd ${PROGS}/netcdf-${NCCC_VER}/bin
ln -s ../../netcdf-fortran-${NCFC_VER}/bin/* .
cd ${PROGS}/netcdf-${NCCC_VER}/lib
ln -s ../../netcdf-cxx-${NCXX_VER}/lib/* .
rm -rf pkgconfig
ln -s ../../netcdf-fortran-${NCFC_VER}/lib/* .
cd ${PROGS}/netcdf-${NCCC_VER}/include
ln -s ../../netcdf-cxx-${NCXX_VER}/include/* .
ln -s ../../netcdf-fortran-${NCFC_VER}/include/* .

# install xerces
cd ${PROGS}
wget "http://ftp.itu.edu.tr/Mirror/Apache//xerces/c/3/sources/xerces-c-${XERC_VER}.tar.gz"
tar -zxvf xerces-c-${XERC_VER}.tar.gz
rm -f xerces-c-${XERC_VER}.tar.gz
cd xerces-c-${XERC_VER}
./configure --prefix=${PROGS}/xerces-c-${XERC_VER} CC=${CC} CXX=${CXX}
make > make.log
make install >> make.log
export XERCES=${PROGS}/xerces-c-${XERC_VER}
export LD_LIBRARY_PATH=${XERCES}/lib:${LD_LIBRARY_PATH}
echo "export XERCES=${PROGS}/xerces-c-${XERC_VER}" >> ~/.bashrc
echo "export LD_LIBRARY_PATH=${XERCES}/lib:${LD_LIBRARY_PATH}" >> ~/.bashrc

# install openmpi
cd ${PROGS}
wget --no-check-certificate "https://www.open-mpi.org/software/ompi/v1.10/downloads/openmpi-${OMPI_VER}.tar.gz"
tar -zxvf openmpi-${OMPI_VER}.tar.gz
rm -f openmpi-${OMPI_VER}.tar.gz
cd openmpi-${OMPI_VER}
./configure --prefix=${PROGS}/openmpi-${OMPI_VER} CC=${CC} CXX=${CXX} FC=${FC}
make > make.log
make install >> make.log
export PATH=${PROGS}/openmpi-${OMPI_VER}/bin:${PATH}
export LD_LIBRARY_PATH=${PROGS}/openmpi-${OMPI_VER}/lib:${LD_LIBRARY_PATH}
echo "export PATH=${PROGS}/openmpi-${OMPI_VER}/bin:${PATH}" >> ~/.bashrc
echo "export LD_LIBRARY_PATH=${PROGS}/openmpi-${OMPI_VER}/lib:${LD_LIBRARY_PATH}" >> ~/.bashrc

# install esmf
cd ${PROGS}
wget --no-check-certificate "https://sourceforge.net/projects/esmf/files/ESMF_${ESMF_VER}/esmf_${ESMF_VER}_src.tar.gz"
tar -zxvf esmf_${ESMF_VER}_src.tar.gz
rm -f esmf_${ESMF_VER}_src.tar.gz
mv esmf esmf-${ESMF_VER//_/.}
cd esmf-${ESMF_VER//_/.}  
export ESMF_OS=Linux
export ESMF_TESTMPMD=OFF
export ESMF_TESTHARNESS_ARRAY=RUN_ESMF_TestHarnessArray_default
export ESMF_TESTHARNESS_FIELD=RUN_ESMF_TestHarnessField_default
export ESMF_DIR=${PROGS}/esmf-${ESMF_VER//_/.}
export ESMF_TESTWITHTHREADS=OFF
export ESMF_INSTALL_PREFIX=${PROGS}/esmf-${ESMF_VER//_/.}/install_dir
export ESMF_COMM=openmpi
export ESMF_TESTEXHAUSTIVE=ON
export ESMF_BOPT=O
export ESMF_OPENMP=OFF
export ESMF_SITE=default
export ESMF_ABI=64
if [ "$FC" == "gfortran" ]; then
  export ESMF_COMPILER=gfortran
elif [ "$FC" == "ifort" ]; then
  export ESMF_COMPILER=intel 
fi
export ESMF_PIO=internal
export ESMF_NETCDF=split
export ESMF_NETCDF_INCLUDE=${NETCDF}/include
export ESMF_NETCDF_LIBPATH=${NETCDF}/lib 
export ESMF_XERCES=standard
export ESMF_XERCES_INCLUDE=${XERCES}/include
export ESMF_XERCES_LIBPATH=${XERCES}/lib
export ESMF_LIB=${ESMF_INSTALL_PREFIX}/lib/lib${ESMF_BOPT}/${ESMF_OS}.${ESMF_COMPILER}.${ESMF_ABI}.${ESMF_COMM}.${ESMF_SITE}
export ESMFMKFILE=${ESMF_LIB}/esmf.mk
export LD_LIBRARY_PATH=${ESMF_LIB}:${LD_LIBRARY_PATH}
export PATH=${ESMF_DIR}/apps/apps${ESMF_BOPT}/${ESMF_OS}.${ESMF_COMPILER}.${ESMF_ABI}.${ESMF_COMM}.${ESMF_SITE}:${PATH}
export PATH=${ESMF_INSTALL_PREFIX}/bin/bin${ESMF_BOPT}/${ESMF_OS}.${ESMF_COMPILER}.${ESMF_ABI}.${ESMF_COMM}.${ESMF_SITE}:${PATH}

echo "export ESMF_OS=Linux" >> ~/.bashrc
echo "export ESMF_TESTMPMD=OFF" >> ~/.bashrc
echo "export ESMF_TESTHARNESS_ARRAY=RUN_ESMF_TestHarnessArray_default" >> ~/.bashrc
echo "export ESMF_TESTHARNESS_FIELD=RUN_ESMF_TestHarnessField_default" >> ~/.bashrc
echo "export ESMF_DIR=${PROGS}/esmf-${ESMF_VER//_/.}" >> ~/.bashrc
echo "export ESMF_TESTWITHTHREADS=OFF" >> ~/.bashrc
echo "export ESMF_INSTALL_PREFIX=${PROGS}/esmf-${ESMF_VER//_/.}/install_dir" >> ~/.bashrc
echo "export ESMF_COMM=openmpi" >> ~/.bashrc
echo "export ESMF_TESTEXHAUSTIVE=ON" >> ~/.bashrc
echo "export ESMF_BOPT=O" >> ~/.bashrc
echo "export ESMF_OPENMP=OFF" >> ~/.bashrc
echo "export ESMF_SITE=default" >> ~/.bashrc
echo "export ESMF_ABI=64" >> ~/.bashrc
if [ "$FC" == "gfortran" ]; then
  echo "export ESMF_COMPILER=gfortran" >> ~/.bashrc
elif [ "$FC" == "ifort" ]; then
  echo "export ESMF_COMPILER=intel" >> ~/.bashrc
fi 
echo "export ESMF_PIO=internal" >> ~/.bashrc
echo "export ESMF_NETCDF=split" >> ~/.bashrc
echo "export ESMF_NETCDF_INCLUDE=${NETCDF}/include" >> ~/.bashrc
echo "export ESMF_NETCDF_LIBPATH=${NETCDF}/lib" >> ~/.bashrc
echo "export ESMF_XERCES=standard" >> ~/.bashrc
echo "export ESMF_XERCES_INCLUDE=${XERCES}/include" >> ~/.bashrc
echo "export ESMF_XERCES_LIBPATH=${XERCES}/lib" >> ~/.bashrc
echo "export ESMF_LIB=${ESMF_INSTALL_PREFIX}/lib/lib${ESMF_BOPT}/${ESMF_OS}.${ESMF_COMPILER}.${ESMF_ABI}.${ESMF_COMM}.${ESMF_SITE}" >> ~/.bashrc
echo "export ESMFMKFILE=${ESMF_LIB}/esmf.mk" >> ~/.bashrc
echo "export LD_LIBRARY_PATH=${ESMF_LIB}:${LD_LIBRARY_PATH}" >> ~/.bashrc
echo "export PATH=${ESMF_DIR}/apps/apps${ESMF_BOPT}/${ESMF_OS}.${ESMF_COMPILER}.${ESMF_ABI}.${ESMF_COMM}.${ESMF_SITE}:${PATH}" >> ~/.bashrc
echo "export PATH=${ESMF_INSTALL_PREFIX}/bin/bin${ESMF_BOPT}/${ESMF_OS}.${ESMF_COMPILER}.${ESMF_ABI}.${ESMF_COMM}.${ESMF_SITE}:${PATH}" >> ~/.bashrc

make info >> make.log
make >> make.log
make install >> make.log

# install atm model
cd ${PROGS}
if [ "${CATM_VER}" == "4.5.0-rc2" ]; then
  wget "https://gforge.ictp.it/gf/download/frsrelease/250/1555/RegCM-4.5.0-rc2.tar.gz"
fi
tar -zxvf RegCM-${CATM_VER}.tar.gz
rm -f RegCM-${CATM_VER}.tar.gz
cd RegCM-${CATM_VER}
./configure --prefix=${PROGS}/RegCM-${CATM_VER} --enable-cpl CC=${CC} FC=${FC}
make > make.log
make install >> make.log

# install ocn model
#cd ${PROGS}
#cd ocn
#cat ocn/roms-r783/Compilers/Linux-gfortran.mk

# install rtm model
cd ${PROGS}
wget "https://kovan.itu.edu.tr/index.php/s/veMPowZBRF6Z0rp/download"
mv download rtm.tar.gz
tar -zxvf rtm.tar.gz
cd rtm
cat Makefile | sed "s/__FC__/${FC} -O3 -DCPL/g" | sed "s/__NETCDF__/${NETCDF////\/}/g" > tmp
mv tmp Makefile
make install

# install wav model
cd ${PROGS}
wget "https://kovan.itu.edu.tr/index.php/s/FlJNvnVK3BZceM1/download"
mv download wav.tar.gz
tar -zxvf wav.tar.gz
cd wav
cat mk/.dirset | sed "s/__PRODADMDIR__/${PROGS////\/}\/wav/g" | sed "s/__FCFLAGS__/-O3 -DCPL/g" | sed "s/__NETCDFLIB__/${NETCDF////\/}\/lib/g" | sed "s/__NETCDFINC__/${NETCDF////\/}\/include/g" > tmp
mv tmp mk/.dirset 
cd mk
./create_binaries
