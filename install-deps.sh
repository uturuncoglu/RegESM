#!/bin/sh
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

CC=gcc
FC=gfortran
CXX=g++

# install zlib
cd ${PROGS}
wget "http://zlib.net/zlib-${ZLIB_VER}.tar.gz"
tar -zxvf zlib-${ZLIB_VER}.tar.gz
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
cd hdf5-${HDF5_VER}
./configure --prefix=${PROGS}/hdf5-${HDF5_VER} --with-zlib=${PROGS}/zlib-${ZLIB_VER} --enable-fortran --enable-cxx CC=${CC} FC=${FC} CXX=${CXX}
make > make.log
make install >> make.log
export LD_LIBRARY_PATH=$PROGS/hdf5-${HDF5_VER}/lib:${LD_LIBRARY_PATH}
echo "export LD_LIBRARY_PATH=$PROGS/hdf5-${HDF5_VER}/lib:${LD_LIBRARY_PATH}" >> ~/.bashrc

# install netcdf
cd ${PROGS}
mkdir netcdf-${NCCC_VER}
cd netcdf-${NCCC_VER}
wget "ftp://ftp.unidata.ucar.edu/pub/netcdf/netcdf-${NCCC_VER}.tar.gz"
tar -zxvf netcdf-${NCCC_VER}.tar.gz
mv netcdf-${NCCC_VER} src
cd src
./configure --prefix=${PROGS}/netcdf-${NCCC_VER} CC=${CC} FC=${FC} LDFLAGS="-L${PROGS}/zlib-${ZLIB_VER}/lib -L${PROGS}/hdf5-${HDF5_VER}/lib" CPPFLAGS="-I${PROGS}/zlib-${ZLIB_VER}/include -I${PROGS}/hdf5-${HDF5_VER}/include"
make > make.log
make install >> make.log

cd ${PROGS}
mkdir netcdf-cxx-${NCXX_VER}
cd netcdf-cxx-${NCXX_VER}
wget "ftp://ftp.unidata.ucar.edu/pub/netcdf/netcdf-cxx-${NCXX_VER}.tar.gz"
tar -zxvf netcdf-cxx-${NCXX_VER}.tar.gz
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
mv netcdf-fortran-${NCFC_VER} src 
cd src
./configure --prefix=${PROGS}/netcdf-fortran-${NCFC_VER} CC=${CC} FC=${FC} LDFLAGS="-L${PROGS}/zlib-${ZLIB_VER}/lib -L${PROGS}/hdf5-${HDF5_VER}/lib -L${PROGS}/netcdf-${NCCC_VER}/lib" CPPFLAGS="-I${PROGS}/zlib-${ZLIB_VER}/include -I${PROGS}/hdf5-${HDF5_VER}/include -I${PROGS}/netcdf-${NCCC_VER}/include"
make > make.log
make install >> make.log

cd ${PROGS}/netcdf-${NCCC_VER}/lib
ln -s ../../netcdf-cxx-${NCXX_VER}/lib/* .
rm -rf pkgconfig
ln -s ../../netcdf-fortran-${NCFC_VER}/lib/* .
cd ${PROGS}/netcdf-${NCCC_VER}/include
ln -s ../../netcdf-cxx-${NCXX_VER}/include/* .
ln -s ../../netcdf-fortran-${NCFC_VER}/include/* .
export NETCDF=${PROGS}/netcdf-${NCCC_VER}
export LD_LIBRARY_PATH=${NETCDF}/lib:${LD_LIBRARY_PATH}
export PATH=${NETCDF}/bin:${PATH}
echo "export NETCDF=${PROGS}/netcdf-${NCCC_VER}" >> ~/.bashrc
echo "export LD_LIBRARY_PATH=${NETCDF}/lib:${LD_LIBRARY_PATH}" >> ~/.bashrc
echo "export PATH=${NETCDF}/bin:${PATH}" >> ~/.bashrc

# install xerces
cd ${PROGS}
wget "http://ftp.itu.edu.tr/Mirror/Apache//xerces/c/3/sources/xerces-c-${XERC_VER}.tar.gz"
tar -zxvf xerces-c-${XERC_VER}.tar.gz
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
cd openmpi-${OMPI_VER}
./configure --prefix=${PROGS}/openmpi-${OMPI_VER} CC=${CC} CXX=${CXX} FC=${FC}
make > make.log
make install >> make.log
export LD_LIBRARY_PATH=${PROGS}/openmpi-${OMPI_VER}/lib:${LD_LIBRARY_PATH}
export PATH=${PROGS}/openmpi-${OMPI_VER}/bin:${PATH}
echo "export LD_LIBRARY_PATH=${PROGS}/openmpi-${OMPI_VER}/lib:${LD_LIBRARY_PATH}" >> ~/.bashrc
echo "export PATH=${PROGS}/openmpi-${OMPI_VER}/bin:${PATH}" >> ~/.bashrc
