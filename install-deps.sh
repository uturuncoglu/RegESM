#!/bin/sh
set -ex

PROGS=$1

# parameters
ZLIB_VER="1.2.8"
HDF5_VER="1.8.16"

CC=gcc
FC=gfortran
CXX=g++

# install zlib
cd $PROGS
wget "http://zlib.net/zlib-${ZLIB_VER}.tar.gz"
tar -zxvf zlib-${ZLIB_VER}.tar.gz
cd zlib-${ZLIB_VER}
./configure --prefix=${PROGS}/zlib-${ZLIB_VER}
make >& make.log 
make install >>& make.log
export LD_LIBRARY_PATH=${PROGS}/zlib-${ZLIB_VER}/lib:${LD_LIBRARY_PATH}
echo "LD_LIBRARY_PATH=${PROGS}/zlib-${ZLIB_VER}/lib:${LD_LIBRARY_PATH}" >> ~/.bashrc

# install hdf5
cd $PROGS
wget "https://www.hdfgroup.org/ftp/HDF5/releases/hdf5-${HDF5_VER}/src/hdf5-${HDF5_VER}.tar.gz" 
tar -zxvf hdf5-${HDF5_VER}.tar.gz
cd hdf5-${HDF5_VER}
./configure --prefix=$PROGS/hdf5-${HDF5_VER} --with-zlib=$PROGS/zlib-${ZLIB_VER} --enable-fortran --enable-cxx CC=${CC} FC=${FC} CXX=${CXX}
make >& make.log
make install >>& make.log
export LD_LIBRARY_PATH=$PROGS/hdf5-${HDF5_VER}/lib:$LD_LIBRARY_PATH
echo "export LD_LIBRARY_PATH=$PROGS/hdf5-${HDF5_VER}/lib:$LD_LIBRARY_PATH" >> ~/.bashrc
