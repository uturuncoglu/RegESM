# Chapter 5: Installation and Usage of Co-processing Component

To use newly introduced co-processing component (COP) in **RegESM 1.1**, user needs to install additional third-party libraries and tools depending on underlying working environment. In this case, two types of working environment are supported:

1. Single server with access to X window and graphics hardware
2. HPC cluster environment without access to X window
   * Cluster nodes without any graphics hardware (only CPUs) - see **Sec. 5.1**
   * Cluster nodes with graphics hardware (hybrid systems configured with CPUs and GPUs) - see **Sec. 5.2**

In the first case, the installation of [ParaView](http://www.paraview.org) and its co-processing module, which is called as Catalyst, from source is pretty simple and it does not require addtional work except activating [Catalyst](http://www.paraview.org/Wiki/ParaView/Catalyst/Overview) module.

On the other hand, the second case is generally required by the high-resolution applications and requires special attention in the installation of the [ParaView](http://www.paraview.org). ParaView mainly uses accelerators such as NVIDIA GPUs and Intel XeonPhi cards to take advantage of many core to process and render data. In case of lack of required underlaying graphics hardware (GPUs), it also supports processing on CPUs using [MESA](https://www.mesa3d.org) library for software emulation of GPUs.

This section basically aim to give brief information about the installation of ParaView in different working environment and also installing **RegESM** modeling system with co-processing support. 

## 5.1 Installation of ParaView for Use with OSMesa (No Graphics Hardware) 

This section basically follows information that are given in [here](http://www.paraview.org/Wiki/ParaView/ParaView_And_Mesa_3D#Configuring_ParaView_for_use_with_OSMesa) to build ParaView for working environments without graphics hardware such as GPUs. 

**Installing LLVM:**

```
wget http://releases.llvm.org/3.9.1/llvm-3.9.1.src.tar.xz
tar -xvf llvm-3.9.1.src.tar.xz
mv llvm-3.9.1.src llvm-3.9.1
cd llvm-3.9.1
mkdir build
cd build
cmake -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=$PROGS/llvm-3.9.1 -DLLVM_BUILD_LLVM_DYLIB=ON -DLLVM_ENABLE_RTTI=ON -DLLVM_INSTALL_UTILS=ON -DLLVM_TARGETS_TO_BUILD:STRING=X86 -DCMAKE_CXX_FLAGS="-std=c++11" -DBUILD_SHARED_LIBS=ON ../
make
make install
```

**Note:** CMake 3.4.3 or higher is required to install LLVM. Be aware that you might need to use addtional -DCMAKE_SHARED_LINKER_FLAGS="-shared-intel" option in newer version of Intel compilers (17.0.4).

**Installing Mesa:**

```
wget https://mesa.freedesktop.org/archive/mesa-17.0.0.tar.gz
tar -zxvf mesa-17.0.0.tar.gz
cd mesa-17.0.0/
mkdir build
mv * build/.
mv build src
cd src
./configure --prefix=$PROGS/mesa-17.0.0 --enable-opengl --disable-gles1 --disable-gles2 --disable-va --disable-xvmc --disable-vdpau --enable-shared-glapi --disable-texture-float --enable-gallium-llvm --enable-llvm-shared-libs --with-gallium-drivers=swrast,swr --disable-dri --with-dri-drivers= --disable-egl --with-egl-platforms= --disable-gbm --disable-glx --disable-osmesa --enable-gallium-osmesa --with-llvm-prefix=$PROGS/llvm-3.9.1/build
make
make install
```

**Installing ParaView:**

ParaView can be configured with following options,

```
wget -O ParaView-v5.3.0.tar.gz "http://www.paraview.org/paraview-downloads/download.php?submit=Download&version=v5.3&type=source&os=all&downloadFile=ParaView-v5.3.0.tar.gz"
tar -zxvf ParaView-v5.3.0.tar.gz
mv ParaView-v5.3.0 paraview-5.3.0
cd paraview-5.3.0
mkdir src
mv * src/.
mkdir build
cd build
cmake \
  -DCMAKE_BUILD_TYPE=Release                                   \
  -DPARAVIEW_ENABLE_PYTHON=ON                                  \
  -DPARAVIEW_USE_MPI=ON                                        \
  -DPARAVIEW_BUILD_QT_GUI=OFF                                  \
  -DVTK_USE_X=OFF                                              \
  -DOPENGL_INCLUDE_DIR=IGNORE                                  \
  -DOPENGL_xmesa_INCLUDE_DIR=IGNORE                            \
  -DOPENGL_gl_LIBRARY=IGNORE                                   \
  -DOSMESA_INCLUDE_DIR=${MESA_INSTALL_PREFIX}/include          \
  -DOSMESA_LIBRARY=${MESA_INSTALL_PREFIX}/lib/libOSMesa.so     \
  -DVTK_OPENGL_HAS_OSMESA=ON                                   \
  -DVTK_USE_OFFSCREEN=OFF ../src
make
```

Option **-DOSMESA\_LIBRARY** can be also set as **${MESA\_INSTALL\_PREFIX}/lib/libOSMesa.a** to use static library rather than dynamic one.

**Note:** The performance of COP component can be affected in case of using software emulation for rendering. The initial tests show that the OSMesa installation is 10x slower than EGL configuration. 

## 5.2 Installation of ParaView with NVIDIA EGL drivers

In case of using Nvidia GPUs for the rendering, ParaView can be installed with the support of [EGL](https://www.khronos.org/egl) to process and render data without X window. In this case, user requires following components:

* A graphics card driver that supports OpenGL rendering through EGL. 
* EGL headers (does not come with the Nvidia driver) - download from [here](https://www.khronos.org/registry/EGL/)
* Set of definitions in the advance configuration section of ParaView

For configuration of ParaView, **EGL\_INCLUDE\_DIR**, **EGL\_LIBRARY**, **EGL\_gldispatch\_LIBRARY** and **EGL\_opengl\_LIBRARY** must point correct files and folders. More information about the ParaView EGL support and its configuration can be found [here](https://blog.kitware.com/off-screen-rendering-through-the-native-platform-interface-egl/). There is also Nvidia blog entry in [here](https://devblogs.nvidia.com/parallelforall/egl-eye-opengl-visualization-without-x-server/) for more information.

## 5.3 Model Components and COP Support

The newly introduced state-of-art co-processing component initially tested with two component (**ATM** and **OCN**) model configuration but there is a plan to test with more component including RTM and WAV to have better analysis of multi-component model simulations in very high temporal resolution.

To run the coupled modeling system along with **COP** component, user needs to apply minor patch to **ATM** model component. The patch mainly makes three-dimensional fields available to the **COP** component, which were not used in conventional coupled modeling systems. The user also note that three-dimensional fields are interpolated vertically from sigma coordinate to height levels before sending **COP** component. In this case, vertical height levels used for the interpolation is defined in [namelist.rc](../namelist.rc) configuration file. 

The co-processing component is tested with a development version of atmospheric model component (**RegCM**), which does not have required modifications for **COP** coupling. The following command can be used to retrive code (directory called as **r6146**) and apply patch.

```
svn checkout -r6146 --username [TYPE YOUR USER NAME] https://gforge.ictp.it/svn/regcm/branches/regcm-core r6146
cd r6146
wget https://github.com/uturuncoglu/RegESM/raw/master/tools/cop/regcm_r6146_COP.patch
patch -p 1 < regcm_r6146_COP.patch
```

The RegESM modeling system can be installed with COP support using following command,

```
./configure --prefix=`pwd` --with-atm=../../atm/r6146 --with-ocn=../../ocn/Build CC=icc FC=ifort MPIFC=mpiifort CXX=icpc
```

The detailed information about RegESM installation can be found in [Sec. 3](03_Installation.md). In the current version of COP component, only two-dimensional ocean model fields can be processed. This limitation will be removed in the future relase of coupled modeling system by modifiying ocean model component (**OCN**) to allow exchanging three-dimensional fields along with vertical interpolation (only for **ROMS** and from s-coordinate to depth levels). 

## References

**[1]** http://www.paraview.org/catalyst-adaptors/
