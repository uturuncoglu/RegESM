#!/bin/bash

#############################################################
# Load modules                                              #
#############################################################

. /etc/profile.d/modules.sh

# --- remove all defined modules ---
module --long list >& .log
lstlen=`cat .log | wc -l`
lstlen=$((lstlen-2))
lstmod=`cat .log | tail -n $lstlen | awk '{print $1}'`
for mod in $lstmod
do
  module rm $mod
done

# --- define required modules ---
module load cdo
module load nco

#############################################################
# Parameters                                                #
#############################################################

regcmin="regcm.in_CAS50km"
regesmout="regesmout.txt"
romsin="cas.in"
romsrst="output/ocean_rst.nc"
hdrst="output/hd_rst.nc"
hdout="output/hd_dis.nc"

#############################################################
# RegCM                                                     #
#############################################################

file1=`ls -altr ${regcmin}_* | awk '{print $9}' | tail -n 1`
if [ -z "$file1" ]; then
  echo "[debug] -- '${regcmin}' is already reverted!"
else
  mv ${file1} ${regcmin}
fi

file1=`ls -altr ${regesmout/.txt/_}* | awk '{print $9}' | tail -n 1`
if [ -z "$file1" ]; then
  echo "[debug] -- '${regesmout}' is already reverted!"
else
  mv ${file1} ${regesmout}
fi

#############################################################
# ROMS                                                      #
#############################################################

file1=`ls -altr ${romsin/.in/}_* | awk '{print $9}' | tail -n 1`
if [ -z "$file1" ]; then
  echo "[debug] -- '${romsin}' is already reverted!"
else
  mv ${file1} ${romsin}
fi

file1=`ls -altr ${romsrst/.nc/}_* | awk '{print $9}' | tail -n 1`
if [ -z "$file1" ]; then
  echo "[debug] -- '${romsrst}' is already reverted!"
else
  mv ${file1} ${romsrst}
fi

#############################################################
# HD                                                        #
#############################################################

file1=`ls -altr ${hdout/.nc/}_* | awk '{print $9}' | tail -n 1`
if [ -z "$file1" ]; then
  echo "[debug] -- '${hdout}' is already reverted!"
else
  mv ${file1} ${hdout}
fi

file1=`ls -altr ${hdrst/.nc/}_* | awk '{print $9}' | tail -n 1`
if [ -z "$file1" ]; then
  echo "[debug] -- '${hdrst}' is already reverted!"
else
  mv ${file1} ${hdrst}
fi
