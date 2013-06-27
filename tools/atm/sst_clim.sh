#!/bin/bash
# this script is used to create climatological forcing file
# from ERA-Interim SST file
#
# example usage: ./sst_clim.sh 1973 1978

ystr=$1
yend=$2

# function for leap year
is_leap_yr() { ## USAGE: is_leap_yr [year]
  ily_year=${1:-`date +%Y`}
  [ $(( $ily_year % 400)) -eq 0 -o \
    \( $(( $ily_year % 4)) -eq 0 -a \
    $(( $ily_year % 100)) -ne 0 \) ] && {
    _IS_LEAP_YEAR=1
    echo 1
  } || {
    _IS_LEAP_YEAR=0
    echo 0
  }
}

# create temporary file
cdo -s sinfo CAS_SST.nc | grep ":00:00" | grep -v "=" | tr " " "\n" | grep - > & .tmp
yy1=`cat .tmp | head -n 1 | awk -F\- '{print $1}'`
yy2=`cat .tmp | tail -n 1 | awk -F\- '{print $1}'`
echo "[debug] -- from '$yy1' to '$yy2'"

f1=""
f2=""
for i in `seq $yy1 $yy2`
do
  # create annual file
  if [ -e "CAS_${i}_SST.nc" ]; then
    echo "[debug] -- '$i' is already processed! skip it ..."
  else
    ind1=`cat -n .tmp | grep "$i-" | awk '{print $1}' | head -n 1`
    ind2=`cat -n .tmp | grep "$i-" | awk '{print $1}' | tail -n 1`
    echo "[debug] -- $i : $ind1 $ind2"
    ncks -d time,$((ind1-1)),$((ind2-1)) CAS_SST.nc CAS_${i}_SST.nc
  fi
 
  # check: year is leap or not?
  isleap=$(is_leap_yr $i)

  if [ "$isleap" == "1" ]; then
    if [ -e "CAS_${i}_SST_sub.nc" ]; then
      echo "[debug] -- 29th '$i' is already processed! skip it ..."
    else
      cdo -s sinfo CAS_${i}_SST.nc | grep ":00:00" | grep -v "=" | tr " " "\n" | grep - >& .tmp2
      ind3=`cat -n .tmp2 | grep "$i-02-29" | awk '{print $1}' | head -n 1`
      ind4=`cat -n .tmp2 | grep "$i-02-29" | awk '{print $1}' | tail -n 1`
      ind5=`cat .tmp2 | wc -l`
      echo "[debug] -- Feb 29th : $ind3 $ind4 $ind5"
      ncks -O -d time,0,$((ind3-2)) CAS_${i}_SST.nc p1.nc
      ncks -O -d time,$ind4,$((ind5-1)) CAS_${i}_SST.nc p2.nc
      ncrcat p1.nc p2.nc CAS_${i}_SST_sub.nc
      rm -f p1.nc p2.nc
    fi
    f1="$f1 CAS_${i}_SST_sub.nc"
    f2="$f2 CAS_${i}_SST.nc"
  else
    f1="$f1 CAS_${i}_SST.nc"
  fi
done

# create climatology
if [ -e "CAS_SST_clim.nc" ]; then
  echo "[debug] -- climatology file is already created! skip it ..."
else
  ncea -O $f1 CAS_SST_clim.nc
fi

# create leap year climatology
if [ -e "CAS_SST_clim_w29.nc" ]; then
  echo "[debug] -- climatology file is already created! skip it ..."
else
  # calculate climatology only from leap years
  ncea -O $f2 29th.nc

  # split climatology from 29th of Feb
  cdo -s sinfo 29th.nc | grep ":00:00" | grep -v "=" | tr " " "\n" | grep - >& .tmp2
  ind6=`cat -n .tmp2 | grep "02-29" | awk '{print $1}' | head -n 1`
  ind7=`cat -n .tmp2 | grep "02-29" | awk '{print $1}' | tail -n 1`
  echo "[debug] -- Feb 29th : $ind6 $ind7"
  ncks -O -d time,$((ind6-1)),$((ind7-1)) 29th.nc p2.nc

  # split climatology from 28th of Feb
  cdo -s sinfo CAS_SST_clim.nc | grep ":00:00" | grep -v "=" | tr " " "\n" | grep - >& .tmp2
  ind8=`cat -n .tmp2 | grep "02-28" | awk '{print $1}' | tail -n 1`
  ind9=`cat .tmp2 | wc -l | awk '{print $1}'`
  echo "[debug] -- Feb 28th : $ind8 $ind9"
  ncks -O -d time,0,$((ind8-1)) CAS_SST_clim.nc p1.nc
  ncks -O -d time,$((ind8)),$((ind9-1)) CAS_SST_clim.nc p3.nc

  # merge files 
  ncrcat -O p1.nc p2.nc p3.nc CAS_SST_clim_w29.nc
  rm -f p1.nc p2.nc p3.nc 29th.nc
fi

# create data for missing years
for i in `seq $ystr $yend`
do
  if [ -e "CAS_${i}_SST.nc" ]; then
    echo "[debug] -- '$i' is already processed! skip it ..."
  else
    echo "[debug] -- processing '$i' ..."

    # check: year is leap or not?
    isleap=$(is_leap_yr $i)

    if [ "$isleap" == "1" ]; then
      cp -r CAS_SST_clim_w29.nc ${i}.nc
    else
      cp -r CAS_SST_clim.nc ${i}.nc
    fi

    d1=$(date -d 19491201 +%s)
    d2=$(date -d ${i}0101 +%s)
    diff=$((($d2-$d1)/(3600)))
    echo "[debug] -- time difference is '$diff'"

    string="time[\$time]=float($diff+6*array(0,1,\$time));"
    echo $string
    eval ncap2 -h -O -s '$string' ${i}.nc CAS_${i}_SST.nc 
    ncatted -h -a units,time,c,c,"hours since 1949-12-01 00:00:00 UTC" \
            -a long_name,time,c,c,time -a standard_name,time,c,c,time \
            -a calendar,time,c,c,gregorian CAS_${i}_SST.nc
    rm -f ${i}.nc
  fi
done

# merge all data
if [ -e "CAS_SST_v2.nc" ]; then
  echo "[debug] -- CAS_SST_v2.nc is already created! skip it ..."
else
  lst=`ls -al CAS_*_SST.nc | grep -v "sub" | awk '{printf $9" "}'`
  ncrcat $lst CAS_SST_v2.nc
fi
