#!/bin/bash
# this script is used to create climatological forcing file
# from ERA-Interim ICBC files
#
# example usage: ./icbc_clim.sh 1978

yy=$1

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

date2stamp () {
    date --utc --date "$1" +%s
}

dateDiff (){
    case $1 in
        -s)   sec=1;      shift;;
        -m)   sec=60;     shift;;
        -h)   sec=3600;   shift;;
        -d)   sec=86400;  shift;;
        *)    sec=86400;;
    esac
    dte1=$(date2stamp $1)
    dte2=$(date2stamp $2)
    diffSec=$((dte2-dte1))
    if ((diffSec < 0)); then abs=-1; else abs=1; fi
    echo $((diffSec/sec*abs))
}

# loop over files
for mm in `seq -w 1 12`
do
  echo "[debug] -- process '$mm'"

  # get list of files
  lst=`ls -al CAS_ICBC.*${mm}0100.nc | head -n 33 | awk '{printf $9" "}'`

  # special threatment for Feb.
  if [ "$mm" == "02" ]; then
    f1=""
    f2=""

    # loop over files 
    for i in $lst
    do
      ntime=`ncdump -c $i | grep UNLIMITED | awk -F\( '{print $2}' | awk '{print $1}'`
      fdate=`echo $i | awk -F\. '{print $2}'`
      fyear=${fdate:0:4}
      # check: year is leap or not?
      isleap=$(is_leap_yr $fyear)

      if [ "$isleap" == "1" ]; then 
        if [ -e "${i/.nc/_sub.nc}" ]; then
          echo "[debug] -- '$i' is already processed! skip it ..."
        else
          echo "[debug] -- number of time step is '$ntime'. remove last day ..." 
          ncks -d time,0,111 $i ${i/.nc/_sub.nc}
        fi
        f1="$f1 ${i/.nc/_sub.nc}"
        f2="$f2 $i"
      else
        f1="$f1 $i"
      fi
    done 

    # merge files
    if [ -e "${mm}.nc" ]; then
      echo "[debug] -- '${mm}.nc' is already processed! skip it ..."
    else
      isleap=$(is_leap_yr $yy)
      if [ "$isleap" == "1" ]; then
        ncea -O $f1 ${mm}_woleap.nc
        ncea -O $f2 ${mm}_wleap.nc
        ncks -O -d time,112,115 ${mm}_wleap.nc ${mm}_29th.nc
        ncrcat -O ${mm}_woleap.nc ${mm}_29th.nc ${mm}.nc 
      else
        ncea -O $f1 ${mm}.nc
      fi
    fi
  else
    # merge files
    if [ -e "${mm}.nc" ]; then
      echo "[debug] -- '${mm}.nc' is already processed! skip it ..."
    else
      ncea -O $lst ${mm}.nc
    fi
  fi

  # fix time axis
  if [ -e "${mm}_fixed.nc" ]; then
    echo "[debug] -- '${mm}_fixed.nc' is already processed! skip it ..."
  else
    #d1=$(date -d 19491201 +%s)
    #d2=$(date -d ${yy}${mm}01 +%s)
    #diff=$((($d2-$d1)/(3600)))
    diff=$(dateDiff -h "1949-12-01" "$yy-$mm-01")
    echo "[debug] -- time difference is '$diff'"

    string="time[\$time]=float($diff+6*array(0,1,\$time));"
    echo $string
    eval ncap2 -h -O -s '$string' ${mm}.nc ${mm}_fixed.nc
    ncatted -h -a units,time,c,c,"hours since 1949-12-01 00:00:00 UTC" \
            -a long_name,time,c,c,time -a standard_name,time,c,c,time \
            -a calendar,time,c,c,gregorian ${mm}_fixed.nc
  fi

  mv ${mm}_fixed.nc ${yy}_${mm}_fixed.nc
  rm ${mm}.nc
done
