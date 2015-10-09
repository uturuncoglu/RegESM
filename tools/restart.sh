#!/bin/bash


#############################################################
# Parameters                                                #
# - Set MPATH to use module tool                            #
# - Set active model components (ATM, OCN and RTM)          #
#   if it is active > 0, otherwise set as zero (0)          #
#   ATM - 0 (None), 1 (RegCM)                               #
#   OCN - 0 (None), 1 (ROMS), 2 (MITgcm)                    #
#   RTM - 0 (None), 1 (HD)                                  #
#   WAV - 0 (None), 1 (WAM)                                 #
# - Set model parameter and stdout files                    #
#   *_PARAM - configuration file                            #
#   *_STOUT - stdout file, if it exists                     #
#############################################################

MPATH="/etc/profile.d"
OPATH="output"
IPATH="input"

ATM=1
ATM_PARAM="regcm.in_MED50km"
ATM_STOUT="regesmout.txt"
OCN=2
OCN_PARAM="data"
OCN_PARAM_ADD="data.cal"
RTM=0
RTM_PARAM="hdini.inp"
WAV=1
WAV_PARAM="WAM_User"

#############################################################
# Load modules                                              #
#############################################################

if [ -e "$MPATH/modules.sh" ];then
  . $MPATH/modules.sh

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
  module load openmpi/1.5.3/intel/2011
  module load netcdf/4.1.2/intel/2011
  module load hdf5/1.8.5-patch1
  module load ncl/5.2.1/gnu/4.1.2
  module load nco
  module load cdo  
fi

#############################################################
# Functions                                                 #
#############################################################

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

#############################################################
# Get parameters                                            #
#############################################################

# get date stamp
dstamp=`date +"%d-%m-%y_%H:%M"`

# get latest SAV file name and query new restart date
fsav=`ls -al $OPATH/*_SAV.* | tail -n 1 | awk '{print $9}'`
mdate1=`echo "$fsav" | awk -F. '{print $2}'`
yy=${mdate1:0:4}
mm=${mdate1:4:2}
dd=${mdate1:6:2}
dstr="$yy-$mm-$dd"
echo "[debug] -- new restart date is $mdate1"

#############################################################
# ATM (RegCM4)                                              #
#############################################################

if [ "$ATM" -eq "1" ];then
  # backup original files
  mv $ATM_STOUT ${ATM_STOUT/.txt/}_$dstamp.txt
  cp $ATM_PARAM ${ATM_PARAM}_$dstamp

  # get latest SAV file name and query new restart date
  fsav=`ls -al $OPATH/*_SAV.* | tail -n 1 | awk '{print $9}'`
  mdate1=`echo "$fsav" | awk -F. '{print $2}'` 
  prefix=`cat $ATM_PARAM | grep domname | awk -F\' '{print $2}'`
  echo "[debug] -- model prefix is $prefix"
  echo "[debug] -- new restart date is $mdate1"

  # mdate1
  # check restart time is same or not?
  str1=`cat $ATM_PARAM | grep "mdate1" | tail -n 1`
  val1=`echo "$str1" | awk -F= '{print $2}' | tr -d ' '`
  val2=$mdate1
  if [ "$val1" == "$val2," ]; then
    echo "[debug] -- the restart time is already changed. do not change it again!"
  else
    str2=${str1/$val1/$val2,}
    cat $ATM_PARAM | sed "s/$str1/$str2/g" > .tmp
    mv .tmp $ATM_PARAM
    echo "[debug] -- mdate1 is changed to '$val2' in '$ATM_PARAM' file."
  fi

  # ifrest 
  # check that the run is restarted before or not?
  str1=`cat $ATM_PARAM | grep "ifrest"`
  if [ -n "`echo $str1 | grep "true"`" ]; then
    echo "[debug] -- the simulation is restarted before. do not change it again!"
  else
    val1=`echo "$str1" | awk -F= '{print $2}'`
    str2=${str1/$val1/ .true. ,}
    cat $ATM_PARAM | sed "s/$str1/$str2/g" > .tmp
    mv .tmp $ATM_PARAM
    echo "[debug] -- ifrest is changed to '.true.' in '$ATM_PARAM' file."
  fi
else
  echo "[debug] -- Skip ATM component! It is not active ..."
fi

#############################################################
# OCN (ROMS)                                                #
#############################################################

if [ "$OCN" -eq "1" ]; then
  # backup parameter files
  cp $OCN_PARAM ${OCN_PARAM/.in/}_$dstamp.in

  # get name of ROMS restart file
  ocnrst=`cat cas.in | grep "RSTNAME ==" | awk '{print $3}'`

  # get list of dates from the ROMS restart file
  cdo -s showdate $ocnrst | tr " " "\n" | grep - >&.tmp

  # fix dates if dstart is used (for example, 306_day calendar) 
  dstart=`ncdump -v dstart $ocnrst | grep "dstart =" | awk '{print $3}'`
  echo "[debug] -- dstart was set to '$dstart'."
  if [ "$dstart" != "0" ]; then
    yy=${dstart:0:4}
    yy=$((yy-1))
    cat .tmp | awk -v var="$yy" -F\- '{print $1+var"-"$2"-"$3}' > .tmp2
    mv .tmp2 .tmp
  fi
 
  # find time indices to split data
  lno=`awk -v dstr=$dstr '{if($1==dstr) print NR}' .tmp`

  # create restart file
  if [ $((lno-1)) -eq "0" ]; then
    echo "[debug] -- the restart file has just created. do not create it again!"
  else
    mv $ocnrst ${ocnrst/.nc/}_$dstamp.nc

    # check perfect restart activated or not?
    pr=`ncdump -h ${ocnrst/.nc/}_$dstamp.nc | grep CPP_options | grep PERFECT_RESTART`
    if [ -z "$pr" ]; then
      echo "[debug] -- normal restart file. retrieving only one (1) time step ... "
      echo "[debug] -- time steps $((lno-3))-$((lno-1))"
      #ncks -d ocean_time,$((lno-1)) output/ocean_rst_$dstamp.nc output/ocean_rst.nc
      ncks -d ocean_time,$((lno-3)),$((lno-1)) ${ocnrst/.nc/}_$dstamp.nc $ocnrst 
    else
      echo "[debug] -- PERFECT_RESTART is activated!!! retrieving three (3) time step ..."
      ncks -d ocean_time,$((lno-3)),$((lno-1)) ${ocnrst/.nc/}_$dstamp.nc $ocnrst 
    fi
    echo "[debug] -- restart file is created for time step '$lno'."
    echo "[debug] -- old one is saved as 'output/ocean_rst_$dstamp.nc'."
  fi

  # modify parameters
  # NRREC
  str1=`cat $OCN_PARAM | grep "NRREC" | head -n 1`
  str2=`echo $str1 | awk -F! '{print $2}'`
  if [ -n "$str1" ]; then
    str1=${str1/\!$str2/""}
  fi
  val1=`echo "$str1" | awk -F"==" '{print $2}' | tr -d ' '`
  if [ -z "$val1" ]; then
    val1=`echo "$str1" | awk -F"=" '{print $2}' | tr -d ' '`
  fi
  val2="-1"
  if [ "$val1" == "$val2" ]; then
    echo "[debug] -- the restart record (NRREC) is already changed. do not change it again!"
  else
    str2=${str1/$val1/$val2}
    cat $OCN_PARAM | sed "s/$str1/$str2/g" > .tmp
    mv .tmp $OCN_PARAM
    echo "[debug] -- NRREC is changed to '$val2' in '$OCN_PARAM' file."
  fi

  # LDEFOUT
  str1=`cat $OCN_PARAM | grep "LDEFOUT" | head -n 1`
  str2=`echo $str1 | awk -F! '{print $2}'`
  str1=${str1/\!$str2/""}
  val1=`echo "$str1" | awk -F"==" '{print $2}' | tr -d ' '`
  if [ -z "$val1" ]; then
    val1=`echo "$str1" | awk -F"=" '{print $2}' | tr -d ' '`
  fi
  val2="F"
  if [ "$val1" == "$val2" ]; then
    echo "[debug] -- the LDEFOUT option is already changed. do not change it again!"
  else
    str3="     `echo "$str1" | awk -F"==" '{print $1}' | tr -d ' '` == $val2 "
    cat $OCN_PARAM | sed "s/$str1/$str3/g" > .tmp
    mv .tmp $OCN_PARAM
    echo "[debug] -- LDEFOUT is changed to '$val2' in '$OCN_PARAM' file."
  fi

  # ININAME
  str1=`cat $OCN_PARAM | grep "ININAME" | head -n 1`
  str2=`echo $str1 | awk -F! '{print $2}'`
  str1=${str1/\!$str2/""}
  val1=`echo "$str1" | awk -F"==" '{print $2}' | tr -d ' '`
  if [ -z "$val1" ]; then
    val1=`echo "$str1" | awk -F"=" '{print $2}' | tr -d ' '`
  fi
  val2="$ocnrst"
  if [ "$val1" == "$val2" ]; then
    echo "[debug] -- the restart file (ININAME) is already changed. do not change it again!"
  else
    str2=${str1/$val1/$val2}
    cat $OCN_PARAM | sed "s:$str1:$str2:g" > .tmp
    mv .tmp $OCN_PARAM
    echo "[debug] -- ININAME is changed to '$val2' in '$OCN_PARAM' file."
  fi
else
  echo "[debug] -- Skip OCN component! It is not active ..."
fi

#############################################################
# OCN (MITgcm)                                              #
#############################################################

if [ "$OCN" -eq "2" ]; then
  # get time step (in sec)
  dt=`cat $OCN_PARAM | grep -v "\#" | grep "deltaT[^a-zA-Z0-9]"`
  dt=`echo $dt | awk -F\= '{print $2}'`
  dt=${dt/.,/}
  dt=${dt/,/}
  dt=${dt/.0/}
  echo "[debug] -- MITgcm : deltaT = $dt"

  # get start time
  sdate=`cat $OCN_PARAM_ADD | grep -v "\#" | grep "startDate_1[^a-zA-Z0-9]"`
  sdate=`echo $sdate | awk -F\= '{print $2}'` 
  sdate=${sdate/.,/}
  sdate=${sdate/,/}  
  echo "[debug] -- MITgcm : startDate_1 = $sdate"  
  sdate2="${sdate:0:4}-${sdate:4:2}-${sdate:6:2}"

  # get time diference
  diff=$(dateDiff -d "$sdate2" "$dstr")
  echo "[debug] -- MITgcm : Pickup File = '`find . -name "pickup.*$((diff*86400/dt)).data"`'"

  # estimate restart time
  str1=`cat $OCN_PARAM | grep -v "\#" | grep "startTime[^a-zA-Z0-9]"`
  val1=`echo $str1 | awk -F\= '{print $2}'`
  val2=$((diff*86400))
  str2=${str1/$val1/$val2,}
  echo "[debug] -- MITgcm : startTime = $val1"
  cp $OCN_PARAM ${OCN_PARAM}_$dstamp
  cat $OCN_PARAM | sed "s/$str1/$str2/g" > .tmp 
  mv .tmp $OCN_PARAM
  echo "[debug] -- startTime is changed to '$val2' in '$OCN_PARAM' file."
fi

#############################################################
# HD                                                        #
#############################################################

if [ "$RTM" -eq "1" ];then
  # get name of restart and output file
  lno=`cat -n $RTM_PARAM | grep TDNRES | awk '{print $1}'`
  hdrst=`head -n $((lno+1)) hdini.inp | tail -n 1`
  lno=`cat -n $RTM_PARAM | grep TDNOUT | awk '{print $1}'`
  hdout=`head -n $((lno+1)) hdini.inp | tail -n 1`
  lno=`cat -n $RTM_PARAM | grep TDNINI | awk '{print $1}'`
  hdini=`head -n $((lno+1)) hdini.inp | tail -n 1`

  # get list of dates from the HD restart file
  cdo -s showdate ${hdrst} | tr " " "\n" | grep - >&.tmp

  # find time indices to split data
  lno=`awk -v dstr=$dstr '{if($1==dstr) print NR}' .tmp`
  if [ -z "$lno" ]; then
    lno=`cat -n .tmp | tail -n 1 | awk '{print $1}'`
  fi
  echo "[debug] -- RTM component restart time index = $lno"

  # get data from restart file 
  ncks -O -d time,$((lno-1)),$((lno-1)) ${hdrst} ${hdini}

  # backup files
  mv ${hdrst} ${hdrst/.nc/}_$dstamp.nc  
  mv ${hdout} ${hdout/.nc/}_$dstamp.nc  
else
  echo "[debug] -- Skip RTM component! It is not active ..."
fi

#############################################################
# WAM                                                       #
#############################################################

if [ "$WAV" -eq "1" ];then
  # backup parameter files
  cp $WAV_PARAM ${WAV_PARAM}_$dstamp.in

  # get atm component stop time
  endd=`cat regcm.in_MED50km | grep mdate2 | awk -F= '{print $2}' | awk -F, '{print $1}'`

  # modify parameters
  # start time
  val1=`cat WAM_User | grep $endd | awk '{print $1}'`
  val2="${dstr//-/}000000"
  if [ "$val1" == "$val2" ]; then
    echo "[debug] -- start date is already changed. do not change it again!"
  else
    cat $WAV_PARAM | sed "s/$val1/$val2/g" > .tmp
    mv .tmp $WAV_PARAM
    echo "[debug] -- start date is changed to '$val2' in '$WAV_PARAM' file."
  fi

  # restart flag
  lno=`cat WAM_User | grep -n "C COLDSTART" | awk -F: '{print $1}'`
  val1=`cat WAM_User | head -n $((lno+2)) | tail -n 1`
  val2="          F"
  if [ "$val1" == "$val2" ]; then
    echo "[debug] -- restart flag is already changed. do not change it again!"
  else  
    cat WAM_User | sed -e "$((lno+2))s/$val1/$val2/" > .tmp
    mv .tmp $WAV_PARAM
    echo "[debug] -- restart flag is changed to '$val2' in '$WAV_PARAM' file."
  fi
else
  echo "[debug] -- Skip WAV component! It is not active ..."
fi
