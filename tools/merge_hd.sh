#!/bin/bash

f1=$1
f2=$2

# get time information
#cdo -s showdate $f1 | tr " " "\n" | grep - >&.tmp1
dstr1=`head -n 1 .tmp1`
dstr2=`tail -n 1 .tmp1`
echo "[debug] -- 1st file time interval '$dstr1 - $dstr2'"
#cdo -s showdate $f2 | tr " " "\n" | grep - >&.tmp2
dstr3=`head -n 1 .tmp2`
dstr4=`tail -n 1 .tmp2`
echo "[debug] -- 2nd file time interval '$dstr3 - $dstr4'"

# find time indices to split data
lno=`awk -v dstr3=$dstr3 '{if($1==dstr3) print NR}' .tmp1`
echo "[debug] -- $dstr3 found in line '$lno'"

# split 1st file
#ncks -d time,1,$((lno-2)) $f1 ${f1/.nc/}_split.nc

# merge siplitted file with 2nd file
ncrcat ${f1/.nc/}_split.nc $f2 ${f2/.nc/}_${dstr1}-${dstr4}.nc

