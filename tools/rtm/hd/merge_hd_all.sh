#!/bin/bash

arr=(`ls -altr hd_dis*.nc | awk '{printf $9" "}'`)
len=${#arr[@]}
len=$((len-1))
for i in `seq 1 $len`
do
  if [ "$i" -lt "2" ]; then
    echo "./merge_hd.sh ${arr[$((i-1))]} ${arr[$i]} merge_${i}.nc"
    ./merge_hd.sh ${arr[$((i-1))]} ${arr[$i]} merge_${i}.nc
  else
    echo "./merge_hd.sh merge_$((i-1)).nc ${arr[$i]} merge_${i}.nc"
    ./merge_hd.sh merge_$((i-1)).nc ${arr[$i]} merge_${i}.nc
  fi
done
