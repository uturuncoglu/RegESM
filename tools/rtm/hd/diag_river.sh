#!/bin/bash

arr=(`ls -altr regesmout*.txt | awk '{printf $9" "}'`)
len=${#arr[@]}
len=$((len-1))

for j in `seq 1 4`
do
  echo "--- $j : data_0$j.txt ---"

  # remove file
  rm -f data_0$j.txt

  for i in `seq 1 $len`
  do
    # get restart date
    rdate=`cat ${arr[$i]} | grep "River (0$j)" | grep "00:00:00" \
          | head -n 1 | awk -F\[ '{print $2}' | awk -F\] '{print $1}'`
  
    # get line number
    cat ${arr[$((i-1))]} | grep "River (0$j)" | grep "00:00:00" > .tmp
    lno=`cat -n .tmp | grep "$rdate" | awk '{print $1}'`
    echo $rdate $lno
  
    # append data
    cat .tmp | head -n $((lno-1)) >> data_0$j.txt
  done
  
  # append last file to the file
  cat ${arr[$len]} | grep "River (0$j)" | grep "00:00:00" >> data_0$j.txt

  # add first line
  fline=`cat data_0$j.txt | head -n 1` 
  echo "$fline" |  sed -e "s/02T00/01T00/g" | cat - data_0$j.txt > temp && mv temp data_0$j.txt
done
