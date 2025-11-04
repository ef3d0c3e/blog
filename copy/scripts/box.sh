#!/usr/bin/env bash

# A script to output benchmarks for box plots

sizes=("512" "1K" "2K" "4K" "8K" "16K" "32K" "64K" "128K" "256K" "512K" "1M")
methods=(stdio)

[[ ! -d data ]] && mkdir data/
for m in "${methods[@]}"; do
	echo "size,min,q1,med,q3,max,avg" > "data/box_${m}.csv"
done

for size in "${sizes[@]}"; do
	echo " -> Size ${size}"
	dd if=/dev/random of=/dev/stdout bs="${size}" count=1 > in.bin
	for m in "${methods[@]}"; do
		echo "> Method ${m}"
		gcc copy.c -Wall -Wextra -Wconversion -DCOPY="${m}" -O2
		ser=$(./a.out 512 box)
		printf "%s,%s\n" "${size}" "${ser}" >> "data/box_${m}.csv"
	done
done
