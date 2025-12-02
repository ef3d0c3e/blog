#!/usr/bin/env bash

# A script to run selected benchmarks

# Sizes to test
#sizes=("64" 2000 "128" 2000 "256" 1000 "512" 1000 "1K" 1000 "2K" 1000 "4K" 1000 "8K" 500 "16K" 500 "32K" 250 "64K" 250 "128K" 200 "256K" 150 "512K" 100 "1M" 100)
# Methods to test
#methods=(memcpy)
sizes=("64" 2000 "128" 2000 "256" 1000 "512" 1000 "1K" 1000 "2K" 1000 "4K" 1000 "8K" 500 "16K" 500 "32K" 250 "64K" 250 "128K" 200 "256K" 150 "512K" 100 "1M" 100)
methods=(memcpy_aligned)

here=$(dirname "$(realpath "$0")")
cd "${here}/../src" || exit
[[ ! -d data ]] && mkdir data/

for m in "${methods[@]}"; do
	echo "-> Method ${m}"
	gcc copy.c -Wall -Wextra -Wconversion -DCOPY="${m}" -O2 -o copy
	output=()
	num_sizes=$(echo -e "${sizes[@]}" | wc -w)
	for ((i=0; i<num_sizes; i+=2)); do
		size="${sizes[$((i))]}"
		num="${sizes[$((i+1))]}"
		echo " > Size ${size}/${num}"
		dd if=/dev/random of=/dev/stdout bs="${size}" count=1 > in.bin
		output+=("${size}\n$(./copy ${num})")
	done
	echo "> Done benchmarking ${m}"

	# Get number of rows (assuming all elements have same number of lines)
	num_rows=$(echo -e "${output[0]}" | wc -l)

	# Generate CSV
	file=""
	for ((r=0; r<num_rows; r++)); do
		row=""
		for ((c=0; c<${#output[@]}; c++)); do
			# Extract r-th line of c-th element
			line=$(echo -e "${output[c]}" | sed -n "$((r+1))p")
			row+="$line"
			[[ $c -lt $((${#output[@]}-1)) ]] && row+=","
		done
		file+="${row}"$'\n'
	done
	echo "$file" > "../data/${m}.csv"
	echo "> File ../data/${m}.csv written"
done
