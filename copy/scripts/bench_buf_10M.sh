#!/usr/bin/env bash

# A script to run selected benchmarks

# Sizes to test
sizes=("10M" 100)
# Buffer sizes to test
bufsizs=(64 128 192 256 384 512 768 1024 1536 2048 3072 4096 6144 8192 12288 16384 24576 32768 49152 65536 98304 131072 196608 262144)
# Methods to test
methods=(posix_buffered)

here=$(dirname "$(realpath "$0")")
cd "${here}/../src" || exit
[[ ! -d ../data/bufsiz_10M ]] && mkdir -p ../data/bufsiz_10M/

for m in "${methods[@]}"; do
	echo "-> Method ${m}"
	for buf in "${bufsizs[@]}"; do
		echo "> Buffer size ${buf}"
		gcc copy.c -Wall -Wextra -Wconversion -DCOPY="${m}" -DBUFFER_SIZE="${buf}" -O2 -o copy
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
		echo "$file" > "../data/bufsiz_10M/${m}_${buf}.csv"
		echo "> File ../data/bufsiz_10M/${m}_${buf}.csv written"
	done
done
