#!/usr/bin/env bash

# Benchmark read call overhead

# Buffer sizes to test
bufsizs=(4096 8192 12288 16384 20480 24576 28672 32768 36864 40960 45056 49152 53248 57344 61440 65536)

here=$(dirname "$(realpath "$0")")
cd "${here}/../src" || exit
[[ ! -d ../data/overhead ]] && mkdir -p ../data/overhead/

dd if=/dev/random of=/dev/stdout bs=10M count=1 > in.bin
# Dummy read to spin up SSD cache
gcc buffer_overhead_read.c -Wall -Wextra -Wconversion -DBUFFER_SIZE=4096 -O2 -o read_oh
./read_oh 5 >/dev/null
for buf in "${bufsizs[@]}"; do
	echo "> Buffer size ${buf}"
	gcc buffer_overhead_read.c -Wall -Wextra -Wconversion -DBUFFER_SIZE="${buf}" -O2 -o read_oh
	output="$(./read_oh 500)"
	echo "${output}" > "../data/overhead/read_${buf}.csv"
	echo "> File ../data/overhead/read_${buf}.csv written"
done
