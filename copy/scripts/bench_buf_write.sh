#!/usr/bin/env bash

# Benchmark write call overhead

# Buffer sizes to test
bufsizs=(4096 8192 12288 16384 20480 24576 28672 32768 36864 40960 45056 49152 53248 57344 61440 65536 73728 81920 90112 98304 106496 114688 122880 131072 139264 147456 155648 163840 172032 180224 188416 196608 204800 212992 221184 229376 237568 245760 253952 262144 270336)

here=$(dirname "$(realpath "$0")")
cd "${here}/../src" || exit
[[ ! -d ../data/overhead ]] && mkdir -p ../data/overhead/

dd if=/dev/random of=/dev/stdout bs=10M count=1 > in.bin
# Dummy read to spin up SSD cache
gcc buffer_overhead_write.c -Wall -Wextra -Wconversion -DBUFFER_SIZE=4096 -O2 -o write_oh
./write_oh 5 >/dev/null
for buf in "${bufsizs[@]}"; do
	echo "> Buffer size ${buf}"
	gcc buffer_overhead_write.c -Wall -Wextra -Wconversion -DBUFFER_SIZE="${buf}" -O2 -o write_oh
	output="$(./write_oh 500)"
	echo "${output}" > "../data/overhead/write_${buf}.csv"
	echo "> File ../data/overhead/write_${buf}.csv written"
done
