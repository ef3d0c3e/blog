#!/usr/bin/env bash

sizes=("1K" "10K" "100K" "1M" "10M")
methods=(naive stdio buffered copy_file_range sendfile splice)

for m in "${methods[@]}"; do
	files=()
	headers=()
	for s in "${sizes[@]}"; do
		file=$(printf "data/%s_%s" "${m}" "${s}")
		if [[ ! -f "${file}" ]]; then continue; fi
		headers+=("$s")
		files+=("$file")
	done

    [[ ${#files[@]} -eq 0 ]] && continue
    header_line=$(IFS=, ; echo "${headers[*]}")
    outfile="data/${m}.csv"
    echo "$header_line" > "$outfile"
    paste -d, "${files[@]}" >> "$outfile"

    echo "> Created $outfile"
done
