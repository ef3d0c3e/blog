#define _GNU_SOURCE
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>

typedef struct
{
	double* buf;
	size_t size;
	size_t capacity;
} timing;

void
add_time(timing* t, struct timespec start, struct timespec end)
{
	time_t sec = end.tv_sec - start.tv_sec;
	long nsec = end.tv_nsec - start.tv_nsec;
	if (nsec < 0) {
		--sec;
		nsec += 1000000000L;
	}
	const double diff_us = (double)sec * 1e6 + (double)nsec * 1e-3;

	if (t->size >= t->capacity) {
		size_t new_cap = t->size * 2ull + !t->size * 16ull;
		t->buf = realloc(t->buf, new_cap * sizeof(double));
		t->capacity = new_cap;
	}
	t->buf[t->size++] = diff_us;
}

#ifndef BUFFER_SIZE
#define BUFFER_SIZE (1024 * 8)
#endif // BUFFER_SIZE

timing
write_overhead(const char* in, const char* out)
{
	int fdin = open(in, O_RDONLY);
	size_t size = (size_t)lseek(fdin, 0, SEEK_END);
	unsigned char* buf = malloc((size_t)size);
	read(fdin, buf, size);
	close(fdin);

	timing t = { 0, 0, 0 };
	int fd = open(out, O_RDWR | O_TRUNC | O_CREAT, 0666);
	struct timespec start, end;

	size_t pos = 0;
	while (1) {
		clock_gettime(CLOCK_MONOTONIC_RAW, &start);
		ssize_t r;
		if (pos + BUFFER_SIZE <= size)
			r = write(fd, buf + pos, BUFFER_SIZE);
		else
			r = write(fd, buf + pos, size - pos);
		clock_gettime(CLOCK_MONOTONIC_RAW, &end);
		if (r < 0) {
			perror("write");
			exit(1);
		} else if (r == 0)
			break;
		pos += (size_t)r;
		add_time(&t, start, end);
	}

	posix_fadvise(fd, 0, 0, POSIX_FADV_DONTNEED);
	close(fd);
	free(buf);
	return t;
}

int
main(int ac, char** av)
{
	const char* in = "in.bin";
	const char* out = "out.bin";

	if (ac != 2) {
		fprintf(stderr, "USAGE %s COUNT\n", av[0]);
		return (1);
	}

	size_t n_tests = (size_t)atoll(av[1]);
	if (n_tests <= 0)
	{
		fprintf(stderr, "Expected at least 1 test attempt\n");
		exit(1);
	}
	timing* tests = calloc(n_tests, sizeof(timing));

	for (size_t i = 0; i < n_tests; ++i) {
		tests[i] = write_overhead(in, out);
	}
	for (size_t j = 0; j < tests[0].size; ++j) {
		for (size_t i = 0; i < n_tests; ++i) {
			printf("%F,", tests[i].buf[j]);
		}
		printf("\n");
	}
	for (size_t i = 0; i < n_tests; ++i) {
		free(tests[i].buf);
	}
	free(tests);
	return 0;
}
