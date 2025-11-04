// gcc copy.c -Wall -Wextra -Wconversion -DCOPY=<name> -O2
#define _GNU_SOURCE
#define _FILE_OFFSET_BITS 64
#include <bits/posix1_lim.h>
#include <fcntl.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/sendfile.h>
#include <time.h>
#include <unistd.h>
#include <sys/wait.h>

/** Validate both files */
int
validate(const char* in, const char* out)
{
	int fd_in = open(in, O_RDONLY);
	int fd_out = open(out, O_RDONLY);
	unsigned char buf_in[8192];
	unsigned char buf_out[8192];

	while (1) {
		ssize_t n_in = read(fd_in, buf_in, 8192);
		ssize_t n_out = read(fd_out, buf_out, 8192);

		if (n_out != n_in || n_out < 0) {
			close(fd_out);
			close(fd_in);
			return (0);
		}
		for (size_t i = 0; i < (size_t)n_out; ++i) {
			if (buf_in[i] != buf_out[i]) {
				close(fd_out);
				close(fd_in);
				return (0);
			}
		}
		if (!n_out && !n_in)
			break;
	}

	close(fd_out);
	close(fd_in);
	return (1);
}

/** Copy 1-byte at a time */
void
copy_posix(const char* in, const char* out)
{
	int fd_in = open(in, O_RDONLY);
	int fd_out = open(out, O_WRONLY | O_CREAT, 0666);

	unsigned char c;
	while (1) {
		if (read(fd_in, &c, 1) != 1)
			break;
		if (write(fd_out, &c, 1) != 1)
			break;
	}
	close(fd_out);
	close(fd_in);
}

/** Copy 1-byte at a time using stdio */
void
copy_stdio(const char* in, const char* out)
{
	FILE* fin = fopen(in, "rb");
	FILE* fout = fopen(out, "wb");

	unsigned char c;
	while (1) {
		if (fread(&c, 1, 1, fin) != 1)
			break;
		if (fwrite(&c, 1, 1, fout) != 1)
			break;
	}
	fclose(fout);
	fclose(fin);
}

/** Copy 1-byte at a time using stdio */
void
copy_stdio_lb(const char* in, const char* out)
{
	FILE* fin = fopen(in, "rb");
	FILE* fout = fopen(out, "wb");
	char inbuf[8192];
	setvbuf(fin, inbuf, _IOFBF, sizeof(inbuf));
	char outbuf[8192];
	setvbuf(fout, outbuf, _IOFBF, sizeof(outbuf));

	unsigned char c;
	while (1) {
		if (fread(&c, 1, 1, fin) != 1)
			break;
		if (fwrite(&c, 1, 1, fout) != 1)
			break;
	}
	fclose(fout);
	fclose(fin);
}

#ifndef BUFFER_SIZE
#define BUFFER_SIZE (1024 * 4)
#endif // BUFFER_SIZE

/** Copy buffered */
void
copy_posix_buffered(const char* in, const char* out)
{
	int fd_in = open(in, O_RDONLY);
	int fd_out = open(out, O_WRONLY | O_CREAT, 0666);
	unsigned char buf[BUFFER_SIZE];

	while (1) {
		ssize_t nread = read(fd_in, buf, BUFFER_SIZE);
		if (nread <= 0)
			break;
		size_t pos = 0;
		while (pos != (size_t)nread) {
			ssize_t nwrite = write(fd_out, buf + pos, (size_t)nread - pos);
			if (nwrite < 0)
				goto end;
			pos += (size_t)nwrite;
		}
	}
end:
	close(fd_out);
	close(fd_in);
}

/** Copy buffered using stdio */
void
copy_buffered_stdio(const char* in, const char* out)
{
	FILE* fin = fopen(in, "rb");
	FILE* fout = fopen(out, "wb");
	unsigned char buf[BUFFER_SIZE];

	unsigned char c;
	while (1) {
		if (fread(&c, 1, 1, fin) != 1)
			break;
		if (fwrite(&c, 1, 1, fout) != 1)
			break;
	}
	while (1) {
		unsigned long nread = fread(buf, BUFFER_SIZE, 1, fin);
		if (nread <= 0)
			break;
		size_t pos = 0;
		while (pos != (size_t)nread) {
			unsigned long nwrite = fwrite(buf, nread - pos, 1, fout);
			pos += (size_t)nwrite;
		}
	}
	fclose(fout);
	fclose(fin);
}

/** Copy with copy_file_range(2) */
void
copy_copy_file_range(const char* in, const char* out)
{
	int fd_in = open(in, O_RDONLY);
	int fd_out = open(out, O_WRONLY | O_CREAT, 0666);

	off_t offset = 0;
	while (1) {
		ssize_t n = copy_file_range(fd_in, &offset, fd_out, NULL, SSIZE_MAX, 0);
		if (n <= 0)
			break;
	}

	close(fd_out);
	close(fd_in);
}

/** Copy using sendfile(2) */
void
copy_sendfile(const char* in, const char* out)
{
	int fd_in = open(in, O_RDONLY);
	int fd_out = open(out, O_WRONLY | O_CREAT, 0666);

	off_t offset = 0;
	while (1) {
		ssize_t n = sendfile(fd_out, fd_in, &offset, SSIZE_MAX);
		if (n <= 0)
			break;
	}

	close(fd_out);
	close(fd_in);
}

#ifndef SPLICE_CHUNK
#define SPLICE_CHUNK (1024 * 1024)
#endif // SPLICE_CHUNK

void
copy_splice(const char* in, const char* out)
{
	int fd_in = open(in, O_RDONLY);
	int fd_out = open(out, O_WRONLY | O_CREAT, 0666);
	int pipefd[2];
	pipe(pipefd);

	while (1) {
		ssize_t nread =
		  splice(fd_in, NULL, pipefd[1], NULL, SPLICE_CHUNK, SPLICE_F_MORE | SPLICE_F_MOVE);
		if (nread <= 0)
			break;

		ssize_t remaining = nread;
		while (remaining > 0) {
			ssize_t nwrite = splice(
			  pipefd[0], NULL, fd_out, NULL, (size_t)remaining, SPLICE_F_MORE | SPLICE_F_MOVE);
			if (nwrite <= 0)
				goto end;
			remaining -= nwrite;
		}
	}
end:
	close(pipefd[0]);
	close(pipefd[1]);
	close(fd_out);
	close(fd_in);
}

/** Copy using `cp` */
void
copy_cp(const char *in, const char *out, struct timespec *start, struct timespec *end)
{
	char *cin = strdup(in);
	char *cout = strdup(out);
	int cpid = fork();

	if (!cpid) {
		execvp("cp", (char *const[]){"cp", cin, cout, NULL});
	}
	clock_gettime(CLOCK_MONOTONIC_RAW, start);
	waitpid(cpid, NULL, 0);
	clock_gettime(CLOCK_MONOTONIC_RAW, end);
	free(cin);
	free(cout);
}

/** Perform memcpy only */
void
copy_memcpy(const char *in, const char *out, struct timespec *start, struct timespec *end)
{
	(void)out;

	int fd = open(in, O_RDONLY);
	off_t size = lseek(fd, 0, SEEK_END);
	lseek(fd, 0, SEEK_SET);
	char *buf_in = malloc((size_t)size);
	size_t pos = 0;
	while (1)
	{
		ssize_t r = read(fd, buf_in + pos, 8192);
		if (r <= 0)
			break;
		pos += (size_t)r;
	}
	char *buf_out = malloc((size_t)size);

	clock_gettime(CLOCK_MONOTONIC_RAW, start);
	memcpy(buf_in, buf_out, (size_t)size);
	clock_gettime(CLOCK_MONOTONIC_RAW, end);
	free(buf_out);
	free(buf_in);
}

#define __CAT(x, y) x##y
#define CAT(x, y) __CAT(x, y)
#define COPY_FUN CAT(copy_, COPY)

int
double_cmp(const void* a, const void* b)
{
	const double* x = a;
	const double* y = b;

	if (*x > *y)
		return (1);
	else if (*x < *y)
		return (-1);
	return (0);
}

int
main(int ac, char** av)
{
	const char* in = "in.bin";
	const char* out = "out.bin";

	if (ac == 1) {
		fprintf(stderr, "USAGE %s COUNT [min|Minify output]\n", av[0]);
		return (1);
	}

	size_t n_tests = (size_t)atoll(av[1]);
	double* distrib = malloc(sizeof(double) * n_tests);

	for (size_t i = 0; i < n_tests; ++i) {
		// Remove 'out' file if it exists, since linux may optimize copying.
		remove(out);

		struct timespec start, end;
#define cp 1
#define memcpy 1
#if COPY==1
#undef cp
#undef memcpy
		COPY_FUN(in, out, &start, &end);
#else
#undef cp
#undef memcpy
		clock_gettime(CLOCK_MONOTONIC_RAW, &start);
		COPY_FUN(in, out);
		clock_gettime(CLOCK_MONOTONIC_RAW, &end);
#endif // COPY == 1
#ifndef NOCHECK
#define memcpy 1
#if COPY==0
#undef memcpy
		if (!validate(in, out)) {
			fprintf(stderr, "Invalid copy\n");
			free(distrib);
			return (1);
		}
#endif // COPPY == 0
#undef memcpy
		time_t sec = end.tv_sec - start.tv_sec;
		long nsec = end.tv_nsec - start.tv_nsec;
		if (nsec < 0) {
			--sec;
			nsec += 1000000000L;
		}
		const double diff_us = (double)sec * 1e6 + (double)nsec * 1e-3;
		if (ac <= 2)
			printf("%F\n", diff_us);
		distrib[i] = diff_us;
#endif // NOCHECK
	}
	if (ac >= 3 && !strcmp(av[2], "avg")) {
		double sum = 0.0;
		for (size_t i = 0; i < n_tests; ++i)
			sum += distrib[i];
		printf("%F\n", sum / (double)n_tests);
	}
	if (ac >= 3 && !strcmp(av[2], "box")) {
		qsort(distrib, n_tests, sizeof(double), double_cmp);
		double sum = 0.0;
		const size_t off_start = (size_t)((double)n_tests * 0.01);
		const size_t size = (size_t)((double)n_tests * 0.98);
		for (size_t i = off_start; i < size; ++i)
			sum += distrib[i];
		printf("%F,%F,%F,%F,%F,%F\n",
		       distrib[off_start],
		       distrib[off_start + size / 4],
		       distrib[off_start + size / 2],
		       distrib[off_start + (3 * size) / 4],
		       distrib[off_start + size - 1],
		       sum / (double)size);
	}
	free(distrib);
	return 0;
}
