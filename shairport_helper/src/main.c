/*
 * HairTunes - RAOP packet handler and slave-clocked replay engine
 * Copyright (c) James Laird 2011, philippe_44 2017
 * All rights reserved.
 *
 * Modularisation: philippe_44@outlook.com, 2017
 *
 * Permission is hereby granted, free of charge, to any person
 * obtaining a copy of this software and associated documentation
 * files (the "Software"), to deal in the Software without
 * restriction, including without limitation the rights to use,
 * copy, modify, merge, publish, distribute, sublicense, and/or
 * sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
 * OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
 * WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
 * OTHER DEALINGS IN THE SOFTWARE.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <sys/types.h>
#include <pthread.h>
#include <openssl/aes.h>
#include <math.h>
#include <errno.h>
#include <sys/stat.h>
#include <stdint.h>
#include <fcntl.h>
#include <assert.h>

#include "platform.h"
#include "hairtunes.h"
#include "util.h"
#include "log_util.h"


#define _printf(...) _fprintf_(stdout, ##__VA_ARGS__)

extern int 		mdns_server(int argc, char *argv[]);
static int 		_fprintf_(FILE *file, ...);
static int 		_fflush_(FILE *file);
static char*	_fgets(char *str, int n, FILE *file);
static void 	print_usage(int argc, char **argv);

const char *version = "0.74.0";

short unsigned cport = 0, tport = 0, in_port, out_port, err_port;
static	int in_sd = -1, out_sd = -1, err_sd = -1;

log_level 			raop_loglevel = lERROR;
log_level			util_loglevel = lERROR;

static log_level 	*loglevel = &raop_loglevel;


/*----------------------------------------------------------------------------*/
static void die(char *why) {
	if (in_sd != -1) close_socket(in_sd);
	if (out_sd != -1) close_socket(out_sd);
	if (err_sd != -1) close_socket(err_sd);
	_fprintf_(stderr, "FATAL: %s\n", why);
	LOG_ERROR("FATAL ERROR: %s", why);
	exit(1);
}


/*----------------------------------------------------------------------------*/
static int hex2bin(char *buf, char *hex) {
	int i;
	int j;
	if (strlen(hex) != 0x20)
		return 1;
	for (i=0; i < 0x10; i++) {
		if (!sscanf(hex, "%2X", &j))
		   return 1;
		hex += 2;
		*buf++ = j;
	}
	return 0;
}


/*----------------------------------------------------------------------------*/
static char *_fgets(char *str, int num, FILE *file)
{
	fd_set rfds;
	char *p = str;
	int n;

	if (file == stdin && in_sd == -1) return fgets(str, num, file);

	FD_ZERO(&rfds);
	FD_SET(in_sd, &rfds);

	n = select(in_sd + 1, &rfds, NULL, NULL, NULL);
	if (n == -1) {
		LOG_DEBUG("gets EOF on select", NULL);
		return NULL;
	}

	do {
		n = recv(in_sd, p, 1, 0);
		if (n == 1) p++;
		else break;
	}  while (p-str < num && *(p-1) != '\n');

	*p = '\0';

	if (p == str) {
		LOG_DEBUG("gets EOF on closed socket", NULL);
		return NULL;
	}

	return str;
}


/*----------------------------------------------------------------------------*/
static int _fprintf_(FILE *file, ...)
{
	va_list args, cp;
	char *p, *fmt;
	int n;

	va_start(args, file);
	fmt = va_arg(args, char*);

	if ((file == stdout && out_sd == -1) || (file == stderr && err_sd == -1)) {
		int n = vfprintf(file, fmt, args);
		va_end(args);
		return n;
	}

#ifdef WIN32
	n = vsnprintf(NULL, 0, fmt, args);
#else
	va_copy(cp, args);
	n = vsnprintf(NULL, 0, fmt, cp);
	va_end(cp);
#endif

	p = malloc(n + 1);
	vsprintf(p, fmt, args);

	n = send(file == stdout ? out_sd : err_sd, p, n, 0);

	free(p);

	return n;
}

/*----------------------------------------------------------------------------*/
static int _fflush_(FILE *file)
{
	if (!out_port || !err_port) return fflush(file);

	return 0;
}


/*----------------------------------------------------------------------------*/
static void hairtunes_cb(void *owner, hairtunes_event_t event)
{
	switch(event) {
		case HAIRTUNES_PLAY:
			_printf("play\n");
			break;
		default:
			LOG_ERROR("unknown hairtunes event %d", event);
			break;
	}
}


/*----------------------------------------------------------------------------*/
static void print_usage(int argc, char **argv) {
	mdns_server(argc, argv);
	printf("AIRPORT mode:\n\tiv <n> key <n> \n"
		   "[host <ip>]\n"
		   "[socket <in>,<out>,<err>]\n"
		   "[fmtp <n>]\n"
		   "[cport <n>] [tport <n>]\n"
		   "[log <file>] [dbg <error|warn|info|debug|sdebug>]\n"
		   "[flac]\n"
		   "[sync]\n"
		   "[latency <airplay max ms hold[:http ms delay]>]\n"
		   );
}


/*----------------------------------------------------------------------------*/
int main(int argc, char **argv) {
	char aeskey[16], aesiv[16], *fmtp = NULL;
	char *arg, *logfile = NULL, *latencies = "";
	int ret = 0;
	bool use_flac = false, use_sync = false;
	static struct in_addr host_addr;

	assert(RAND_MAX >= 0x7fff);    // XXX move this to compile time

	if (argc > 1 && (!strcmp(argv[1], "-h") || !strcmp(argv[1], "-dns"))) {
		if (!strcmp(argv[1], "-h")) {
			print_usage(argc, argv);
		} else mdns_server(argc - 1, argv + 1);
		exit (0);
	}

	while ( (arg = *++argv) != NULL ) {
		if (!strcasecmp(arg, "iv")) {
			hex2bin(aesiv, *++argv);
		} else
		if (!strcasecmp(arg, "key")) {
			hex2bin(aeskey, *++argv);
		} else
		if (!strcasecmp(arg, "host")) {
			host_addr.s_addr = inet_addr(*++argv);
		} else
		if (!strcasecmp(arg, "fmtp")) {
			fmtp = *++argv;
		} else
		if (!strcasecmp(arg, "cport")) {
			cport = atoi(*++argv);
		} else
		if (!strcasecmp(arg, "tport")) {
			tport = atoi(*++argv);
		} else
		if (!strcasecmp(arg, "latencies")) {
			latencies = *++argv;
		} else

		if (!strcasecmp(arg, "socket")) {
			char *ports = *++argv;
			sscanf(ports, "%hu,%hu,%hu", &in_port, &out_port, &err_port);
		}
		if (!strcasecmp(arg, "log")) {
			logfile = *++argv;
		}
		if (!strcasecmp(arg, "dbg")) {
			++argv;
			if (!strcmp(*argv, "error"))  *loglevel = lERROR;
			if (!strcmp(*argv, "warn"))   *loglevel = lWARN;
			if (!strcmp(*argv, "info"))   *loglevel = lINFO;
			if (!strcmp(*argv, "debug"))  *loglevel = lDEBUG;
			if (!strcmp(*argv, "sdebug")) *loglevel = lSDEBUG;
		}
		if (!strcasecmp(arg, "flac")) {
			use_flac = true;
		}
		if (!strcasecmp(arg, "sync")) {
			use_sync = true;
		}
	}

	if (logfile && !freopen(logfile, "w", stderr))
		die("cannot open logfile");

	LOG_INFO("client: %s", inet_ntoa(host_addr));

#ifdef WIN32
	winsock_init();
#endif

	if (in_port) in_sd = conn_socket(in_port);
	if (out_port) out_sd = conn_socket(out_port);
	if (err_port) err_sd = conn_socket(err_port);

	_fflush_(stdout);

	if (((in_port || out_port || err_port) && (in_sd * out_sd * err_sd > 0)) ||
		(!in_port && !out_port && !err_port)) {
		hairtunes_resp_t ht;
		char line[128];
		int in_line = 0, n;

		ht = hairtunes_init(host_addr, use_flac, use_sync, latencies, aeskey, aesiv,
							fmtp, cport, tport, NULL, hairtunes_cb);

		_printf("port: %d\n", ht.aport);
		_printf("cport: %d\n", ht.cport);
		_printf("tport: %d\n", ht.tport);
		_printf("hport: %d\n", ht.hport);

		_fprintf_(stderr, "shairport_helper: VERSION: %s\n", version);

		while (_fgets(line + in_line, sizeof(line) - in_line, stdin)) {
			n = strlen(line);

			if (line[n-1] != '\n') {
				in_line = strlen(line) - 1;
				if (n == sizeof(line)-1) in_line = 0;
				continue;
			}

			if (!strcmp(line, "exit\n")) {
				break;
			}

			if (strstr(line, "flush")) {
				unsigned short flush_seqno;

				sscanf(line, "flush %hu", &flush_seqno);
				if (hairtunes_flush(ht.ctx, flush_seqno, 0)) {
					_printf("flushed %hu\n", flush_seqno);
				}
			}
		}

		hairtunes_end(ht.ctx);

		_fprintf_(stderr, "bye!\n", NULL);
		_fflush_(stderr);

	} else {
		LOG_ERROR("Cannot start, check parameters", NULL);
		_fprintf_(stderr, "Cannot start, check parameters");
		ret = 1;
	}

	if (in_sd != -1) close_socket(in_sd);
	if (out_sd != -1) close_socket(out_sd);
	if (err_sd != -1) close_socket(err_sd);

#ifdef WIN32
	winsock_close();
#endif

	return ret;
}


