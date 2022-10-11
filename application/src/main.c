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
#include "tinysvcmdns.h"
#include "hairtunes.h"
#include "util.h"
#include "log_util.h"

extern int 		mdns_server(int argc, char *argv[]);
static int 		sock_printf(int sock,...);
static char*	sock_gets(int sock, char *str, int n);
static void 	print_usage(void);

const char *version = "0.205.2";

static unsigned short cport, tport, ipc_port;
static unsigned short port_base, port_range;
static int ipc_sock = -1;

static struct mdns_service* svc;
static struct mdnsd* svr;

log_level 			raop_loglevel = lINFO;
log_level			util_loglevel = lERROR;

static log_level 	*loglevel = &raop_loglevel;


/*----------------------------------------------------------------------------*/
static void die(char *why) {
	if (ipc_sock != -1) shutdown_socket(ipc_sock);
	sock_printf(ipc_sock, "FATAL: %s\n", why);
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
static char *sock_gets(int sock, char *str, int num)
{
	fd_set rfds;
	char *p = str;
	int n;

	FD_ZERO(&rfds);
	FD_SET(sock, &rfds);

	n = select(sock + 1, &rfds, NULL, NULL, NULL);
	if (n == -1) {
		LOG_DEBUG("gets EOF on select", NULL);
		return NULL;
	}

	do {
		n = recv(sock, p, 1, 0);
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
static int sock_printf(int sock, ...)
{
	va_list args, cp;
	char *p, *fmt;
	int n;

	va_start(args, sock);
	fmt = va_arg(args, char*);

#ifdef _WIN32
	n = vsnprintf(NULL, 0, fmt, args);
#else
	va_copy(cp, args);
	n = vsnprintf(NULL, 0, fmt, cp);
	va_end(cp);
#endif

	p = malloc(n + 1);
	vsprintf(p, fmt, args);

	n = send(sock, p, n, 0);

	free(p);

	return n;
}

/*----------------------------------------------------------------------------*/
static void hairtunes_cb(void *owner, hairtunes_event_t event)
{
	switch(event) {
		case HAIRTUNES_PLAY:
			sock_printf(ipc_sock, "play\n");
			break;
		default:
			LOG_ERROR("unknown hairtunes event %d", event);
			break;
	}
}

/*----------------------------------------------------------------------------*/
static void print_usage(void) {
	printf("shairport_helper version %s\n", version);
	printf("start with \"-mdns\" to use mDNS server mode\n"
		   "AIRPORT mode (default):\n"
		   " [iv <n> key <n>]\n"
		   " [host <ip>]\n"
		   " [socket <port>]\n"
		   " [fmtp <n>]\n"
		   " [cport <n>] [tport <n>]\n"
		   " [log <file>] [dbg <error|warn|info|debug|sdebug>]\n"
		   " [codec <mp3[:<rate>]|flc[:<level>]|wav|pcm>]\n"
		   " [sync]\n"
   		   " [drift]\n"
		   " [latency <airplay max ms hold[:http ms delay]>]\n"
		   " [ports <start[:<range|128>]>]\n"
		   "mDNS server mode:\n"
		   " [-o <ip>]\n"
		   " -i <identity>\n"
		   " -t <type>\n"
		   " -p <port>\n"
		   " [<txt>] ...[<txt>]\n"
	);
}

/*---------------------------------------------------------------------------*/
static void sighandler(int signum) {
	mdnsd_stop(svr);
#ifdef _WIN32
	winsock_close();
#endif
	exit(0);
}

/*----------------------------------------------------------------------------*/
int main(int argc, char **argv) {
	char aeskey[16] = "", aesiv[16] = "", *fmtp = NULL;
	char *arg, *logfile = NULL, *latencies = "";
	int ret = 0;
	bool use_sync = false, drift = false;
	static struct in_addr peer;
	encode_t encoder;

	// just print usage and exit
	if (argc <= 2) {
		print_usage();
		exit(0);
	}

#ifdef _WIN32
	winsock_init();
#endif

	// mDNS server mode
	if (!strcmp(argv[1], "-mdns")) {
		const char** txt = NULL;
		struct in_addr host;
		char hostname[256], * identity = NULL, * type = NULL;
		int port = 0;

		signal(SIGINT, sighandler);
		signal(SIGTERM, sighandler);
#if defined(SIGPIPE)
		signal(SIGPIPE, SIG_IGN);
#endif
#if defined(SIGQUIT)
		signal(SIGQUIT, sighandler);
#endif
#if defined(SIGHUP)
		signal(SIGHUP, sighandler);
#endif

		argv++;
		argc--;

		while ((arg = *++argv) != NULL) {
			if (!strcasecmp(arg, "-o") || !strcasecmp(arg, "host")) {
				host.s_addr = inet_addr(*++argv);
				argc -= 2;
			} else if (!strcasecmp(arg, "-p")) {
				port = atoi(*++argv);
			} else if (!strcasecmp(arg, "-t")) {
				(void)! asprintf(&type, "%s.local", *++argv);
			} else if (!strcasecmp(arg, "-i")) {
				identity = *++argv;
			} else {
				// nothing let's try to be smart and handle legacy crappy		
				if (!identity) identity = *argv;
				else if (!type) (void) !asprintf(&type, "%s.local", *argv);
				else if (!port) port = atoi(*argv);
				else {
					txt = (const char**) malloc((argc  + 1)* sizeof(char**));
					memcpy(txt, argv, argc * sizeof(char**));
					txt[argc] = NULL;
				}
				argc--;
			}
		}

		gethostname(hostname, sizeof(hostname));
		strcat(hostname, ".local");
		get_interface(&host);

		svr = mdnsd_start(host); 
		if (svr) {
			LOG_INFO("host: %s\nidentity: %s\ntype: %s\nip: %s\nport: %u\n", hostname, identity, type, inet_ntoa(host), port);

			mdnsd_set_hostname(svr, hostname, host);
			svc = mdnsd_register_svc(svr, identity, type, port, NULL, txt);
			mdns_service_destroy(svc);
#ifdef _WIN32
			Sleep(INFINITE);
#else
			pause();
#endif
			mdnsd_stop(svr);
		} else {
			LOG_ERROR("Can't start server");
			print_usage();
		}

		free(type);
		free(txt);
	} else {
		memset(&encoder, 0, sizeof(encode_t));
		encoder.codec = CODEC_FLAC;

		while ( (arg = *++argv) != NULL ) {
			if (!strcasecmp(arg, "iv")) {
				hex2bin(aesiv, *++argv);
			} else if (!strcasecmp(arg, "key")) {
				hex2bin(aeskey, *++argv);
			} else if (!strcasecmp(arg, "host")) {
				peer.s_addr = inet_addr(*++argv);
			} else if (!strcasecmp(arg, "fmtp")) {
				fmtp = *++argv;
			} else if (!strcasecmp(arg, "cport")) {
				cport = atoi(*++argv);
			} else if (!strcasecmp(arg, "tport")) {
				tport = atoi(*++argv);
			} else if (!strcasecmp(arg, "latencies")) {
				latencies = *++argv;
			} else if (!strcasecmp(arg, "ports")) {
				sscanf(*++argv, "%hu:%hu", &port_base, &port_range);
				if (!port_range) port_range = 32;
			} else if (!strcasecmp(arg, "socket")) {
				ipc_port = atoi(*++argv);
			} else if (!strcasecmp(arg, "log")) {
				logfile = *++argv;
			} else if (!strcasecmp(arg, "dbg")) {
				++argv;
				if (!strcmp(*argv, "error"))  *loglevel = util_loglevel = lERROR;
				if (!strcmp(*argv, "warn"))   *loglevel = util_loglevel = lWARN;
				if (!strcmp(*argv, "info"))   *loglevel = util_loglevel = lINFO;
				if (!strcmp(*argv, "debug"))  *loglevel = util_loglevel = lDEBUG;
				if (!strcmp(*argv, "sdebug")) *loglevel = util_loglevel = lSDEBUG;
			} else if (!strcasecmp(arg, "codec")) {
				++argv;
				if (!strcasecmp(*argv, "pcm")) encoder.codec = CODEC_PCM;
				else if (!strcasecmp(*argv, "wav")) encoder.codec = CODEC_WAV;
				else if (strcasestr(*argv, "mp3")) {
					encoder.codec = CODEC_MP3;
					if (strchr(*argv, ':')) encoder.mp3.bitrate = atoi(strchr(*argv, ':') + 1);
				} else {
					encoder.codec = CODEC_FLAC;
					if (strchr(*argv, ':')) encoder.flac.level = atoi(strchr(*argv, ':') + 1);
				}
			} else if (!strcasecmp(arg, "metadata")) {
				encoder.mp3.icy = atoi(*++argv);
			} else if (!strcasecmp(arg, "sync")) {
				use_sync = true;
			} else if (!strcasecmp(arg, "drift")) {
				drift = true;
			}
		}

		if (logfile && !freopen(logfile, "w", stderr)) die("cannot open logfile");

		LOG_INFO("client: %s, ipc port %hu", inet_ntoa(peer), ipc_port);
	
		ipc_sock = conn_socket(ipc_port);

		if (ipc_sock != -1) {
			hairtunes_resp_t ht;
			char line[128];
			int in_line = 0, n;
			struct in_addr host;

			host.s_addr = INADDR_ANY;
			ht = hairtunes_init(host, peer, encoder, use_sync, drift, false, latencies,
								aeskey, aesiv, fmtp, cport, tport, NULL,
								hairtunes_cb, NULL, port_base, port_range, -1);

			sock_printf(ipc_sock, "port: %d\n", ht.aport);
			sock_printf(ipc_sock, "cport: %d\n", ht.cport);
			sock_printf(ipc_sock, "tport: %d\n", ht.tport);
			sock_printf(ipc_sock, "hport: %d\n", ht.hport);
			sock_printf(ipc_sock, "shairport_helper: VERSION: %s\n", version);

			while (sock_gets(ipc_sock, line + in_line, sizeof(line) - in_line)) {
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
					unsigned short seqno;
					unsigned rtptime;

					sscanf(line, "flush %hu %u", &seqno, &rtptime);
					if (hairtunes_flush(ht.ctx, seqno, rtptime, false, false)) {
						sock_printf(ipc_sock, "flushed %hu %u\n", seqno, rtptime);
					}
				}

				if (strstr(line, "record")) {
					unsigned short seqno;
					unsigned rtptime;

					sscanf(line, "record %hu %u", &seqno, &rtptime);
					hairtunes_record(ht.ctx, seqno, rtptime);
				}
			}

			hairtunes_end(ht.ctx);

			sock_printf(ipc_sock, "bye!\n", NULL);
		} else {
			LOG_ERROR("Cannot start, check parameters", NULL);
			sock_printf(ipc_sock, "Cannot start, check parameters");
			ret = 1;
		}

		if (ipc_sock != -1) shutdown_socket(ipc_sock);
	}

#ifdef _WIN32
	winsock_close();
#endif

	return ret;
}


