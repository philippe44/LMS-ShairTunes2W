/*
 * HairTunes - RAOP packet handler and slave-clocked replay engine
 * Copyright (c) James Laird 2011
 * (c) Philippe, philippe_44@outlook.com
 *
 * See LICENSE file
 *
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
#include "mdnssvc.h"
#include "raop_streamer.h"
#include "cross_net.h"
#include "cross_util.h"
#include "cross_log.h"
#include "cross_ssl.h"

extern int 		mdns_server(int argc, char *argv[]);
static int 		sock_printf(int sock,...);
static char*	sock_gets(int sock, char *str, int n);
static void 	print_usage(void);

const char *version = "1.9.5";

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
static void streamer_cb(void *owner, raopst_event_t event)
{
	switch(event) {
		case RAOP_STREAMER_PLAY:
			sock_printf(ipc_sock, "play\n");
			break;
		default:
			LOG_ERROR("unknown event %d", event);
			break;
	}
}

/*----------------------------------------------------------------------------*/
static void print_usage(void) {
	printf("shairport_helper version %s\n", version);
	printf("start with \"-mdns\" to use mDNS server mode\n"
		   "AIRPORT mode (default):\n"
		   " [iv <n> key <n>]\n"
		   " [peer <ip>]\n"
		   " [socket <port>]\n"
		   " [fmtp <n>]\n"
		   " [cport <n>] [tport <n>]\n"
		   " [log <file>] [dbg <error|warn|info|debug|sdebug>]\n"
		   " [codec <mp3[:<rate>]|aac[:<rate>]|flc[:<level>]|wav|pcm>]\n"
		   " [sync]\n"
   		   " [drift]\n"
		   " [latency <airplay max ms hold[:http ms delay]>]\n"
		   " [ports <start[:<range|128>]>]\n"
		   "mDNS server mode:\n"
		   " [-o <ip[iface>]\n"
		   " -i <identity>\n"
		   " -t <type>\n"
		   " -p <port>\n"
		   " [<txt>] ...[<txt>]\n"
	);
}

/*---------------------------------------------------------------------------*/
static void sighandler(int signum) {
	mdnsd_stop(svr);
	netsock_close();
	exit(0);
}

/*----------------------------------------------------------------------------*/
int main(int argc, char **argv) {
	char aeskey[16] = "", aesiv[16] = "", *fmtp = NULL;
	char *arg, *logfile = NULL, *latencies = "";
	int ret = 0;
	bool use_sync = false, drift = false;
	static struct in_addr peer;
	char* codec = "";
	bool metadata = false;

	// just print usage and exit
	if (argc <= 2) {
		print_usage();
		exit(0);
	}

	netsock_init();

	// mDNS server mode
	if (!strcmp(argv[1], "-mdns")) {
		const char** txt = NULL;
		struct in_addr host;
		char hostname[256], * identity = NULL, * type = NULL, *iface = NULL;
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
				iface = *++argv;
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
					break;
				}
				argc--;
			}
		}

		gethostname(hostname, sizeof(hostname));
		strcat(hostname, ".local");

		host = get_interface(iface, NULL, NULL);
		svr = mdnsd_start(host, false); 

		if (svr) {
			char txt_info[1024];
			for (int i = 0, n = 0; txt[i]; i++) n += snprintf(txt_info + n, sizeof(txt_info) - n, "\t%s\n", txt[i]);
			LOG_INFO("host: %s\nidentity: %s\ntype: %s\nip: %s\nport: %u\nwith TXT:\n%s", hostname, identity, type, inet_ntoa(host), port, txt_info);

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

		if (!cross_ssl_load()) {
			LOG_ERROR("Cannot load SSL libraries", NULL);
			return false;
		}

		while ( (arg = *++argv) != NULL ) {
			if (!strcasecmp(arg, "iv")) {
				hex2bin(aesiv, *++argv);
			} else if (!strcasecmp(arg, "key")) {
				hex2bin(aeskey, *++argv);
			} else if (!strcasecmp(arg, "peer")) {
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
				codec = *argv;
			} else if (!strcasecmp(arg, "metadata")) {
				metadata = atoi(*++argv);
			} else if (!strcasecmp(arg, "sync")) {
				use_sync = true;
			} else if (!strcasecmp(arg, "drift")) {
				drift = true;
			}
		}

		if (logfile && !freopen(logfile, "w", stderr)) die("cannot open logfile");

		LOG_INFO("client: %s, ipc port %hu", inet_ntoa(peer), ipc_port);
	
		ipc_sock = tcp_connect_loopback(ipc_port);

		if (ipc_sock != -1) {
			raopst_resp_t ht;
			char line[128];
			int in_line = 0, n;
			struct in_addr host;

			host.s_addr = INADDR_ANY;
			ht = raopst_init(host, peer, codec, metadata, use_sync, drift, latencies,
								aeskey, aesiv, fmtp, cport, tport, NULL,
								streamer_cb, NULL, port_base, port_range, -1);

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
					if (raopst_flush(ht.ctx, seqno, rtptime, false, false)) {
						sock_printf(ipc_sock, "flushed %hu %u\n", seqno, rtptime);
					}
				}

				if (strstr(line, "record")) {
					unsigned short seqno;
					unsigned rtptime;

					sscanf(line, "record %hu %u", &seqno, &rtptime);
					raopst_record(ht.ctx, seqno, rtptime);
				}
			}

			raopst_end(ht.ctx);

			sock_printf(ipc_sock, "bye!\n", NULL);
		} else {
			LOG_ERROR("Cannot start, check parameters", NULL);
			sock_printf(ipc_sock, "Cannot start, check parameters");
			ret = 1;
		}

		if (ipc_sock != -1) shutdown_socket(ipc_sock);

		cross_ssl_free();
	}

	netsock_close();

	return ret;
}


