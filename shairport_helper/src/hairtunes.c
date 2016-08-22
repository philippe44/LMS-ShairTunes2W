/*
 * HairTunes - RAOP packet handler and slave-clocked replay engine
 * Copyright (c) James Laird 2011
 * All rights reserved.
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

#ifdef _WIN32
#include <winsock2.h>
#include <in6addr.h>
#include <ws2tcpip.h>
#define close closesocket
#define strcasecmp stricmp
char* strsep(char** stringp, const char* delim);
#define errno WSAGetLastError()
#else
#include <unistd.h>
#include <sys/select.h>
#include <sys/socket.h>
#include <netdb.h>
#include <arpa/inet.h>
#include <netinet/in.h>
#include <sys/time.h>
#include <sys/signal.h>
#endif

#include "hairtunes.h"


#ifdef FANCY_RESAMPLING
#include <samplerate.h>
#endif

#include <assert.h>
static int debug = 0;

#include "alac.h"

#include "http.h"

int _fprintf(FILE *file, ...);
#define _printf(...) _fprintf(stdout, ##__VA_ARGS__)
int _fflush(FILE *file);
char *_fgets(char *str, int n, FILE *file);
int in_port, out_port, err_port;
int in_sd = -1, out_sd = -1, err_sd = -1;
int create_socket(int port);
int close_socket(int sd);

const char *version = "0.3";

// default buffer size
#define START_FILL    64
#define MIN_FILL      32
#define BUFFER_FRAMES 1024
#define MAX_PACKET    2048

typedef unsigned short seq_t;

// global options (constant after init)
static unsigned char aeskey[16], aesiv[16];
static AES_KEY aes;
static char *rtphost = 0;
static int controlport = 0;
static int timingport = 0;
static int fmtp[32];
static int sampling_rate;
static int frame_size;

static int http_listener = -1;
static int http_status = 0;
static int http_connection = -1;
static char http_buffer[4096];

static int http_on_headers_complete(http_parser* parser);

  http_cb      on_message_begin;
  http_data_cb on_url;
  http_cb      on_status_complete;
  http_data_cb on_header_field;
  http_data_cb on_header_value;
  http_cb      on_headers_complete;
  http_data_cb on_body;
  http_cb      on_message_complete;

http_parser_settings http_settings = { NULL, NULL, NULL, NULL, NULL,
									   http_on_headers_complete,
									   NULL, NULL,
};

http_parser *http_parser_ctx = NULL;


static int buffer_start_fill;

#define FRAME_BYTES (4*frame_size)
// maximal resampling shift - conservative
#define OUTFRAME_BYTES (4*(frame_size+3))


static alac_file *decoder_info;

#ifdef FANCY_RESAMPLING
static int fancy_resampling = 1;
static SRC_STATE *src;
#endif

static int  init_rtp(void);
static int  init_http(void);
static void init_buffer(void);
static int  init_output(void);
static void rtp_request_resend(seq_t first, seq_t last);
static void ab_resync(void);

// interthread variables
// stdin->decoder
static double volume = 1.0;
static int fix_volume = 0x10000;
static pthread_mutex_t vol_mutex = PTHREAD_MUTEX_INITIALIZER;



typedef struct audio_buffer_entry {   // decoded audio packets
    int ready;
	signed short *data;
} abuf_t;
static abuf_t audio_buffer[BUFFER_FRAMES];
#define BUFIDX(seqno) ((seq_t)(seqno) % BUFFER_FRAMES)

// mutex-protected variables
static seq_t ab_read; 
static seq_t ab_write;
static int ab_buffering = 1;
static int ab_synced = 0;
static pthread_mutex_t ab_mutex = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t ab_buffer_ready = PTHREAD_COND_INITIALIZER;

static void die(char *why) {
	if (in_sd != -1) close_socket(in_sd);
	if (out_sd != -1) close_socket(out_sd);
	if (err_sd != -1) close_socket(err_sd);
	_fprintf(stderr, "FATAL: %s\n", why);
	exit(1);
}

#ifdef HAIRTUNES_STANDALONE
static int hex2bin(unsigned char *buf, char *hex) {
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
#endif

static int init_decoder(void) {
	alac_file *alac;
	int sample_size = fmtp[3];

	frame_size = fmtp[1]; // stereo samples
	sampling_rate = fmtp[11];

	if (sample_size != 16)
		die("only 16-bit samples supported!");

	alac = create_alac(sample_size, 2);
	if (!alac)
		return 1;
    decoder_info = alac;

    alac->setinfo_max_samples_per_frame = frame_size;
    alac->setinfo_7a =      fmtp[2];
    alac->setinfo_sample_size = sample_size;
    alac->setinfo_rice_historymult = fmtp[4];
    alac->setinfo_rice_initialhistory = fmtp[5];
    alac->setinfo_rice_kmodifier = fmtp[6];
	alac->setinfo_7f =      fmtp[7];
	alac->setinfo_80 =      fmtp[8];
	alac->setinfo_82 =      fmtp[9];
	alac->setinfo_86 =      fmtp[10];
	alac->setinfo_8a_rate = fmtp[11];
	allocate_buffers(alac);
	return 0;
}

int hairtunes_init(char *pAeskey, char *pAesiv, char *fmtpstr, int pCtrlPort, int pTimingPort,
		 char *pRtpHost, int bufStartFill)
{
	int i = 0;
	char *arg;
	char line[128];
	int in_line = 0;
	int n;
	double f;


	if(pAeskey != NULL)
		memcpy(aeskey, pAeskey, sizeof(aeskey));
	if(pAesiv != NULL)
		memcpy(aesiv, pAesiv, sizeof(aesiv));
	if(pRtpHost != NULL)
		rtphost = pRtpHost;

	controlport = pCtrlPort;
	timingport = pTimingPort;
	if(bufStartFill < 0)
		bufStartFill = START_FILL;
	buffer_start_fill = bufStartFill;

	AES_set_decrypt_key(aeskey, 128, &aes);

	memset(fmtp, 0, sizeof(fmtp));

	while ( (arg = strsep(&fmtpstr, " \t")) )
		fmtp[i++] = atoi(arg);

	init_decoder();
	init_buffer();
	init_rtp();      // open a UDP listen port and start a listener; decode into ring buffer
	init_http();
	_fflush(stdout);
	init_output();              // resample and output from ring buffer

	while (_fgets(line + in_line, sizeof(line) - in_line, stdin)) {
		n = strlen(line);
		if (line[n-1] != '\n') {
			in_line = strlen(line) - 1;
			if (n == sizeof(line)-1)
				in_line = 0;
			continue;
		}
		if (sscanf(line, "vol: %lf\n", &f)) {
			assert(f<=0);
			if (debug)
				_fprintf(stderr, "VOL: %lf\n", f);
			pthread_mutex_lock(&vol_mutex);
			volume = pow(10.0,0.05*f);
			fix_volume = 65536.0 * volume;
			pthread_mutex_unlock(&vol_mutex);
			continue;
		}
		if (!strcmp(line, "exit\n")) {
			return (0);
		}
		if (!strcmp(line, "flush\n")) {
			pthread_mutex_lock(&ab_mutex);
			ab_resync();
			pthread_mutex_unlock(&ab_mutex);
			if (debug)
				_fprintf(stderr, "FLUSH\n", NULL);
		}
	}
	_fprintf(stderr, "bye!\n", NULL);
	_fflush(stderr);

	return EXIT_SUCCESS;
}

#ifdef AF_INET6
static int ipv4_only = 0;
#else
static int ipv4_only = 1;
#endif

#ifdef MDNS_SVC
extern int mdns_server(int argc, char *argv[]);
#endif

#ifdef WIN32
static void winsock_init(void) {
	WSADATA wsaData;
	WORD wVersionRequested = MAKEWORD(2, 2);
	int WSerr = WSAStartup(wVersionRequested, &wsaData);
	if (WSerr != 0) exit(1);
}

static void winsock_close(void) {
	WSACleanup();
}
#endif

#ifdef HAIRTUNES_STANDALONE
int main(int argc, char **argv) {
	char *hexaeskey = 0;
	char *hexaesiv = 0;
	char *fmtpstr = 0;
	char *arg;
	char *logfile = NULL;
	int ret;
	assert(RAND_MAX >= 0x7fff);    // XXX move this to compile time

#ifdef MDNS_SVC
	if (argc > 1 && (!strcmp(argv[1], "-h") || !strcmp(argv[1], "-dns"))) {
		if (!strcmp(argv[1], "-h")) {
			printf("MDNS mode:\n\t-dns ");
			mdns_server(argc, argv);
#ifdef FANCY_RESAMPLING
			printf("AIRPORT mode:\n\tiv <n> key <n> [socket <in>,<out>,<err>] "
				   "[ipv4_only] [fmtp <n>] [cport <n>] "
				   "[tport <n>] [host <n>] "
				   "[resamp] [log <file>]"
				   "\n");
#else
			printf("AIRPORT mode:\n\tiv <n> key <n> [socket <in>,<out>,<err>] "
				   "[ipv4_only] [fmtp <n>] [cport <n>] "
				   "[tport <n>] [host <n>] "
				   "[log <file>]"
				   "\n");
#endif
		}
		else mdns_server(argc - 1, argv + 1);
		exit (0);
	}
#endif

	while ( (arg = *++argv) ) {
		if (!strcasecmp(arg, "iv")) {
			hexaesiv = *++argv;
			argc--;
		} else
		if (!strcasecmp(arg, "ipv4_only")) {
			ipv4_only = 1;
		} else
		if (!strcasecmp(arg, "key")) {
			hexaeskey = *++argv;
			argc--;
		} else
		if (!strcasecmp(arg, "fmtp")) {
			fmtpstr = *++argv;
		} else
		if (!strcasecmp(arg, "cport")) {
			controlport = atoi(*++argv);
		} else
		if (!strcasecmp(arg, "tport")) {
			timingport = atoi(*++argv);
		} else
		if (!strcasecmp(arg, "host")) {
			rtphost = *++argv;
		} else
		if (!strcasecmp(arg, "socket")) {
			char *ports = *++argv;
			sscanf(ports, "%d,%d,%d", &in_port, &out_port, &err_port);
		}
		if (!strcasecmp(arg, "log")) {
			logfile = *++argv;
			argc--;
		}
#ifdef FANCY_RESAMPLING
		else if (!strcasecmp(arg, "resamp")) {
			fancy_resampling = atoi(*++argv);
		}
#endif
	}

	if (logfile && !freopen(logfile, "w", stderr))
		die("cannot open logfile");

	if (!hexaeskey || !hexaesiv)
		die("Must supply AES key and IV!");
	if (hex2bin(aesiv, hexaesiv))
		die("can't understand IV");
	if (hex2bin(aeskey, hexaeskey))
		die("can't understand key");

#ifdef WIN32
	winsock_init();
#endif

	if (in_port) in_sd = create_socket(in_port);
	if (out_port) out_sd = create_socket(out_port);
	if (err_port) err_sd = create_socket(err_port);

	if (((in_port || out_port || err_port) && (in_sd * out_sd * err_sd > 0)) ||
		(!in_port && !out_port && !err_port)) {
		ret = hairtunes_init(NULL, NULL, fmtpstr, controlport, timingport, NULL, START_FILL);
	}
	else {
		_fprintf(stderr, "Cannot start, check parameters");
	}

	if (in_sd != -1) close_socket(in_sd);
	if (out_sd != -1) close_socket(out_sd);
	if (err_sd != -1) close_socket(err_sd);

#ifdef WIN32
	winsock_close();
#endif

	return ret;
}
#endif

static void init_buffer(void) {
	int i;
	for (i = 0; i < BUFFER_FRAMES; i++)
		audio_buffer[i].data = malloc(OUTFRAME_BYTES);
	ab_resync();
}

static void ab_resync(void) {
	int i;
	for (i = 0; i < BUFFER_FRAMES; i++)
		audio_buffer[i].ready = 0;
	ab_synced = 0;
	ab_buffering = 1;
}

// the sequence numbers will wrap pretty often.
// this returns true if the second arg is after the first
static inline int seq_order(seq_t a, seq_t b) {
	signed short d = b - a;
	return d > 0;
}

static void alac_decode(short *dest, char *buf, int len) {
	unsigned char packet[MAX_PACKET];
	unsigned char iv[16];
	int aeslen;
	int outsize;
	assert(len<=MAX_PACKET);

	aeslen = len & ~0xf;
	memcpy(iv, aesiv, sizeof(iv));
	AES_cbc_encrypt((unsigned char*)buf, packet, aeslen, &aes, iv, AES_DECRYPT);
	memcpy(packet+aeslen, buf+aeslen, len-aeslen);

	decode_frame(decoder_info, packet, dest, &outsize);

	assert(outsize == FRAME_BYTES);
}

static void buffer_put_packet(seq_t seqno, char *data, int len) {
	abuf_t *abuf = 0;
	short buf_fill;

	pthread_mutex_lock(&ab_mutex);
	if (!ab_synced) {
		ab_write = seqno;
		ab_read = seqno-1;
		ab_synced = 1;
	}
	if(debug)
		_fprintf(stderr, "buffer_put_packet: [%i]\n", seqno);

	if (seqno == (seq_t)(ab_write+1)) {                  // expected packet
		abuf = audio_buffer + BUFIDX(seqno);
		ab_write = seqno;
	} else if (seq_order(ab_write, seqno)) {    // newer than expected
		rtp_request_resend(ab_write+1, seqno-1);
		abuf = audio_buffer + BUFIDX(seqno);
		ab_write = seqno;
	} else if (seq_order(ab_read, seqno)) {     // late but not yet played
		abuf = audio_buffer + BUFIDX(seqno);
		if (debug)
			_fprintf(stderr, "buffer_put_packet: recovered packet %04X (%04X:%04X)\n", seqno, ab_read, ab_write);
	} else {    // too late.
		_fprintf(stderr, "buffer_put_packet: too late packet %04X (%04X:%04X)\n", seqno, ab_read, ab_write);
	}
	buf_fill = ab_write - ab_read;
	pthread_mutex_unlock(&ab_mutex);

	if (abuf) {
		alac_decode(abuf->data, data, len);
		abuf->ready = 1;
    }

    pthread_mutex_lock(&ab_mutex);
    if (ab_buffering && buf_fill >= buffer_start_fill) {
        ab_buffering = 0;
        pthread_cond_broadcast(&ab_buffer_ready);
	}
    pthread_mutex_unlock(&ab_mutex);
}

static int rtp_sockets[2];  // data, control

static struct sockaddr_storage rtp_client;
static socklen_t rtp_client_len;

static void *rtp_thread_func(void *arg) {
    char packet[MAX_PACKET];
	char *pktp;
    seq_t seqno;
    ssize_t plen;
    int sock = rtp_sockets[0]; 
    int csock = rtp_sockets[1];
    int readsock;
	char type;
	fd_set fds;

	FD_ZERO(&fds);
	FD_SET(sock, &fds);
	FD_SET(csock, &fds);

	while (select(csock>sock ? csock+1 : sock+1, &fds, 0, 0, 0)!=-1) {
		if (FD_ISSET(sock, &fds)) {
			readsock = sock;
		} else {
			readsock = csock;
		}
		FD_SET(sock, &fds);
		FD_SET(csock, &fds);

		rtp_client_len = sizeof(struct sockaddr_storage);
		plen = recvfrom(readsock, packet, sizeof(packet), 0, (struct sockaddr*) &rtp_client, &rtp_client_len);
		if (plen < 0)
			continue;
		assert(plen<=MAX_PACKET);

		type = packet[1] & ~0x80;
		if(debug)
			_fprintf(stderr, "rtp_thread_func: {%02X}\n", type);
		_fflush(stderr);
		if (type == 0x60 || type == 0x56) {   // audio data / resend
			pktp = packet;
			if (type==0x56) {
				pktp += 4;
				plen -= 4;
			}
			seqno = ntohs(*(unsigned short *)(pktp+2));

			// adjust pointer and length
			pktp += 12;
			plen -= 12;

			// check if packet contains enough content to be reasonable
			if (plen >= 16) {
				buffer_put_packet(seqno, pktp, plen);
			} else {
				// resync?
				if (type == 0x56 && seqno == 0) {
					_fprintf(stderr, "rtp_thread_func: Suspected resync request packet received. Initiating resync.\n");
					pthread_mutex_lock(&ab_mutex);
					ab_resync();
					pthread_mutex_unlock(&ab_mutex);
				}
			}
		}
		// 1st sync packet received (signals a restart of playback)
		else if (type == 0x54 && (packet[0] & 0x10)) {
			_printf("play\n");
			_fprintf(stderr, "1st frame play received\n");
		}
	}

	return 0;
}

static void rtp_request_resend(seq_t first, seq_t last) {
	unsigned char req[8];    // *not* a standard RTCP NACK

	if (seq_order(last, first))
		return;

	if (debug)
		_fprintf(stderr, "\nRR[W:%i R:%i first=%i last=%i\n", ab_write, ab_read, first, last);

	req[0] = 0x80;
	req[1] = 0x55|0x80;  // Apple 'resend'
    *(unsigned short *)(req+2) = htons(1);  // our seqnum
    *(unsigned short *)(req+4) = htons(first);  // missed seqnum
	*(unsigned short *)(req+6) = htons(last-first+1);  // count

    if (ipv4_only) {
	((struct sockaddr_in *)&rtp_client)->sin_port = htons(controlport);
	}
#ifdef AF_INET6
	else {
	((struct sockaddr_in6 *)&rtp_client)->sin6_port = htons(controlport);
	}
#endif

	if (sizeof(req) != sendto(rtp_sockets[1], req, sizeof(req), 0, (struct sockaddr*) &rtp_client, sizeof(struct sockaddr_storage))) {
		_fprintf(stderr, "rtp_request_resend: SENDTO failed (%s)\n", strerror(errno));
	}
}


static int init_rtp(void) {
	struct sockaddr_in si;
	int type = AF_INET;
	struct sockaddr* si_p = (struct sockaddr*)&si;
	socklen_t si_len = sizeof(si);
	unsigned short *sin_port = &si.sin_port;
	int sock = -1;
	int csock = -1;    // data and control (we treat the streams the same here)
	int tsock = -1;	   // need 3, even if we don't handle timing port
	unsigned short port = 6003;
	pthread_t rtp_thread;

#ifdef AF_INET6
	struct sockaddr_in6 si6;

	if (!ipv4_only) {
		type = AF_INET6;
		si_p = (struct sockaddr*)&si6;
		si_len = sizeof(si6);
		sin_port = &si6.sin6_port;
		memset(&si6, 0, sizeof(si6));
	}
#endif

	memset(&si, 0, sizeof(si));

	si.sin_family = AF_INET;
#ifdef SIN_LEN
	si.sin_len = sizeof(si);
#endif
	si.sin_addr.s_addr = htonl(INADDR_ANY);

#ifdef AF_INET6
	if (!ipv4_only) {
		si6.sin6_family = AF_INET6;
		#ifdef SIN6_LEN
		si6.sin6_len = sizeof(si);
		#endif
		si6.sin6_addr = in6addr_any;
		si6.sin6_flowinfo = 0;
	}
#endif

	while(1) {
		int bind1;
		int bind2;
		int bind3;

		if(sock < 0)

		sock = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);

#ifdef AF_INET6
		if(sock==-1 && type == AF_INET6) {
			// try fallback to IPv4
			type = AF_INET;
			si_p = (struct sockaddr*)&si;
			si_len = sizeof(si);
			sin_port = &si.sin_port;
			continue;
		}
#endif

		if (sock==-1)
			die("init_rtp: Can't create data socket!");

		if(csock < 0)
			csock = socket(type, SOCK_DGRAM, IPPROTO_UDP);
		if (csock==-1)
			die("init_rtp: Can't create control socket!");
		if(tsock < 0)
			tsock = socket(type, SOCK_DGRAM, IPPROTO_UDP);
		if (tsock==-1)
			die("init_rtp: Can't create timing socket!");

		*sin_port = htons(port);
		bind1 = bind(sock, si_p, si_len);
		*sin_port = htons(port + 1);
		bind2 = bind(csock, si_p, si_len);
		*sin_port = htons(port + 2);
		bind3 = bind(tsock, si_p, si_len);

		if(bind1 != -1 && bind2 != -1 && bind3 != -1) break;
		if(bind1 != -1) { close(sock); sock = -1; }
		if(bind2 != -1) { close(csock); csock = -1; }
		if(bind3 != -1) { close(tsock); tsock = -1; }

		port += 3;
	}

	_printf("port: %d\n", port); // let our handler know where we end up listening
	_printf("cport: %d\n", port+1);
	_printf("tport: %d\n", port+2);

	rtp_sockets[0] = sock;
	rtp_sockets[1] = csock;

	_fprintf(stderr, "shairport_helper: VERSION: %s\n", version);
	pthread_create(&rtp_thread, NULL, rtp_thread_func, (void *)rtp_sockets);

	return port;
}

static int init_http(void)
{
	int port = 8000;
	struct sockaddr_in servaddr;

	if (0 > (http_listener = socket(AF_INET, SOCK_STREAM, 0))) {
		_fprintf(stderr, "init_http: Could not create http listening socket (%s)\n", strerror(errno));
		die("init_http() failed.");
	}

	while (port < 8100) {
		/*  Populate socket address structure  */
		memset(&servaddr, 0, sizeof(servaddr));
		servaddr.sin_family      = AF_INET;
		servaddr.sin_addr.s_addr = htonl(INADDR_ANY);
		servaddr.sin_port        = htons(port);

		if (0 > bind(http_listener, (struct sockaddr*) &servaddr, sizeof(servaddr))) {
			// _fprintf(stderr, "init_http: Could not bind http listening socket to port %i (%s)\n", port, strerror(errno));
			port++;
			continue;
		}

		break;
	}

	/*  Make socket a listening socket  */
	if (0 > listen(http_listener, 1)) {
		_fprintf(stderr, "init_http: Could not listen on http socket (%s)\n", strerror(errno));
		die("init_http: failed");
	}

	_printf("hport: %d\n", port);

	return port;
}

static short lcg_rand(void) {
	static unsigned long lcg_prev = 12345;
	lcg_prev = lcg_prev * 69069 + 3;
	return lcg_prev & 0xffff;
}

static inline short dithered_vol(short sample) {
    static short rand_a; 
    static short rand_b;
    long out;

    out = (long)sample * fix_volume;
    if (fix_volume < 0x10000) {
        rand_b = rand_a;
        rand_a = lcg_rand();
        out += rand_a;
        out -= rand_b;
    }
    return out>>16;
}

typedef struct {
    double hist[2];
    double a[2];
    double b[3];
} biquad_t;

/*
static void biquad_init(biquad_t *bq, double a[], double b[]) {
    bq->hist[0] = bq->hist[1] = 0.0;
    memcpy(bq->a, a, 2*sizeof(double));
    memcpy(bq->b, b, 3*sizeof(double));
}
*/

/*
static void biquad_lpf(biquad_t *bq, double freq, double Q) {
    double w0 = 2.0 * M_PI * freq * frame_size / (double)sampling_rate;
    double alpha = sin(w0)/(2.0*Q);

    double a_0 = 1.0 + alpha;
    double b[3], a[2];
    b[0] = (1.0-cos(w0))/(2.0*a_0);
    b[1] = (1.0-cos(w0))/a_0;
    b[2] = b[0];
    a[0] = -2.0*cos(w0)/a_0;
    a[1] = (1-alpha)/a_0;

    biquad_init(bq, a, b);
}
*/

static double biquad_filt(biquad_t *bq, double in) {
    double w = in - bq->a[0]*bq->hist[0] - bq->a[1]*bq->hist[1];
    double out = bq->b[1]*bq->hist[0] + bq->b[2]*bq->hist[1] + bq->b[0]*w;
    bq->hist[1] = bq->hist[0];
    bq->hist[0] = w;

    return out;
}

static double bf_playback_rate = 1.0;

static double bf_est_drift = 0.0;   // local clock is slower by
static biquad_t bf_drift_lpf;
static double bf_est_err = 0.0;
static double bf_last_err;
static biquad_t bf_err_lpf; 
static biquad_t bf_err_deriv_lpf;
static double desired_fill;
static int fill_count;

/*
static void bf_est_reset(short fill) {
    biquad_lpf(&bf_drift_lpf, 1.0/180.0, 0.3);
    biquad_lpf(&bf_err_lpf, 1.0/10.0, 0.25);
    biquad_lpf(&bf_err_deriv_lpf, 1.0/2.0, 0.2);
    fill_count = 0;
    bf_playback_rate = 1.0;
    bf_est_err = bf_last_err = 0;
    desired_fill = fill_count = 0;
}
*/

static void bf_est_update(short fill) {
	double buf_delta;
	double err_deriv;
	double adj_error;

	if (fill_count < 1000) {
		desired_fill += (double)fill/1000.0;
		fill_count++;
		return;
	}

#define CONTROL_A   (1e-4)
#define CONTROL_B   (1e-1)

	buf_delta = fill - desired_fill;
	bf_est_err = biquad_filt(&bf_err_lpf, buf_delta);
	err_deriv = biquad_filt(&bf_err_deriv_lpf, bf_est_err - bf_last_err);
	adj_error = CONTROL_A * bf_est_err;

    bf_est_drift = biquad_filt(&bf_drift_lpf, CONTROL_B*(adj_error + err_deriv) + bf_est_drift);

    if (debug)
        _fprintf(stderr, "bf_est_update: bf %d err %f drift %f desiring %f ed %f estd %f\n",
                fill, bf_est_err, bf_est_drift, desired_fill, err_deriv, err_deriv + adj_error);
    bf_playback_rate = 1.0 + adj_error + bf_est_drift;

    bf_last_err = bf_est_err;
}

// get the next frame, when available. return 0 if underrun/stream reset.
static short *buffer_get_frame(void) {
    short buf_fill;
	seq_t read;
    abuf_t *abuf = 0;
    abuf_t *curframe = 0;
    unsigned short next;
    int i;

    if (ab_buffering)
        return 0;

    pthread_mutex_lock(&ab_mutex);

    buf_fill = ab_write - ab_read;

	if (buf_fill < MIN_FILL || !ab_synced) {    // init or underrun
	ab_buffering = 1;
        pthread_mutex_unlock(&ab_mutex);
        return 0;
	}
    if (buf_fill >= BUFFER_FRAMES) {   // overrunning! uh-oh. restart at a sane distance
        _fprintf(stderr, "buffer_get_frame: overrun. buf_fill: %i, buffer_frames: %i\n" , buf_fill, BUFFER_FRAMES);
        ab_read = ab_write - buffer_start_fill;
    }

    read = ab_read;
    ab_read++;
    buf_fill = ab_write - ab_read;
    bf_est_update(buf_fill);

    // check if t+16, t+32, t+64, t+128, ... (buffer_start_fill / 2)
    // packets have arrived... last-chance resend
    if (!ab_buffering) {
        for (i = 16; i < (buffer_start_fill/2); i = (i * 2)) {
            next = ab_read + i;
            abuf = audio_buffer + BUFIDX(next);
            if (!abuf->ready) {
                rtp_request_resend(next, next);
            }
        }
    }

    curframe = audio_buffer + BUFIDX(read);
    if (!curframe->ready) {
        _fprintf(stderr, "buffer_get_frame: missing frame\n");
        memset(curframe->data, 0, FRAME_BYTES);
	}
    curframe->ready = 0;
    pthread_mutex_unlock(&ab_mutex);

    return curframe->data;
}

static int stuff_buffer(double playback_rate, short *inptr, short *outptr) {
    int i;
    int stuffsamp = frame_size;
    int stuff = 0;
    double p_stuff;

    p_stuff = 1.0 - pow(1.0 - fabs(playback_rate-1.0), frame_size);

    if (rand() < p_stuff * RAND_MAX) {
        stuff = playback_rate > 1.0 ? -1 : 1;
        stuffsamp = rand() % (frame_size - 1);
    }

	pthread_mutex_lock(&vol_mutex);
    for (i=0; i<stuffsamp; i++) {   // the whole frame, if no stuffing
        *outptr++ = dithered_vol(*inptr++);
        *outptr++ = dithered_vol(*inptr++);
    };
    if (stuff) {
        if (stuff==1) {
            if (debug)
                _fprintf(stderr, "stuff_buffer: +++++++++\n");
            // interpolate one sample
            *outptr++ = dithered_vol(((long)inptr[-2] + (long)inptr[0]) >> 1);
            *outptr++ = dithered_vol(((long)inptr[-1] + (long)inptr[1]) >> 1);
        } else if (stuff==-1) {
            if (debug)
                _fprintf(stderr, "stuff_buffer: ---------\n");
            inptr++;
            inptr++;
        }
        for (i=stuffsamp; i<frame_size + stuff; i++) {
            *outptr++ = dithered_vol(*inptr++);
            *outptr++ = dithered_vol(*inptr++);
        }
    }
    pthread_mutex_unlock(&vol_mutex);

    return frame_size + stuff;
}

static void *audio_thread_func(void *arg) {
    int play_samples = 0;
	int i;
#ifdef WIN32
	signed short buf_fill;
#else
	signed short buf_fill __attribute__((unused));
#endif
	signed short *inbuf, *outbuf, *silence;
	outbuf = malloc(OUTFRAME_BYTES);
	silence = malloc(OUTFRAME_BYTES);

    for (i=0; i<OUTFRAME_BYTES/2; i++) {
        silence[i] = 0;
    }

#ifdef FANCY_RESAMPLING
    float *frame, *outframe;
    SRC_DATA srcdat;
	if (fancy_resampling) {
        frame = malloc(frame_size*2*sizeof(float));
        outframe = malloc(2*frame_size*2*sizeof(float));

        srcdat.data_in = frame;
        srcdat.data_out = outframe;
        srcdat.input_frames = FRAME_BYTES;
        srcdat.output_frames = 2*FRAME_BYTES;
        srcdat.src_ratio = 1.0;
        srcdat.end_of_input = 0;
    }
#endif

    while (1) {
        if (http_connection == -1) {
			http_connection = accept(http_listener, NULL, NULL);

            if (http_connection != -1) {
                if (debug)
                    _fprintf(stderr, "audio_thread_func: got HTTP connection %i\n", http_connection);

				http_status = 1;
				http_parser_ctx = malloc(sizeof(http_parser));
				http_parser_init(http_parser_ctx, HTTP_REQUEST);
			}
		}

		if (http_connection >= 0) {
			ssize_t recvd;
#ifdef WIN32
			fd_set rfds;
			struct timeval timeout = {0, 0};
			int n;

			FD_ZERO(&rfds);
			FD_SET(http_connection, &rfds);

			n = select(http_connection + 1, &rfds, NULL, NULL, &timeout);
			if (n <= 0) {
				if (n < 0 && (errno != EAGAIN) && (errno != WSAEWOULDBLOCK)) {
					_fprintf(stderr, "audio_thread_func: HTTP recv failed %d (%s)\n", n, strerror(errno));
				}
			} else {
				recvd = recv(http_connection, http_buffer, sizeof(http_buffer), 0);
#else
			if (0 > (recvd = recv(http_connection, http_buffer, sizeof(http_buffer), MSG_DONTWAIT))) {
				if ((errno != EAGAIN) && (errno != EWOULDBLOCK)) {
					_fprintf(stderr, "audio_thread_func: HTTP recv failed %d (%s)\n", errno, strerror(errno));
				}
			} else {
#endif
				if (recvd <= 0) {
					close(http_connection);
					http_connection = -1;
					free (http_parser_ctx);
					http_parser_ctx = 0;
					http_status = 0;
				} else {
					if (debug) {
						_fprintf(stderr, "audio_thread_func: HTTP recvd %d:\n", recvd);
						fwrite(http_buffer, recvd, 1, stderr);
						_fflush(stderr);
					}

                    http_parser_execute(http_parser_ctx, &http_settings, http_buffer, recvd);
                }
			}			        
        }

        inbuf = buffer_get_frame();
        while (!inbuf) {
            pthread_mutex_lock(&ab_mutex);
            pthread_cond_wait(&ab_buffer_ready, &ab_mutex);
            pthread_mutex_unlock(&ab_mutex);
            inbuf = buffer_get_frame();
        }

#ifdef FANCY_RESAMPLING
        if (fancy_resampling) {
            int i;
            pthread_mutex_lock(&vol_mutex);
            for (i=0; i<2*FRAME_BYTES; i++) {
                frame[i] = (float)inbuf[i] / 32768.0;
                frame[i] *= volume;
            }
            pthread_mutex_unlock(&vol_mutex);
            srcdat.src_ratio = bf_playback_rate;
            src_process(src, &srcdat);
            assert(srcdat.input_frames_used == FRAME_BYTES);
            src_float_to_short_array(outframe, outbuf, FRAME_BYTES*2);
            play_samples = srcdat.output_frames_gen;
        } else
#endif

        play_samples = stuff_buffer(bf_playback_rate, inbuf, outbuf);
        
		if (http_status == 1000) {
			ssize_t sent = send(http_connection, outbuf, play_samples * 4, 0);

			if (sent != (play_samples * 4)) {
				_fprintf(stderr, "HTTP send() unexpected response: %li (data=%i): %s\n", (long int)sent, play_samples * 4, strerror(errno));
			}
        }
    }

    return 0;
}

int http_on_headers_complete(http_parser* parser) 
{
    ssize_t sent;
    
	const char* response = "HTTP/1.1 200 OK\r\nServer: HairTunes\r\nConnection: close\r\nContent-Type: audio/L16;rate=44100;channels=2\r\n\r\n";
    
    if (debug)
        _fprintf(stderr, "http_on_headers_complete\n");
    
    sent = send(http_connection, response, strlen(response), 0);    
    
    if (sent != strlen(response)) {
        _fprintf(stderr, "HTTP send() unexpected response: %li (strlen=%lu)\n", (long int)sent, (long unsigned int)strlen(response));
    }
    
	http_status = 1000;
    
    return 0;
}

static int init_output(void) {
    void* arg = 0;

#ifdef FANCY_RESAMPLING
    int err;
    if (fancy_resampling)
        src = src_new(SRC_SINC_MEDIUM_QUALITY, 2, &err);
    else
        src = 0;
#endif

    pthread_t audio_thread;
	pthread_create(&audio_thread, NULL, audio_thread_func, arg);

    return 0;
}

#ifdef WIN32
char* strsep(char** stringp, const char* delim)
{
  char* start = *stringp;
  char* p;

  p = (start != NULL) ? strpbrk(start, delim) : NULL;

  if (p == NULL)
  {
	*stringp = NULL;
  }
  else
  {
	*p = '\0';
	*stringp = p + 1;
  }

  return start;
}
#endif

int create_socket(int port)
{
	struct sockaddr_in addr;
	int sd;
	char buf[256];

	sd = socket(AF_INET, SOCK_STREAM, 0);
	// set_nonblock(ctx->cli_sock);
	addr.sin_family = AF_INET;
	addr.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
	addr.sin_port = htons(port);

	if (connect(sd, (struct sockaddr *) &addr, sizeof(addr)) < 0) {
		close(sd);
		return -1;
	}

	sprintf(buf, "created socket %d", sd);
	puts(buf);

	return sd;
}

int close_socket(int sd)
{
#ifdef WIN32
	shutdown(sd, SD_BOTH);
#else
	shutdown(sd, SHUT_RDWR);
#endif
	return close(sd);
}

char *_fgets(char *str, int num, FILE *file)
{
	fd_set rfds;
	char *p = str;
	int n;

	if (file == stdin && in_sd == -1) return fgets(str, num, file);

	FD_ZERO(&rfds);
	FD_SET(in_sd, &rfds);

	n = select(in_sd + 1, &rfds, NULL, NULL, NULL);
	if (n == -1) return NULL;

	do {
		n = recv(in_sd, p, 1, 0);
		if (n == 1) p++;
		else break;
	}  while (p-str < num && *(p-1) != '\n');

	*p = '\0';

	if (p == str) return NULL;

	return str;
}


int _fprintf(FILE *file, ...)
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

	fprintf(stderr, p);
	fflush(stderr);

	free(p);

	return n;
}

int _fflush(FILE *file)
{
	if (!out_port || !err_port) return fflush(file);

	return 0;
}


