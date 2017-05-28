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

#include "platform.h"

#if WIN
char* strsep(char** stringp, const char* delim);
#endif

#include "hairtunes.h"

#include <assert.h>

#include "alac.h"
#include "FLAC/stream_encoder.h"
#include "http.h"
#include "log_util.h"

int _fprintf(FILE *file, ...);
#define _printf(...) _fprintf(stdout, ##__VA_ARGS__)
int _fflush(FILE *file);
char *_fgets(char *str, int n, FILE *file);
int in_port, out_port, err_port;
int in_sd = -1, out_sd = -1, err_sd = -1;
int create_socket(int port);
int close_socket(int sd);
unsigned int gettime_ms(void);

const char *version = "0.61.0";

static log_level 	main_loglevel = lERROR;
static log_level 	*loglevel = &main_loglevel;

// #define __RTP_STORE

#ifdef __RTP_STORE
FILE *rtpFP;
char *rtpFile = "airplay.pcm";
#endif

// default buffer size
#define BUFFER_FRAMES 1024
#define MAX_PACKET    2048
#define FLAC_BLOCK_SIZE 1024
#define MAX_FLAC_BYTES (FLAC_BLOCK_SIZE*4 + 1024)

#define AB_SYNC_LEVEL	  64
enum { AB_SYNCING, AB_BUFFERING, AB_RUNNING };

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
static FLAC__StreamEncoder *flac_encoder;
static char flac_buffer[MAX_FLAC_BYTES];
static int flac_len;
static bool flac_ready = false;

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


#define FRAME_BYTES (4*frame_size)

static alac_file *decoder_info;

static int  init_rtp(void);
static int  init_http(void);
static void init_buffer(void);
static int  init_output(void);
static bool rtp_request_resend(seq_t first, seq_t last);
static void ab_resync(void);
static FLAC__StreamEncoderWriteStatus flac_write_callback(const FLAC__StreamEncoder *encoder, const FLAC__byte buffer[], size_t bytes, unsigned samples, unsigned current_frame, void *client_data);

// interthread variables
// stdin->decoder
static int flush_seqno = -1;
static bool rtp_first = false;
static bool use_flac = false;
static unsigned int sync_time, sync_rtptime;
static struct sockaddr_storage rtp_control, rtp_audio;
static int rtp_sockets[2];  // data, control

typedef struct audio_buffer_entry {   // decoded audio packets
	int ready;
	unsigned rtptime;
	signed short *data;
} abuf_t;
static abuf_t audio_buffer[BUFFER_FRAMES];


#define BUFIDX(seqno) ((seq_t)(seqno) % BUFFER_FRAMES)

// mutex-protected variables
static seq_t ab_read, ab_write;
static int ab_sync = AB_SYNCING;
static pthread_mutex_t ab_mutex = PTHREAD_MUTEX_INITIALIZER;

static void die(char *why) {
	if (in_sd != -1) close_socket(in_sd);
	if (out_sd != -1) close_socket(out_sd);
	if (err_sd != -1) close_socket(err_sd);
	_fprintf(stderr, "FATAL: %s\n", why);
	LOG_ERROR("FATAL ERROR: %s", why);
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


static void init_flac() {
	bool ok = true;

	if (flac_ready) return;

	ok &= FLAC__stream_encoder_set_verify(flac_encoder, false);
	ok &= FLAC__stream_encoder_set_compression_level(flac_encoder, 5);
	ok &= FLAC__stream_encoder_set_channels(flac_encoder, 2);
	ok &= FLAC__stream_encoder_set_bits_per_sample(flac_encoder, 16);
	ok &= FLAC__stream_encoder_set_sample_rate(flac_encoder, 44100);
	ok &= FLAC__stream_encoder_set_blocksize(flac_encoder, FLAC_BLOCK_SIZE);
	ok &= !FLAC__stream_encoder_init_stream(flac_encoder, flac_write_callback, NULL, NULL, NULL, NULL);

	flac_len = 0;
	flac_ready = true;

	if (!ok) {
		LOG_ERROR("Cannot set FLAC parameters", NULL);
	}
}


static FLAC__StreamEncoderWriteStatus flac_write_callback(const FLAC__StreamEncoder *encoder, const FLAC__byte buffer[], size_t bytes, unsigned samples, unsigned current_frame, void *client_data) {
	if (bytes <= MAX_FLAC_BYTES) {
		flac_len = bytes;
		memcpy(flac_buffer, buffer, bytes);
	} else {
		LOG_WARN("flac coded buffer too big %u", bytes);
	}

 	return FLAC__STREAM_ENCODER_WRITE_STATUS_OK;
}
 

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
		 char *pRtpHost)
{
	int i = 0;
	char *arg;
	char line[128];
	int in_line = 0;
	int n;

#ifdef __RTP_STORE
	rtpFP = fopen(rtpFile, "wb");
#endif

	if(pAeskey != NULL)
		memcpy(aeskey, pAeskey, sizeof(aeskey));
	if(pAesiv != NULL)
		memcpy(aesiv, pAesiv, sizeof(aesiv));
	if(pRtpHost != NULL)
		rtphost = pRtpHost;

	controlport = pCtrlPort;
	timingport = pTimingPort;

	AES_set_decrypt_key(aeskey, 128, &aes);

	memset(fmtp, 0, sizeof(fmtp));

	while ( (arg = strsep(&fmtpstr, " \t")) != NULL)
		fmtp[i++] = atoi(arg);

	init_decoder();
	init_buffer();
	init_rtp();      // open a UDP listen port and start a listener; decode into ring buffer
	init_http();
	if (use_flac) {
		if ((flac_encoder = FLAC__stream_encoder_new()) == NULL) {
			LOG_ERROR("Cannot init FLAC", NULL);
			return 0;
		}
		LOG_INFO("Using FLAC", NULL);
	}	
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
		if (!strcmp(line, "exit\n")) {
			return (0);
		}
		if (strstr(line, "flush")) {
			pthread_mutex_lock(&ab_mutex);
			ab_resync();
			if (use_flac) {
				FLAC__stream_encoder_finish(flac_encoder);
				flac_ready = false;
			}
			pthread_mutex_unlock(&ab_mutex);
			sscanf(line, "flush %d", &flush_seqno);
			_printf("flushed %d\n", flush_seqno);
			LOG_INFO("Flushed at %u", flush_seqno);
		}
	}
	
	if (use_flac) FLAC__stream_encoder_delete(flac_encoder);
	
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
			printf("AIRPORT mode:\n\tiv <n> key <n> [socket <in>,<out>,<err>] "
				   "[ipv4_only] [fmtp <n>] [cport <n>] "
				   "[tport <n>] [host <n>] "
				   "[log <file>] [dbg <error|warn|info|debug|sdebug>] "
				   "flac [encode output using flac] "
				   "\n");
		} else mdns_server(argc - 1, argv + 1);
		exit (0);
	}
#endif

	while ( (arg = *++argv) != NULL ) {
		if (!strcasecmp(arg, "iv")) {
			hexaesiv = *++argv;
		} else
		if (!strcasecmp(arg, "ipv4_only")) {
			ipv4_only = 1;
		} else
		if (!strcasecmp(arg, "key")) {
			hexaeskey = *++argv;
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
		ret = hairtunes_init(NULL, NULL, fmtpstr, controlport, timingport, NULL);
	} else {
		_fprintf(stderr, "Cannot start, check parameters");
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
#endif

static void init_buffer(void) {
	int i;
	for (i = 0; i < BUFFER_FRAMES; i++)
		audio_buffer[i].data = malloc(FRAME_BYTES);
	ab_resync();
}

static void ab_resync(void) {
	int i;
	for (i = 0; i < BUFFER_FRAMES; i++) {
		audio_buffer[i].ready = 0;
	}
	ab_sync = AB_SYNCING;
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

static void buffer_put_packet(seq_t seqno, unsigned rtptime, bool resend, char *data, int len) {
	abuf_t *abuf = NULL;
	static int count = 0;

	pthread_mutex_lock(&ab_mutex);

	if (ab_sync == AB_SYNCING) {
		if (rtp_first || ((flush_seqno != -1) && ((seqno > flush_seqno) || seqno + 8192 < flush_seqno))) {
			ab_write = seqno-1;
			ab_read = seqno;
			ab_sync = AB_BUFFERING;
			sync_rtptime = rtptime;
			sync_time = gettime_ms();
			rtp_first = false;
			flush_seqno = -1;
			if (use_flac) init_flac();
		} else {
			pthread_mutex_unlock(&ab_mutex);
			return;
	   }
	}

	if (!(count++ & 0x1ff)) {
		LOG_INFO("buffer fill status [level:%u] [W:%u R:%u]", ab_write - ab_read, ab_write, ab_read);
	}

	if (ab_sync == AB_BUFFERING && (ab_write - ab_read) > AB_SYNC_LEVEL) ab_sync = AB_RUNNING;

	if (seqno == (seq_t)(ab_write+1)) {                  // expected packet
		abuf = audio_buffer + BUFIDX(seqno);
		ab_write = seqno;
		LOG_SDEBUG("packet expected seqno:%u rtptime:%u (W:%u R:%u)", seqno, rtptime, ab_write, ab_read);
	} else if (seq_order(ab_write, seqno)) {    // newer than expected
		if (rtp_request_resend(ab_write+1, seqno-1)) {
			seq_t i;
			for (i = ab_write+1; i <= seqno-1; i++) audio_buffer[BUFIDX(i)].rtptime = rtptime - (seqno-i)*frame_size;
		}
		LOG_DEBUG("packet newer seqno:%u rtptime:%u (W:%u R:%u)", seqno, rtptime, ab_write, ab_read);
		abuf = audio_buffer + BUFIDX(seqno);
		ab_write = seqno;
	} else if (seqno == ab_read || seq_order(ab_read, seqno)) {     // late but not yet played
		abuf = audio_buffer + BUFIDX(seqno);
		LOG_DEBUG("packet recovered seqno:%u rtptime:%u (W:%u R:%u)", seqno, rtptime, ab_write, ab_read);
	} else {    // too late.
		LOG_DEBUG("packet too late seqno:%u rtptime:%u (W:%u R:%u)", seqno, rtptime, ab_write, ab_read);
	}

	if (abuf) {
		alac_decode(abuf->data, data, len);
		abuf->ready = 1;
		abuf->rtptime = rtptime;
#ifdef __RTP_STORE
		fwrite(abuf->data, FRAME_BYTES, 1, rtpFP);
#endif
	}

	pthread_mutex_unlock(&ab_mutex);
}

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
	unsigned rtptime;
	socklen_t rtp_client_len;

	FD_ZERO(&fds);
	FD_SET(sock, &fds);
	FD_SET(csock, &fds);

	while (select(csock>sock ? csock+1 : sock+1, &fds, 0, 0, 0) != -1) {

		if (FD_ISSET(sock, &fds)) readsock = sock;
		else readsock = csock;

		FD_SET(sock, &fds);
		FD_SET(csock, &fds);

		rtp_client_len = sizeof(struct sockaddr_storage);
		plen = recvfrom(readsock, packet, sizeof(packet), 0, (struct sockaddr*) ((readsock == csock) ? &rtp_control : &rtp_audio), &rtp_client_len);

		if (plen < 0) continue;
		assert(plen <= MAX_PACKET);

		type = packet[1] & ~0x80;

		if (type == 0x60 || type == 0x56) {   // audio data / resend
			pktp = packet;
			if (type == 0x56) {
				pktp += 4;
				plen -= 4;
			}
			seqno = ntohs(*(unsigned short *)(pktp+2));
			rtptime = ntohl(*(unsigned int*)(pktp+4));

			// adjust pointer and length
			pktp += 12;
			plen -= 12;

			LOG_SDEBUG("seqno:%u rtp:%u (type: %x, first: %u)", seqno, rtptime, type, packet[1] & 0x80);

			// check if packet contains enough content to be reasonable
			if (plen >= 16) {
				if ((packet[1] & 0x80) && (type != 0x56)) {
					rtp_first = true;
					LOG_INFO("1st audio packet received", NULL);
				}
				buffer_put_packet(seqno, rtptime, type == 0x56, pktp, plen);
			}
		} else if (type == 0x54 && (packet[0] & 0x10)) {
			// 1st sync packet received (signals a restart of playback)
			_printf("play\n");
			rtp_first = true;
			LOG_INFO("1st sync packet received", NULL);
		} else {
			LOG_SDEBUG("unused packet:%u", type);
		}
	}

	return NULL;
}

static bool rtp_request_resend(seq_t first, seq_t last) {
	unsigned char req[8];    // *not* a standard RTCP NACK

	// do not request silly ranges (happens in case of network large blackouts)
	if (seq_order(last, first) || last - first > BUFFER_FRAMES / 2) return false;

	LOG_DEBUG("resend request [W:%u R:%u first=%u last=%u]", ab_write, ab_read, first, last);

	req[0] = 0x80;
	req[1] = 0x55|0x80;  // Apple 'resend'
	*(unsigned short *)(req+2) = htons(1);  // our seqnum
	*(unsigned short *)(req+4) = htons(first);  // missed seqnum
	*(unsigned short *)(req+6) = htons(last-first+1);  // count

	if (ipv4_only) {
	((struct sockaddr_in *)&rtp_control)->sin_port = htons(controlport);
	}
#ifdef AF_INET6
	else {
	((struct sockaddr_in6 *)&rtp_control)->sin6_port = htons(controlport);
	}
#endif

	if (sizeof(req) != sendto(rtp_sockets[1], req, sizeof(req), 0, (struct sockaddr*) &rtp_control, sizeof(struct sockaddr_storage))) {
		LOG_WARN("SENDTO failed (%s)", strerror(errno));
	}

	return true;
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
			LOG_INFO("Could not bind http listening socket to port %i (%s)", port, strerror(errno));
			port++;
			continue;
		}

		break;
	}

	/*  Make socket a listening socket  */
	if (0 > listen(http_listener, 1)) {
		_fprintf(stderr, "init_http: Could not listen on http listener socket (%s)\n", strerror(errno));
		die("init_http: failed");
	}

	_printf("hport: %d\n", port);

	return port;
}

// get the next frame, when available. return 0 if underrun/stream reset.
static short *buffer_get_frame(void) {
	short buf_fill;
	abuf_t *curframe = 0;
	unsigned now, fpos_rtptime, fpos_time ;
	int i;
	static int count = 0;

	pthread_mutex_lock(&ab_mutex);

	buf_fill = ab_write - ab_read;

	if (!(count++ & 0x1ff)) {
		LOG_INFO("buffer drain status [level:%u] [W:%u R:%u]", buf_fill, ab_write, ab_read);
	}

	if (buf_fill >= BUFFER_FRAMES) {
		LOG_ERROR("Buffer overrun %u", buf_fill);
		ab_read = ab_write - AB_SYNC_LEVEL;
	}

	now = gettime_ms();
	curframe = audio_buffer + BUFIDX(ab_read);
	fpos_rtptime = (curframe->rtptime - sync_rtptime) / frame_size;
	fpos_time = ((unsigned long long) (now - sync_time) * 44100) / (1000 * frame_size);

	// no frame available yet or buffering or too early
	if (ab_sync != AB_RUNNING || !buf_fill || fpos_time - fpos_rtptime < AB_SYNC_LEVEL) {
		LOG_SDEBUG("waiting (fill:%u gap:%u, W:%u R:%u)", buf_fill - 1, fpos_time - fpos_rtptime, ab_write, ab_read);
		pthread_mutex_unlock(&ab_mutex);
		return NULL;
	}

	// last chance resend
	for (i = 16; i < AB_SYNC_LEVEL/2 && seq_order(ab_read + i, ab_write); i = (i * 2)) {
		if (!audio_buffer[BUFIDX(ab_read + i)].ready) rtp_request_resend(ab_read + i, ab_read + i);
	}

	if (!curframe->ready) {
		LOG_DEBUG("created zero frame (fill:%u gap:%u, W:%u R:%u)", buf_fill - 1, fpos_time - fpos_rtptime, ab_write, ab_read);
		memset(curframe->data, 0, FRAME_BYTES);
	}
	else {
		LOG_SDEBUG("prepared frame (fill:%u gap:%u, W:%u R:%u)", buf_fill - 1, fpos_time - fpos_rtptime, ab_write, ab_read);
	}

	curframe->ready = 0;
	ab_read++;
	
	pthread_mutex_unlock(&ab_mutex);

	return curframe->data;
}


static void *audio_thread_func(void *arg) {
	signed short *inbuf;
	int frame_count = 0;
	FLAC__int32 *flac_samples = NULL;

	if (use_flac) {
		if ((flac_samples = malloc(2 * frame_size * sizeof(FLAC__int32))) == NULL) {
			LOG_ERROR("Cannot allocate FLAC sample buffer %u", frame_size);
		}
	}

	while (1) {
		ssize_t sent;

		if (http_connection == -1) {
			http_connection = accept(http_listener, NULL, NULL);

			if (http_connection != -1) {
				LOG_INFO("audio_thread_func: got HTTP connection %u", http_connection);

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
					LOG_WARN("HTTP recv failed %d (%s)", n, strerror(errno));
				}
			} else {
				recvd = recv(http_connection, http_buffer, sizeof(http_buffer), 0);
#else
			if (0 > (recvd = recv(http_connection, http_buffer, sizeof(http_buffer), MSG_DONTWAIT))) {
				if ((errno != EAGAIN) && (errno != EWOULDBLOCK)) {
					_fprintf(stderr, "audio_thread_func: HTTP recv failed %d (%s)\n", errno, strerror(errno));
					LOG_WARN("audio_thread_func: HTTP recv failed %d (%s)", errno, strerror(errno));
				}
			} else {
#endif
				if (recvd <= 0) {
					close_socket(http_connection);
					LOG_INFO("HTTP close %u", http_connection);
					frame_count = 0;
					http_connection = -1;
					free (http_parser_ctx);
					http_parser_ctx = 0;
					http_status = 0;
				} else {
					if (*loglevel == lDEBUG) {
						LOG_DEBUG("HTTP recvd %d:", recvd);
						fwrite(http_buffer, recvd, 1, stderr);
					}
					http_parser_execute(http_parser_ctx, &http_settings, http_buffer, recvd);
				}
			}
		}

		/*
		When using synchronized players, LMS blocks the addRead of the player
		until the HTTP connection is established. This means the RECORD message
		is not responded and RTP does not start. So if this loops blocks when 
		there is no frame received yet, this is a dead lock
		*/

		// even if the HTTP session is not established, empty the buffer queue
		if ((inbuf = buffer_get_frame()) != NULL) {
			if (http_status == 1000) {
				int len;

				if (use_flac) {
					for (len = 0; len < 2*frame_size; len++) flac_samples[len] = inbuf[len];
					FLAC__stream_encoder_process_interleaved(flac_encoder, flac_samples, frame_size);
					inbuf = (void*) flac_buffer;
					len = flac_len;
					flac_len = 0;
				} else len = FRAME_BYTES;

				if (len) {
					u32_t gap;
					LOG_SDEBUG("HTTP sent frame count:%u bytes:%u (W:%u R:%u)", frame_count++, len, ab_write, ab_read);
					gap = gettime_ms();
					sent = send(http_connection, (void*) inbuf, len, 0);
					gap = gettime_ms() - gap;
					if (gap > 50) {
						LOG_ERROR("Spent %u ms in send! %u", gap);
					}
					if (sent != len) {
						LOG_WARN("HTTP send() unexpected response: %li (data=%i): %s", (long int)sent, len, strerror(errno));
					}
				} else {
					LOG_SDEBUG("no encoded frame ready yet, waiting", NULL);
					usleep(frame_size*((1000*2*1000)/(44100*3)));
				}
			}
		} else {
			usleep(frame_size*((1000*2*1000)/(44100*3)));
		}	
	}

	if (use_flac && flac_samples) free(flac_samples);

	return NULL;
}

int http_on_headers_complete(http_parser* parser)
{
	ssize_t sent;
	const char* response;

	if (use_flac) response = "HTTP/1.1 200 OK\r\nServer: HairTunes\r\nConnection: close\r\nContent-Type: audio/flac\r\n\r\n";
	else response = "HTTP/1.1 200 OK\r\nServer: HairTunes\r\nConnection: close\r\nContent-Type: audio/L16;rate=44100;channels=2\r\n\r\n";
	
	LOG_INFO("HTTP header received", NULL);
	sent = send(http_connection, response, strlen(response), 0);

	if (sent != (ssize_t) strlen(response)) {
		LOG_ERROR("HTTP send() unexpected response: %li (strlen=%lu)", (long int)sent, (long unsigned int)strlen(response));
	}

	http_status = 1000;

	return 0;
}

static int init_output(void) {
	void* arg = 0;
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

	sd = socket(AF_INET, SOCK_STREAM, 0);
	// set_nonblock(ctx->cli_sock);
	addr.sin_family = AF_INET;
	addr.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
	addr.sin_port = htons(port);

	if (connect(sd, (struct sockaddr *) &addr, sizeof(addr)) < 0) {
		close(sd);
		return -1;
	}

	LOG_DEBUG("created socket %d", sd);

	return sd;
}

int close_socket(int sd)
{
#ifdef WIN32
	shutdown(sd, SD_BOTH);
#else
	shutdown(sd, SHUT_RDWR);
#endif

	LOG_DEBUG("closed socket %d", sd);

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

	free(p);

	return n;
}

int _fflush(FILE *file)
{
	if (!out_port || !err_port) return fflush(file);

	return 0;
}

unsigned int gettime_ms(void) {
#ifdef WIN32
	return GetTickCount();
#else
#if defined(linux) || defined(__FreeBSD_)
	struct timespec ts;
	if (!clock_gettime(CLOCK_MONOTONIC, &ts)) {
		return ts.tv_sec * 1000 + ts.tv_nsec / 1000000;
	}
#endif
	struct timeval tv;
	gettimeofday(&tv, NULL);
	return tv.tv_sec * 1000 + tv.tv_usec / 1000;
#endif
}


