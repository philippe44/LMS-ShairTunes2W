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

#define NTP2MS(ntp) ((((ntp) >> 10) * 1000L) >> 22)
#define MS2NTP(ms) (((((__u64) (ms)) << 22) / 1000) << 10)
#define NTP2TS(ntp, rate) ((((ntp) >> 16) * (rate)) >> 16)
#define TS2NTP(ts, rate)  (((((__u64) (ts)) << 16) / (rate)) << 16)
#define MS2TS(ms, rate) ((((__u64) (ms)) * (rate)) / 1000)
#define TS2MS(ts, rate) NTP2MS(TS2NTP(ts,rate))

#define GAP_THRES	8
#define GAP_COUNT	20

extern int mdns_server(int argc, char *argv[]);
int _fprintf(FILE *file, ...);
#define _printf(...) _fprintf(stdout, ##__VA_ARGS__)
int _fflush(FILE *file);
char *_fgets(char *str, int n, FILE *file);
int in_port, out_port, err_port;
int in_sd = -1, out_sd = -1, err_sd = -1;
int latency = 0;
int conn_socket(int port);
int close_socket(int sd);
unsigned int gettime_ms(void);

const char *version = "0.70.0";

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

typedef u16_t seq_t;

// global options (constant after init)
static unsigned char aeskey[16], aesiv[16];
static AES_KEY aes;
static int fmtp[32];
static int sampling_rate;
static int frame_size;
static struct in_addr host_addr;
struct sockaddr_in rtp_host = { AF_INET, INADDR_ANY };

static struct {
	int port, sock;
} rtp_sockets[3]; 					 // data, control, timing

enum { DATA, CONTROL, TIMING };

#define RTP_SYNC	(0x01)
#define NTP_SYNC	(0x02)

static struct timing_s {
	u64_t local;
	u64_t remote;
	u32_t count, gap_count;
	s64_t gap_sum, gap_adjust;
} timing;

static struct {
	u32_t rtp;
	u32_t time;
	u8_t  status;
} synchro;

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

static bool  init_rtp(void);
static bool  init_http(void);
static void init_buffer(void);
static void init_flac(void);
static int  init_output(void);
static bool rtp_request_resend(seq_t first, seq_t last);
static bool rtp_request_timing(void);
static void ab_reset(void);
static FLAC__StreamEncoderWriteStatus flac_write_callback(const FLAC__StreamEncoder *encoder, const FLAC__byte buffer[], size_t bytes, unsigned samples, unsigned current_frame, void *client_data);

// interthread variables
// stdin->decoder
static int flush_seqno = -1;
static bool playing = false;
static bool use_flac = false;

typedef struct audio_buffer_entry {   // decoded audio packets
	int ready;
	u32_t rtptime;
	s16_t *data;
} abuf_t;
static abuf_t audio_buffer[BUFFER_FRAMES];

#define BUFIDX(seqno) ((seq_t)(seqno) % BUFFER_FRAMES)

// mutex-protected variables
static seq_t ab_read, ab_write;
static pthread_mutex_t ab_mutex = PTHREAD_MUTEX_INITIALIZER;

static void die(char *why) {
	if (in_sd != -1) close_socket(in_sd);
	if (out_sd != -1) close_socket(out_sd);
	if (err_sd != -1) close_socket(err_sd);
	_fprintf(stderr, "FATAL: %s\n", why);
	LOG_ERROR("FATAL ERROR: %s", why);
	exit(1);
}

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

int hairtunes_init(char *pAeskey, char *pAesiv, char *fmtpstr, int pCtrlPort, int pTimingPort)
{
	int i = 0;
	char *arg;
	char line[128];
	int in_line = 0;
	int n;

#ifdef __RTP_STORE
	rtpFP = fopen(rtpFile, "wb");
#endif

	if (hex2bin(aesiv, pAesiv))
		die("can't understand IV");
	if (hex2bin(aeskey, pAeskey))
		die("can't understand key");

	rtp_sockets[CONTROL].port = pCtrlPort;
	rtp_sockets[TIMING].port = pTimingPort;

	AES_set_decrypt_key(aeskey, 128, &aes);

	memset(fmtp, 0, sizeof(fmtp));

	while ( (arg = strsep(&fmtpstr, " \t")) != NULL)
		fmtp[i++] = atoi(arg);

	init_decoder();
	init_buffer();
	if (!init_rtp() < 0) die("cannot init RTP");
	if (!init_http()) die("cannot init HTTP");
	if (use_flac) {
		if ((flac_encoder = FLAC__stream_encoder_new()) == NULL) die("Cannot init FLAC");
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
			ab_reset();
			if (use_flac) {
				FLAC__stream_encoder_finish(flac_encoder);
				flac_ready = false;
			}
			sscanf(line, "flush %d", &flush_seqno);
			pthread_mutex_unlock(&ab_mutex);
			_printf("flushed %d\n", flush_seqno);
			LOG_INFO("Flushed at %u", flush_seqno);
		}
	}
	
	if (use_flac) FLAC__stream_encoder_delete(flac_encoder);

	_fprintf(stderr, "bye!\n", NULL);
	_fflush(stderr);

	return EXIT_SUCCESS;
}


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

int main(int argc, char **argv) {
	char *hexaeskey = 0;
	char *hexaesiv = 0;
	char *fmtpstr = 0;
	char *arg;
	char *logfile = NULL;
	int ret;
	int cport = 0, tport = 0;

	assert(RAND_MAX >= 0x7fff);    // XXX move this to compile time

	if (argc > 1 && (!strcmp(argv[1], "-h") || !strcmp(argv[1], "-dns"))) {
		if (!strcmp(argv[1], "-h")) {
			printf("MDNS mode:\n\t-dns ");
			mdns_server(argc, argv);
			printf("AIRPORT mode:\n\tiv <n> key <n> \n"
				   "[host <ip>]\n"
				   "[socket <in>,<out>,<err>]\n"
				   "[fmtp <n>]\n"
				   "[cport <n>] [tport <n>]\n"
				   "[log <file>] [dbg <error|warn|info|debug|sdebug>]\n"
				   "[flac]\n"
				   "[latency <ms>]\n"
				   );
		} else mdns_server(argc - 1, argv + 1);
		exit (0);
	}

	while ( (arg = *++argv) != NULL ) {
		if (!strcasecmp(arg, "iv")) {
			hexaesiv = *++argv;
		} else
		if (!strcasecmp(arg, "key")) {
			hexaeskey = *++argv;
		} else
		if (!strcasecmp(arg, "host")) {
			host_addr.s_addr = inet_addr(*++argv);
		} else
    	if (!strcasecmp(arg, "fmtp")) {
			fmtpstr = *++argv;
		} else
		if (!strcasecmp(arg, "cport")) {
			cport = atoi(*++argv);
		} else
		if (!strcasecmp(arg, "tport")) {
			tport = atoi(*++argv);
		} else
		if (!strcasecmp(arg, "latency")) {
			latency = atoi(*++argv);
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

	LOG_INFO("client: %s", inet_ntoa(host_addr));

#ifdef WIN32
	winsock_init();
#endif

	if (in_port) in_sd = conn_socket(in_port);
	if (out_port) out_sd = conn_socket(out_port);
	if (err_port) err_sd = conn_socket(err_port);

	if (((in_port || out_port || err_port) && (in_sd * out_sd * err_sd > 0)) ||
		(!in_port && !out_port && !err_port)) {
		ret = hairtunes_init(hexaeskey, hexaesiv, fmtpstr, cport, tport);
	} else {
		LOG_ERROR("Cannot start, check parameters", NULL);
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

static void init_buffer(void) {
	int i;
	for (i = 0; i < BUFFER_FRAMES; i++)
		audio_buffer[i].data = malloc(FRAME_BYTES);
	ab_reset();
}

static void ab_reset(void) {
	int i;
	for (i = 0; i < BUFFER_FRAMES; i++) {
		audio_buffer[i].ready = 0;
	}
	playing = false;
}

// the sequence numbers will wrap pretty often.
// this returns true if the second arg is after the first
static inline int seq_order(seq_t a, seq_t b) {
	s16_t d = b - a;
	return d > 0;
}

static void alac_decode(s16_t *dest, char *buf, int len) {
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

static void buffer_put_packet(seq_t seqno, unsigned rtptime, char *data, int len) {
	abuf_t *abuf = NULL;
	static int count = 0;

	pthread_mutex_lock(&ab_mutex);

	if (!playing) {
		if (flush_seqno == -1 || seq_order(flush_seqno, seqno)) {
			ab_write = seqno-1;
			ab_read = seqno;
			flush_seqno = -1;
			playing = true;
			_printf("play\n");
			LOG_INFO("play", NULL);
			if (use_flac) init_flac();
		} else {
			pthread_mutex_unlock(&ab_mutex);
			return;
		}
	}

	if (!(count++ & 0x1ff)) {
		LOG_INFO("buffer fill status [level:%u] [W:%u R:%u]", ab_write - ab_read, ab_write, ab_read);
	}

	if (seqno == (seq_t)(ab_write+1)) {                  // expected packet
		abuf = audio_buffer + BUFIDX(seqno);
		ab_write = seqno;
		LOG_SDEBUG("packet expected seqno:%u rtptime:%u (W:%u R:%u)", seqno, rtptime, ab_write, ab_read);
	} else if (seq_order(ab_write, seqno)) {    // newer than expected
		if (rtp_request_resend(ab_write+1, seqno-1)) {
			seq_t i;
			for (i = ab_write+1; i <= seqno-1; i++)	audio_buffer[BUFIDX(i)].rtptime = rtptime - (seqno-i)*frame_size;
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
		// this is the local time when this frame is epxected to play
		abuf->rtptime = rtptime;
#ifdef __RTP_STORE
		fwrite(abuf->data, FRAME_BYTES, 1, rtpFP);
#endif
	}

	pthread_mutex_unlock(&ab_mutex);
}

static void *rtp_thread_func(void *arg) {
	fd_set fds;
	int i, sock = -1;
	int count = 0;
	bool ntp_sent;

	FD_ZERO(&fds);

	for (i = 0; i < 3; i++) {
		FD_SET(rtp_sockets[i].sock, &fds);
		if (rtp_sockets[i].sock > sock) sock = rtp_sockets[i].sock;
		// send synchro requets 3 times
		ntp_sent = rtp_request_timing();
	}

	while (select(sock + 1, &fds, 0, 0, 0) != -1) {
		ssize_t plen;
		char type, packet[MAX_PACKET];
		socklen_t rtp_client_len = sizeof(struct sockaddr_storage);
		int idx = 0;
		char *pktp = packet;

		for (i = 0; i < 3; i++)	{
			if (FD_ISSET(rtp_sockets[i].sock, &fds)) idx = i;
			FD_SET(rtp_sockets[i].sock, &fds);
		}

		plen = recvfrom(rtp_sockets[idx].sock, packet, sizeof(packet), 0, (struct sockaddr*) &rtp_host, &rtp_client_len);

		if (!ntp_sent) {
			LOG_WARN("NTP request not send yet", NULL);
			ntp_sent = rtp_request_timing();
        }

		if (plen < 0) continue;
		assert(plen <= MAX_PACKET);

		type = packet[1] & ~0x80;
		pktp = packet;

		switch (type) {
			seq_t seqno;
			unsigned rtptime;

			// re-sent packet
			case 0x56: {
				pktp += 4;
				plen -= 4;
			}

			// data packet
			case 0x60: {
				seqno = ntohs(*(u16_t*)(pktp+2));
				rtptime = ntohl(*(u32_t*)(pktp+4));

				// adjust pointer and length
				pktp += 12;
				plen -= 12;

				LOG_SDEBUG("seqno:%u rtp:%u (type: %x, first: %u)", seqno, rtptime, type, packet[1] & 0x80);

				// check if packet contains enough content to be reasonable
				if (plen < 16) break;

				if ((packet[1] & 0x80) && (type != 0x56)) {
					LOG_INFO("1st audio packet received", NULL);
				}

				buffer_put_packet(seqno, rtptime, pktp, plen);

				break;
			}

			// sync packet
			case 0x54: {
				u32_t rtp_now_latency = ntohl(*(u32_t*)(pktp+4));
				u64_t remote = (((u64_t) ntohl(*(u32_t*)(pktp+8))) << 32) + ntohl(*(u32_t*)(pktp+12));
				u32_t rtp_now = ntohl(*(u32_t*)(pktp+16));

				// re-align timestamp and expected local playback time (mutex not needed)
				synchro.rtp = (latency) ? rtp_now - (latency*44100)/1000 : rtp_now_latency;
				synchro.time = timing.local + (u32_t) NTP2MS(remote - timing.remote);

				// now we are synced on RTP frames
				synchro.status |= RTP_SYNC;

				// 1st sync packet received (signals a restart of playback)
				if (packet[0] & 0x10) {
					LOG_INFO("1st sync packet received", NULL);
				}

				LOG_DEBUG("sync packet rtp_latency:%u rtp:%u remote ntp:%Lx, local time %u (now:%u)",
						  rtp_now_latency, rtp_now, remote, synchro.time, gettime_ms());

				if (!count--) {
					rtp_request_timing();
					count = 3;
				}

				break;
			}

			// NTP timing packet
			case 0x53: {
				u64_t expected;
				s64_t delta = 0;
				u32_t reference   = ntohl(*(u32_t*)(pktp+12)); // only low 32 bits in our case
				u64_t remote 	  =(((u64_t) ntohl(*(u32_t*)(pktp+16))) << 32) + ntohl(*(u32_t*)(pktp+20));
				/*
				u64_t remote_sent = remote_sent = (((u64_t) ntohl(*(u32_t*)(pktp+24))) << 32) + ntohl(*(u32_t*)(pktp+28));
				u32_t now 		  = gettime_ms();
				*/

				/*
				 This expected time is more than it should be due to the
				 network transit time server => client, but the timing.remote
				 also has the same error, assuming client => server is the same
				 so the delta calculated below is correct
				*/
				expected = timing.remote + MS2NTP(reference - timing.local);

				timing.remote = remote;
				timing.local = reference;
				timing.count++;

				if (synchro.status & NTP_SYNC) {
					delta = NTP2MS((s64_t) expected - (s64_t) timing.remote);
					timing.gap_sum += delta;

					pthread_mutex_lock(&ab_mutex);

					/*
					  if expected time is more than remote, then our time is
					  running faster and we are transmitting frames too quickly,
					  so we'll run out of frames, need to add one
					*/
					if (timing.gap_sum > GAP_THRES && timing.gap_count++ > GAP_COUNT) {
						LOG_INFO("Sending packets too fast %d", timing.gap_sum);
						ab_read--;
						timing.gap_sum -= GAP_THRES;
						timing.gap_adjust -= GAP_THRES;
					/*
					  if expected time is less than remote, then our time is
					  running slower and we are transmitting frames too slowly,
					  so we'll overflow frames buffer, need to remove one
					*/
					} else if (timing.gap_sum < -GAP_THRES && timing.gap_count++ > GAP_COUNT) {
						LOG_INFO("Sending packets too slow %d", timing.gap_sum);
						ab_read++;
						timing.gap_sum += GAP_THRES;
						timing.gap_adjust += GAP_THRES;
					}

					if (abs(timing.gap_sum) < 8) timing.gap_count = 0;

					pthread_mutex_unlock(&ab_mutex);
				}

				// now we are synced on NTP (mutex not needed)
				synchro.status |= NTP_SYNC;

				LOG_DEBUG("Timing references local:%Lu, remote:%Lx (delta:%Ld, sum:%Ld, adjust:%Ld, gaps:%d)",
						  timing.local, timing.remote, delta, timing.gap_sum, timing.gap_adjust, timing.gap_count);

				break;
			}
		}
	}

	return NULL;
}

static bool rtp_request_timing(void) {
	unsigned char req[32];
	u32_t now = gettime_ms();
	int i;
	struct sockaddr_in host;

	LOG_DEBUG("timing request now:%u (port: %u)", now, rtp_sockets[TIMING].port);

	req[0] = 0x80;
	req[1] = 0x52|0x80;
	*(u16_t*)(req+2) = htons(7);
	*(u32_t*)(req+4) = htonl(0);  // dummy
	for (i = 0; i < 16; i++) req[i+8] = 0;
	*(u32_t*)(req+24) = 0;
	*(u32_t*)(req+28) = htonl(now); // this is not a real NTP, but a 32 ms counter in the low part of the NTP

	if (host_addr.s_addr != INADDR_ANY) {
		host.sin_family = AF_INET;
		host.sin_addr =	host_addr;
	} else host = rtp_host;

	// no address from sender, need to wait for 1st packet to be received
	if (host.sin_addr.s_addr == INADDR_ANY) return false;

	host.sin_port = htons(rtp_sockets[TIMING].port);

	if (sizeof(req) != sendto(rtp_sockets[TIMING].sock, req, sizeof(req), 0, (struct sockaddr*) &host, sizeof(host))) {
		LOG_WARN("SENDTO failed (%s)", strerror(errno));
	}

	return true;
}

static bool rtp_request_resend(seq_t first, seq_t last) {
	unsigned char req[8];    // *not* a standard RTCP NACK

	// do not request silly ranges (happens in case of network large blackouts)
	if (seq_order(last, first) || last - first > BUFFER_FRAMES / 2) return false;

	LOG_DEBUG("resend request [W:%u R:%u first=%u last=%u]", ab_write, ab_read, first, last);

	req[0] = 0x80;
	req[1] = 0x55|0x80;  // Apple 'resend'
	*(u16_t*)(req+2) = htons(1);  // our seqnum
	*(u16_t*)(req+4) = htons(first);  // missed seqnum
	*(u16_t*)(req+6) = htons(last-first+1);  // count

	rtp_host.sin_port = htons(rtp_sockets[CONTROL].port);

	if (sizeof(req) != sendto(rtp_sockets[CONTROL].sock, req, sizeof(req), 0, (struct sockaddr*) &rtp_host, sizeof(rtp_host))) {
		LOG_WARN("SENDTO failed (%s)", strerror(errno));
	}

	return true;
}

static int bind_socket(int *port, int mode)
{
	int sock;
	socklen_t len = sizeof(struct sockaddr);
	struct sockaddr_in addr;

	if ((sock = socket(AF_INET, mode, 0)) < 0) {
		LOG_ERROR("cannot create socket %d", sock);
		return sock;
	}

	/*  Populate socket address structure  */
	memset(&addr, 0, sizeof(addr));
	addr.sin_family      = AF_INET;
	addr.sin_addr.s_addr = htonl(INADDR_ANY);
	addr.sin_port        = htons(*port);
#ifdef SIN_LEN
	si.sin_len = sizeof(si);
#endif

	if (bind(sock, (struct sockaddr*) &addr, sizeof(addr)) < 0) {
		LOG_ERROR("cannot bind socket %d", sock);
		return -1;
	}

	if (!*port) {
		getsockname(sock, (struct sockaddr *) &addr, &len);
		*port = ntohs(addr.sin_port);
	}

	LOG_DEBUG("socket binding %d on port %d", sock, *port);

	return sock;
}

static bool init_rtp(void) {
	bool rc = true;
	pthread_t rtp_thread;
	int i;
	int ports[3] = {0, 0, 0};

	for (i = 0; i < 3; i++) {
		rtp_sockets[i].sock = bind_socket(&ports[i], SOCK_DGRAM);
		rc &= rtp_sockets[i].sock > 0;
	}

	_printf("port: %d\n", ports[DATA]); // let our handler know where we end up listening
	_printf("cport: %d\n", ports[CONTROL]);
	_printf("tport: %d\n", ports[TIMING]);

	_fprintf(stderr, "shairport_helper: VERSION: %s\n", version);
	pthread_create(&rtp_thread, NULL, rtp_thread_func, (void *)rtp_sockets);

	return rc;
}

static bool init_http(void)
{
	int port = 0;

	if ((http_listener = bind_socket(&port, SOCK_STREAM)) < 0) return false;

	if (listen(http_listener, 1) < 0) return false;

	_printf("hport: %d\n", port);

	return true;
}

// get the next frame, when available. return 0 if underrun/stream reset.
static short *buffer_get_frame(void) {
	short buf_fill;
	abuf_t *curframe = 0;
	int i;
	u32_t now, playtime;
	static int count = 0;

	pthread_mutex_lock(&ab_mutex);

	buf_fill = ab_write - ab_read;

	if (buf_fill >= BUFFER_FRAMES) {
		LOG_ERROR("Buffer overrun %u", buf_fill);
		ab_read = ab_write - 64;
	}

	now = gettime_ms();
	curframe = audio_buffer + BUFIDX(ab_read);

	/*
	  Last RTP sync might have happen recently and buffer frames have an RTP
	  older than sync.rtp, so difference will be negative, need to treat that
	  as a signed number. This works even in case of 32 bits rollover
	*/
	playtime = synchro.time + (((s32_t)(curframe->rtptime - synchro.rtp))*1000)/44100;

	if (!playing || !buf_fill || synchro.status != (RTP_SYNC | NTP_SYNC) || now < playtime) {
		LOG_SDEBUG("waiting (fill:%u, W:%u R:%u) now:%u, playtime:%u, wait:%d", buf_fill - 1, ab_write, ab_read, now, playtime, playtime - now);
		pthread_mutex_unlock(&ab_mutex);
		return NULL;
	}

	if (!(count++ & 0x1ff)) {
		LOG_INFO("buffer drain status [level:%u] [W:%u R:%u]", buf_fill, ab_write, ab_read);
	}

	// each missing packet will be requested up to (latency_frames / 16) times
	for (i = 16; seq_order(ab_read + i, ab_write); i += 16) {
		if (!audio_buffer[BUFIDX(ab_read + i)].ready) rtp_request_resend(ab_read + i, ab_read + i);
	}

	if (!curframe->ready) {
		LOG_DEBUG("created zero frame (fill:%u,  W:%u R:%u)", buf_fill - 1, ab_write, ab_read);
		memset(curframe->data, 0, FRAME_BYTES);
	}
	else {
		LOG_SDEBUG("prepared frame (fill:%u, W:%u R:%u)", buf_fill - 1, ab_write, ab_read);
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

  if (p == NULL)  {
	*stringp = NULL;
  } else {
	*p = '\0';
	*stringp = p + 1;
  }

  return start;
}
#endif

int conn_socket(int port)
{
	struct sockaddr_in addr;
	int sd;

	sd = socket(AF_INET, SOCK_STREAM, 0);
	// set_nonblock(ctx->cli_sock);
	addr.sin_family = AF_INET;
	addr.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
	addr.sin_port = htons(port);

	if (sd < 0 || connect(sd, (struct sockaddr *) &addr, sizeof(addr)) < 0) {
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


