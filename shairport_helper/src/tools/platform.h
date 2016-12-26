/*
 *  platform setting definition
 *
 *  (c) Philippe, philippe_44@outlook.com
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 */

#ifndef __PLATFORM_H
#define __PLATFORM_H

#if defined(linux)
#define LINUX     1
#define OSX       0
#define WIN       0
#define FREEBSD   0
#define SUNOS     0
#elif defined (__APPLE__)
#define LINUX     0
#define OSX       1
#define WIN       0
#define FREEBSD   0
#define SUNOS     0
#elif defined (_MSC_VER) || defined(__BORLANDC__)
#define LINUX     0
#define OSX       0
#define WIN       1
#define FREEBSD   0
#define SUNOS     0
#elif defined(__FreeBSD__)
#define LINUX     0
#define OSX       0
#define WIN       0
#define FREEBSD   1
#define SUNOS     0
#elif defined(sun)
#define LINUX     0
#define OSX       0
#define WIN       0
#define FREEBSD   0
#define SUNOS     1
#else
#error unknown target
#endif

#include <stdbool.h>
#include <signal.h>
#include <sys/stat.h>

#if LINUX || OSX || FREEBSD
#include <sys/types.h>
#include <unistd.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <sys/time.h>
#include <sys/socket.h>
#include <netdb.h>
#include <sys/poll.h>
#include <poll.h>
#include <dlfcn.h>
#include <pthread.h>
#include <errno.h>
#include <memcheck.h>

#define min(a,b) (((a) < (b)) ? (a) : (b))
#define max(a,b) (((a) > (b)) ? (a) : (b))

typedef u_int8_t  u8_t;
typedef u_int16_t u16_t;
typedef u_int32_t u32_t;
typedef u_int64_t u64_t;
typedef int16_t   s16_t;
typedef int32_t   s32_t;
typedef int64_t   s64_t;

#define last_error() errno
#define ERROR_WOULDBLOCK EWOULDBLOCK

int SendARP(in_addr_t src, in_addr_t dst, u8_t mac[], u32_t *size);
#define fresize(f,s) ftruncate(fileno(f), s)
char *strlwr(char *str);
#define _random(x) random()
char *GetTempPath(u16_t size, char *path);

#endif

#if SUNOS
#include <sys/types.h>
#include <unistd.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <sys/time.h>
#include <sys/socket.h>
#include <netdb.h>
#include <sys/poll.h>
#include <poll.h>
#include <dlfcn.h>
#include <pthread.h>
#include <errno.h>

#define min(a,b) (((a) < (b)) ? (a) : (b))
#define max(a,b) (((a) > (b)) ? (a) : (b))

typedef uint8_t  u8_t;
typedef uint16_t u16_t;
typedef uint32_t u32_t;
typedef uint64_t u64_t;
typedef int16_t   s16_t;
typedef int32_t   s32_t;
typedef int64_t   s64_t;

#define last_error() errno
#define ERROR_WOULDBLOCK EWOULDBLOCK

int SendARP(in_addr_t src, in_addr_t dst, u8_t mac[], u32_t *size);
#define fresize(f,s) ftruncate(fileno(f), s)
char *strlwr(char *str);
#define _random(x) random()
char *GetTempPath(u16_t size, char *path);

#endif


#if WIN

#include <winsock2.h>
#include <ws2tcpip.h>
#include <io.h>
#include <sys/timeb.h>

typedef unsigned __int8  u8_t;
typedef unsigned __int16 u16_t;
typedef unsigned __int32 u32_t;
typedef unsigned __int64 u64_t;
typedef __int16 s16_t;
typedef __int32 s32_t;
typedef __int64 s64_t;

#define inline __inline

int gettimeofday(struct timeval *tv, struct timezone *tz);

//#define poll(fds,numfds,timeout) WSAPoll(fds,numfds,timeout)
#define usleep(x) Sleep((x)/1000)
#define sleep(x) Sleep((x)*1000)
#define last_error() WSAGetLastError()
#define ERROR_WOULDBLOCK WSAEWOULDBLOCK
#define open _open
#define read _read
#define snprintf _snprintf
#define fresize(f, s) chsize(fileno(f), s)
#define strcasecmp stricmp
#define _random(x) random(x)
#define VALGRIND_MAKE_MEM_DEFINED(x,y)

#define in_addr_t u32_t
#define socklen_t int
#define ssize_t int

#define RTLD_NOW 0

#endif

#if LINUX || FREEBSD || OSX
typedef u8_t  __u8;
typedef u16_t __u16;
typedef u32_t __u32;
typedef u64_t __u64;
typedef s16_t __s16;
typedef s32_t __s32;
typedef s64_t __s64;

typedef struct ntp_s {
	__u32 seconds;
	__u32 fraction;
} ntp_t;

u64_t timeval_to_ntp(struct timeval tv, struct ntp_s *ntp);
u64_t get_ntp(struct ntp_s *ntp);
u32_t gettime_ms(void);
u64_t gettime_ms64(void);

#define SL_LITTLE_ENDIAN (__BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__)
#elif SUNOS
typedef u8_t  __u8;
typedef u16_t __u16;
typedef u32_t __u32;
typedef u64_t __u64;
typedef s16_t __s16;
typedef s32_t __s32;
typedef s64_t __s64;

typedef struct ntp_s {
        __u32 seconds;
        __u32 fraction;
} ntp_t;

u64_t timeval_to_ntp(struct timeval tv, struct ntp_s *ntp);
u64_t get_ntp(struct ntp_s *ntp);
u32_t gettime_ms(void);
u64_t gettime_ms64(void);

#define SL_LITTLE_ENDIAN (__BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__)
#endif

#endif     // __PLATFORM
