/*
 *  (c) Philippe, philippe_44@outlook.com
 *
 */

#include <stdarg.h>

#include "platform.h"
#include "cross_util.h"
#include "util.h"

/*----------------------------------------------------------------------------*/
// cmdline parsing
char *next_param(char *src, char c) {
	static char *str = NULL;
	char *ptr, *ret;
	if (src) str = src;
	if (str && (ptr = strchr(str, c))) {
		ret = str;
		*ptr = '\0';
		str = ptr + 1;
	} else {
		ret = str;
		str = NULL;
	}

	return ret && ret[0] ? ret : NULL;
}

/*--------------------------------------------------------------------------*/
void free_metadata(struct metadata_s *metadata)
{
	NFREE(metadata->artist);
	NFREE(metadata->album);
	NFREE(metadata->title);
	NFREE(metadata->genre);
	NFREE(metadata->path);
	NFREE(metadata->artwork);
	NFREE(metadata->remote_title);
}

/*--------------------------------------------------------------------------*/

void dup_metadata(struct metadata_s *dst, struct metadata_s *src)

{
	free_metadata(dst);
	if (src->artist) dst->artist = strdup(src->artist);
	if (src->album) dst->album = strdup(src->album);
	if (src->title) dst->title = strdup(src->title);
	if (src->genre) dst->genre = strdup(src->genre);
	if (src->path) dst->path = strdup(src->path);
	if (src->artwork) dst->artwork = strdup(src->artwork);
	if (src->remote_title) dst->remote_title = strdup(src->remote_title);
	dst->duration = src->duration;
	dst->track = src->track;
	dst->track_hash = src->track_hash;

}
