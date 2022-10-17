/*
 *  Misc utilities
 *
 */

#pragma once

#include <stdint.h>

typedef struct metadata_s {
	char *artist;
	char *album;
	char *title;
	char *genre;
	char *path;
	char *artwork;
	char *remote_title;
	uint32_t track;
	uint32_t duration;
	uint32_t track_hash;
	uint32_t sample_rate;
	uint8_t  sample_size;
	uint8_t  channels;
} metadata_t;

void 		free_metadata(struct metadata_s *metadata);
void 		dup_metadata(struct metadata_s *dst, struct metadata_s *src);
