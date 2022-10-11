ifeq ($(CC),cc)
CC=$(lastword $(subst /, ,$(shell readlink -f `which cc`)))
endif

PLATFORM ?= $(firstword $(subst -, ,$(CC)))
HOST ?= $(word 2, $(subst -, ,$(CC)))


DEFINES = -DHAIRTUNES_STANDALONE -DHTTP_PARSER_STRICT=0 
CFLAGS  += -Wall -O2 -fPIC $(DEFINES) -fdata-sections -ffunction-sections 
LDFLAGS += -lm -lpthread -static

CFLAGS += $(shell pkg-config --cflags ao)
LDFLAGS += $(shell pkg-config --libs ao)

EXECUTABLE = shairport_helper
SOURCES = alac.c http.c hairtunes.c
OBJECTS = $(SOURCES:.c=.o)

ifeq ($(shell uname),FreeBSD)
	LDFLAGS += -lssl
else
	CFLAGS += $(shell pkg-config --cflags openssl)
	LDFLAGS += $(shell pkg-config --libs openssl)
endif

all: $(EXECUTABLE)

$(EXECUTABLE): $(OBJECTS)
	$(CC) $(OBJECTS) $(LIBRARY) $(LDFLAGS) -o $@
	strip $@

$(OBJECTS): $(DEPS)

.c.o:
	$(CC) $(CFLAGS) $(CPPFLAGS) $< -c -o $@

clean:
	-@rm -f $(OBJECTS) $(EXECUTABLE)

install: shairport_helper
	echo "please copy yourself. pick a reasable name and adapt Plugin.pm for auto detection"

.PHONY: all clean install
.SILENT: clean

