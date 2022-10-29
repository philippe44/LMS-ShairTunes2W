ifeq ($(CC),cc)
CC=$(lastword $(subst /, ,$(shell readlink -f `which cc`)))
endif

PLATFORM ?= $(firstword $(subst -, ,$(CC)))
HOST ?= $(word 2, $(subst -, ,$(CC)))

ifneq ($(HOST),macos)
ifneq ($(HOST),solaris)
LDFLAGS += -static
else
LDFLAGS += -lssp
endif
endif

BUILDDIR	= bin/$(HOST)/$(PLATFORM)
BIN		 	= bin/shairport_helper-$(HOST)-$(PLATFORM)

SRC 		= src
TOOLS		= crosstools/src
RAOP		= libraop/targets
#VALGRIND	= ../valgrind
MDNS		= libmdns/targets
CODECS		= libcodecs/targets
OPENSSL		= libopenssl/targets/$(HOST)/$(PLATFORM)

DEFINES 	= -DHTTP_PARSER_STRICT=0 -D_GNU_SOURCE
CFLAGS  	+= -Wall -Wno-deprecated-declarations -Wno-format-truncation -Wno-stringop-truncation -fPIC -ggdb -O2 $(DEFINES) -fdata-sections -ffunction-sections 
LDFLAGS 	+= -s -lpthread -ldl -lm -L. 

vpath %.c $(TOOLS):$(SRC)

INCLUDE = -I$(CODECS)/include/flac -I$(CODECS)/include/shine \
		  -I$(TOOLS) \
		  -I$(RAOP)/include \
		  -I$(MDNS)/include/mdnssvc \
		  -I$(OPENSSL)/include

SOURCES = main.c \
		  cross_util.c cross_log.c cross_net.c cross_thread.c platform.c 
		
OBJECTS = $(patsubst %.c,$(BUILDDIR)/%.o,$(SOURCES)) 
LIBRARY = $(RAOP)/$(HOST)/$(PLATFORM)/libraop.a $(CODECS)/$(HOST)/$(PLATFORM)/libcodecs.a \
		  $(MDNS)/$(HOST)/$(PLATFORM)/libmdns.a $(OPENSSL)/libopenssl.a

all: directory $(BIN)

$(BIN): $(OBJECTS)
	$(CC) $(OBJECTS) $(LIBRARY) $(CFLAGS) $(LDFLAGS) -o $@ 

$(OBJECTS) $(OBJECTS_STATIC): $(DEPS)

directory:
	@mkdir -p $(BUILDDIR)

$(BUILDDIR)/%.o : %.c
	$(CC) $(CFLAGS) $(CPPFLAGS) $(INCLUDE) $< -c -o $@

clean:
	rm -f $(OBJECTS) $(BIN)
