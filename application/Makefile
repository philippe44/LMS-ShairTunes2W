ifeq ($(CC),cc)
CC=$(lastword $(subst /, ,$(shell readlink -f `which cc`)))
endif

ifeq ($(findstring gcc,$(CC)),gcc)
CFLAGS  += -Wno-deprecated-declarations -Wno-format-truncation -Wno-stringop-truncation 
LDFLAGS += -s
else
CFLAGS += -fno-temp-file
endif

PLATFORM ?= $(firstword $(subst -, ,$(CC)))
HOST ?= $(word 2, $(subst -, ,$(CC)))

ifneq ($(HOST),macos)
ifneq ($(HOST),solaris)
LINKSTATIC = -static -latomic
else
LDFLAGS += -lssp
endif
endif

CORE       = bin/shairport_helper-$(HOST)
BUILDDIR   = $(dir $(CORE))$(HOST)/$(PLATFORM)
EXECUTABLE = $(CORE)-$(PLATFORM)
EXECUTABLE_STATIC = $(EXECUTABLE)-static

SRC 		= src
TOOLS		= crosstools/src
RAOP		= libraop/targets
MDNS		= libmdns/targets
CODECS		= libcodecs/targets
OPENSSL		= libopenssl/targets/$(HOST)/$(PLATFORM)
#VALGRIND	= ../valgrind

DEFINES 	= -DHTTP_PARSER_STRICT=0 -D_GNU_SOURCE
CFLAGS  	+= -Wall -fPIC -ggdb -O2 $(DEFINES) -fdata-sections -ffunction-sections 
LDFLAGS 	+= -lpthread -ldl -lm -L. 

vpath %.c $(TOOLS):$(SRC)

INCLUDE = -I$(CODECS)/include/flac -I$(CODECS)/include/shine \
		  -I$(TOOLS) \
		  -I$(RAOP)/include \
		  -I$(MDNS)/include/mdnssvc \
		  -I$(OPENSSL)/include

SOURCES = main.c \
		  cross_util.c cross_log.c cross_net.c cross_thread.c cross_ssl.c platform.c 
		
OBJECTS = $(patsubst %.c,$(BUILDDIR)/%.o,$(SOURCES)) 
LIBRARY = $(RAOP)/$(HOST)/$(PLATFORM)/libraop.a $(CODECS)/$(HOST)/$(PLATFORM)/libcodecs.a \
		  $(MDNS)/$(HOST)/$(PLATFORM)/libmdns.a
LIBRARY_STATIC = $(LIBRARY) $(OPENSSL)/libopenssl.a          

all: directory $(EXECUTABLE) $(EXECUTABLE_STATIC)

$(EXECUTABLE): $(OBJECTS)
	$(CC) $(OBJECTS) $(LIBRARY) $(CFLAGS) $(LDFLAGS) -o $@
ifeq ($(HOST),macos)
	rm -f $(CORE)
	lipo -create -output $(CORE) $$(ls $(CORE)* | grep -v '\-static')
endif

$(EXECUTABLE_STATIC): $(OBJECTS)
	$(CC) $(OBJECTS) $(LIBRARY_STATIC) $(CFLAGS) $(LDFLAGS) $(LINKSTATIC) -o $@
ifeq ($(HOST),macos)
	rm -f $(CORE)-static
	lipo -create -output $(CORE)-static $(CORE)-*-static
endif	

$(OBJECTS) : $(DEPS)

directory:
	@mkdir -p $(BUILDDIR)

$(BUILDDIR)/%.o : %.c
	$(CC) $(CFLAGS) $(CPPFLAGS) $(INCLUDE) $< -c -o $@

clean:
	rm -f $(OBJECTS) $(EXECUTABLE) $(EXECUTABLE_STATIC) $(CORE) $(CORE)-static 
