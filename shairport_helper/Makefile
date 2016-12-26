BASE		= ..

TOOLS		= ./src/tools
VALGRIND	= ../valgrind
SRC 		= ./src
LIBRARY 	=
DEFINES 	= -DHAIRTUNES_STANDALONE -DHTTP_PARSER_STRICT=0 -D_POSIX_PTHREAD_SEMANTICS -D_REENTRANT -DMDNS_SVC -DNDEBUG

TINYMDNS	= $(BASE)/tinysvcmdns

vpath %.c $(TOOLS):$(SRC):$(TINYMDNS)

INCLUDE = -I. \
		  -I$(VALGRIND)/memcheck -I$(VALGRIND)/include \
		  -I$(TINYMDNS) \
		  -I$(SRC) \
		  -I$(TOOLS)

SOURCES = alac.c hairtunes.c http.c \
		  mdns.c mdnsd.c tinysvcmdns.c \
		  log_util.c
      
OBJECTS = $(patsubst %.c,$(OBJ)/%.o,$(SOURCES)) 

all: $(EXECUTABLE)

$(EXECUTABLE): $(OBJECTS)
	$(CC) $(OBJECTS) $(LIBRARY) $(LDFLAGS) -o $@

$(OBJECTS): | bin $(OBJ)

$(OBJ):
	@mkdir -p $@
	
bin:	
	@mkdir -p bin

$(OBJ)/%.o : %.c
	$(CC) $(CFLAGS) $(CPPFLAGS) $(INCLUDE) $< -c -o $@
	
clean:
	rm -f $(OBJECTS) $(EXECUTABLE) 

