SHELL=/bin/sh
# sources, headers, objects, library and etc.
SRCS_C  = dec_jpeg2000.c dec_png.c enc_jpeg2000.c \
          enc_png.c mova2i.c
SRCS_f  = $(wildcard *.f)
SRCS_m  = gridtemplates.f pdstemplates.f \
          drstemplates.f gribmod.f realloc.f \
          intmath.f
SRCS_F  = $(SRCS_m) $(filter-out $(SRCS_m), $(SRCS_f))
OBJS    = $(patsubst %.c, %.o, $(SRCS_C)) \
          $(patsubst %.f, %.o, $(SRCS_F))

all: build

build: $(LIB)

$(LIB): $(OBJS)
	$(AR) $(ARFLAGS) $@ $^

debug: dbg_build

dbg_build: override CFLAGS += $(DEBUG)
dbg_build: override FFLAGS += $(DEBUG)
dbg_build: $(LIB)

%.o: %.c
	$(CC) -c $(CFLAGS) $(CFLAGSDEFS) $<

%.o: %.f
	$(FC) -c $(FFLAGS) $(FFLAGSDEFS) $<

message: $(MSGSRC)
	$(CC) -c $(firstword $(MSGSRC))
	$(FC) -c $(lastword $(MSGSRC))
	$(AR) $(ARFLAGS) $(LIB) $(patsubst %.c, %.o, $(firstword $(MSGSRC)))
	$(AR) $(ARFLAGS) $(LIB) $(patsubst %.f, %.o, $(lastword $(MSGSRC)))
	$(RM) $(patsubst %.c, %.o, $(firstword $(MSGSRC)))
	$(RM) $(patsubst %.f, %.o, $(lastword $(MSGSRC)))
	$(RM) $(MSGSRC)

install:
	[[ -z "$(LIB)" || -z "$(LIB_DIR)" ]] || mv $(LIB) $(LIB_DIR)
	[[ -z "$(MOD)" || -z "$(INC_DIR)" ]] || mv $(MOD) $(INC_DIR)
	[[ -z "$(SRC_DIR)" ]] || cp ./* $(SRC_DIR)

test:
	@echo "*** unit test unavailable currently ***"

clean:
	$(RM) $(OBJS) $(LIB)
	[[ -z "$(MOD)" ]] || $(RM) -r $(MOD)

.SUFFIXES: .f .mod .c .o .a

# Specify targets that do not generate filesystem objects
.PHONY: all build install debug dbg_build message test clean
