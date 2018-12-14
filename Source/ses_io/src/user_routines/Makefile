#  This is the makefile for ses_io/src/user_routines
_realpath = $(shell perl -e 'use Cwd "abs_path"; print abs_path(shift)," ";' $(1))
REL_ROOT := ../..
ROOT := $(strip $(call _realpath,$(strip $(shell pwd))/$(REL_ROOT)))

ifneq ($(wildcard ../../../Makefile.compile_opts),)
  include ../../../Makefile.compile_opts
else
  include ../../test_makefile/Makefile.compile_opts
endif
include ../../test_makefile/Makefile.local

#environment variables for the local file

SRC=$(wildcard *.c)
OBJ=$(SRC:.c=.o)

#targets

all:  libur.a


libur.a : $(OBJ)
	$(ARCHIVE) -$(ARFLAGS) libur.a $(OBJ)


.c.o:
	$(CC) $(CFLAGS) -o $@ $<

clean :
	rm -f libur.a; rm -f *.o; rm -f *~;
