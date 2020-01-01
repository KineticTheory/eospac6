#  This is the makefile for ses_io/src/wrappers/fortran
_realpath = $(shell perl -e 'use Cwd "abs_path"; print abs_path(shift)," ";' $(1))
REL_ROOT := ../../..
ROOT := $(strip $(call _realpath,$(strip $(shell pwd))/$(REL_ROOT)))


ifneq ($(wildcard ../../../../Makefile.compile_opts),)
  include ../../../../Makefile.compile_opts
else
  include ../../../test_makefile/Makefile.compile_opts
endif
include ../../../test_makefile/Makefile.local

#environment variables for the local file

LOCAL_CFLAGS=-D LC_UNDER
LIBS=-lsesf -lm
FSRC=ses_io_module.f90
FOBJ=ses_io_module.o

SRC=$(wildcard *.c)
OBJ=$(SRC:.c=.o)

#targets

all : libsesw.a libsesf.a
	echo "F90FLAGS is $(F90FLAGS)";
	echo "DEBUG_BITFLAG is $(DEBUG_BITFLAG)";
	echo "ARCH IS $(ARCH)";


libsesf.a : $(FOBJ)
	$(ARCHIVE) -$(ARFLAGS) libsesf.a $(FOBJ)
libsesw.a : $(OBJ)
	$(ARCHIVE) -$(ARFLAGS) libsesw.a $(OBJ)

ses_io_module.o : ses_io_module.f90
	$(F90) $(F90FLAGS) -I$(IPATH1) -I$(IPATH2) -o ses_io_module.o $(FSRC) 

.c.o:
	$(CC) $(LOCAL_CFLAGS) $(CFLAGS) $<


clean:  
	rm -f lib*; rm -f *.o; rm *~;rm *.mod;


