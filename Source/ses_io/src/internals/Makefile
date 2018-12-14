
#  This is the makefile for ses_io/src/internals
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

all : libint.a


libint.a : $(OBJ) binary/libbin2.a ascii/libasc2.a xml/libxml2.a llnl_xml/libllnlxml.a json_parser/libparse.a
	$(ARCHIVE) $(ARFLAGS) libint.a $(OBJ) binary/*.o ascii/*.o xml/*.o llnl_xml/*.o json_parser/*.o


binary/libbin2.a :
	$(MAKE) -C binary clean
	$(MAKE) -C binary $(FLAGS)

ascii/libasc2.a :
	$(MAKE) -C ascii clean
	$(MAKE) -C ascii $(FLAGS)

xml/libxml2.a :
	$(MAKE) -C xml clean
	$(MAKE) -C xml $(FLAGS)

llnl_xml/libllnlxml.a :
	$(MAKE) -C llnl_xml clean
	$(MAKE) -C llnl_xml $(FLAGS)

json_parser/libparse.a :
	$(MAKE) -C json_parser clean
	$(MAKE) -C json_parser $(FLAGS)

.c.o:
	$(CC) $(CFLAGS) -o $@ $<

clean :
	rm -f libint.a *.o *~
	$(MAKE) -C xml clean
	$(MAKE) -C binary clean
	$(MAKE) -C  ascii clean
	$(MAKE) -C llnl_xml clean
	$(MAKE) -C json_parser clean
