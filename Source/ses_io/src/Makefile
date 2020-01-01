#  This is the makefile for ses_io/src
_realpath = $(shell perl -e 'use Cwd "abs_path"; print abs_path(shift)," ";' $(1))
REL_ROOT := ..
ROOT := $(strip $(call _realpath,$(strip $(shell pwd))/$(REL_ROOT)))

ifneq ($(wildcard ../Makefile.compile_opts),)
  include ../Makefile.compile_opts
else
  include ../test_makefile/Makefile.compile_opts
endif
include ../test_makefile/Makefile.local

#environment variables for the local file

#targets

all : libses.a wrappers

for_eospac : 
	$(MAKE) DEBUG="-DDEBUG_PRINT -DHEADER_EOS" all

wrappers : wrappers/fortran/libsesf.a wrappers/fortran/libsesw.a
	$(MAKE) -C wrappers/fortran $(FLAGS)
	$(MAKE) -C wrappers/java    $(FLAGS)

version :
	cd ../scripts; perl ./ses_io_version.pl; cd -;


libses.a :  user_interface/libui.a internals/libint.a user_routines/libur.a 
	ar -cr libses.a user_interface/*.o internals/*.o internals/binary/*.o internals/ascii/*.o internals/xml/*.o internals/llnl_xml/*.o internals/json_parser/*.o user_routines/*.o 

libeos.a :  libses.a   
	gmake clean; gmake for_eospac;ar -cr libeos.a user_interface/*.o internals/*.o internals/binary/*.o internals/ascii/*.o internals/xml/*.o internals/llnl_xml/*.o internals/json_parser/*.o user_routines/*.o 

user_routines/libur.a :
	$(MAKE) -C user_routines clean
	$(MAKE) -C user_routines $(FLAGS)

user_interface/libui.a :
	$(MAKE) -C user_interface clean
	$(MAKE) -C user_interface $(FLAGS)

internals/libint.a :
	$(MAKE) -C internals clean
	$(MAKE) -C internals $(FLAGS)

wrappers/fortran/libsesf.a :
	$(MAKE) -C wrappers/fortran $(FLAGS) libsesf.a

wrappers/fortran/libsesw.a :
	$(MAKE) -C wrappers/fortran $(FLAGS) libsesw.a

# Hack: to get it to work on Linux & Mac.
wrappers/java/libsesj.so :
	$(MAKE) -C wrappers/java $(FLAGS) libsesj.so

wrappers/java/libsesj.dylib :
	$(MAKE) -C wrappers/java $(FLAGS) libsesj.dylib

clean :
	rm -f libses.a; rm -f *.o; rm -f *~
	$(MAKE) -C internals clean
	$(MAKE) -C user_interface clean
	$(MAKE) -C user_routines clean
	$(MAKE) -C wrappers/fortran clean
	$(MAKE) -C wrappers/java clean
