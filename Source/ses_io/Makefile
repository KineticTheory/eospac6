#  This is the top level makefile for ses_io
_realpath = $(shell perl -e 'use Cwd "abs_path"; print abs_path(shift)," ";' $(1))
REL_ROOT :=.
ROOT := $(strip $(call _realpath,$(strip $(shell pwd))/$(REL_ROOT)))

ifneq ($(wildcard ../Makefile.compile_opts),)
  include ../Makefile.compile_opts
else
  include test_makefile/Makefile.compile_opts
endif

include test_makefile/Makefile.local

#  environment variables for the local file

my_install=install

#VERSION=VB0.5_02_29_2012
#VERSION=VB0.6_11_21_2012
#VERSION=VB0.7_02_11_2013
#VERSION=VB0.8_03_07_2013
#VERSION=VB0.9_04_18_2013
#VERSION=VB0.10_04_23_2013
#VERSION=VB0.11_06_02_2013
#VERSION=VB0.12_01_15_2014
#VERSION=VP1.0_05_06_2014
#VERSION=VP1.0_05_21_2014_PATCH
#VERSION=VP1.0_07_09_2014
#VERSION=VP1.2_07_16_2014
#VERSION=VP1.3_11_25_2014
#VERSION=VP1.4_12_02_2014
#VERSION=VP1.5_12_03_2014
#VERSION=VP1.6_12_04_2014
#VERSION=VP1.7_05_12_2015
#VERSION=VP1.8_06_09_2015
#VERSION=VP1.9_10_13_2015
#VERSION=VP1.10_12_24_2015
VERSION=VP2.0_03_22_2016

USER_MANUAL=SESIO_UM_V7.pdf
DIST_DIRS = src test include test_makefile test_files
C_TEST_PATHS=test/ascii_tests \
      test/binary_tests \
      test/routine_tests/internal_tests \
      test/routine_tests/ses_close_tests/src \
      test/routine_tests/ses_open_tests/src \
      test/routine_tests/user_interface_tests \
      test/use_case_tests/src \
      test/user_manual/C_tests
F90_TEST_PATHS=test/wrapping_tests/fortran \
      test/user_manual/f90_tests
JAVA_TEST_PATHS=test/user_manual/java_tests

PATHS= $(C_TEST_PATHS) \
       $(F90_TEST_PATHS) \
       $(JAVA_TEST_PATHS)

#targets

all:
	$(MAKE) -C src  $(FLAGS)
	$(MAKE) -C test $(FLAGS)


for_eospac:
	$(MAKE) DEBUG="-DHEADER_EOS" all

check test: all
	@(for d in $(PATHS); do \
            $(MAKE) -C $$d test; \
          done; \
         )
	$(MAKE) compare

valgrind: all
	@(for d in $(PATHS); do \
            $(MAKE) -C $$d valgrind; \
          done; \
         )
	$(MAKE) compare


compare:
	@echo '-----------------------------------------------------------------------------'
	@(for d in $(PATHS); do \
	    $(MAKE) -C $$d compare > $$d/outer; \
            grep PASSED $$d/outer; \
          done; \
         )
	@echo '-----------------------------------------------------------------------------'

#  move libses.a to where it will be installed, and ses_defines.h to where the user will expect *.h files
install:
	mkdir $(my_install);mkdir $(my_install)/lib; mkdir $(my_install)/mods; mkdir $(my_install)/include;cp src/libses.a $(my_install)/lib;cp include/ses_defines.h $(my_install)/include;cp src/wrappers/fortran/libsesf.a $(my_install)/lib;cp src/wrappers/fortran/libsesw.a $(my_install)/lib;cp include/ses_defines_f90.h $(my_install)/include; cp src/wrappers/fortran/ses_io.mod $(my_install)/mods;

uninstall:
	rm $(my_install)/lib/libses.a; rm $(my_install)/include/ses_defines.h;


dist:
	$(MAKE) -C release distclean
	cp -r $(DIST_DIRS) release
	cd release; find $(DIST_DIRS) -name CVS -exec rm -rf {} \; ; cd -;
	ln -s release ses_io_$(VERSION)
	tar -cf ses_io_$(VERSION).tar $(addprefix ses_io_$(VERSION)/,Makefile README $(USER_MANUAL) $(DIST_DIRS))
	rm -f ses_io_$(VERSION)

clean:
	rm -f ses_io_$(VERSION)
	rm -f libses.a ses_io*.tar *~
	$(MAKE) -iC src clean
	$(MAKE) -iC test clean
	$(MAKE) -iC release clean
	find . -name '*.a' -exec rm -f {} \;
	find . -name '*.o' -exec rm -f {} \;
