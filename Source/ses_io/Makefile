#  This is the Makefile that comes with the ses_io library tar
#  put prefix and exec_prefix here if they DO NOT exist

#  create the libses.a in the src directory (after the untar)
_realpath = $(shell perl -e 'use Cwd "abs_path"; print abs_path(shift)," ";' $(1))
REL_ROOT := 
ROOT := $(strip $(call _realpath,$(strip $(shell pwd))/$(REL_ROOT)))

ifneq ($(wildcard ../Makefile.compile_opts),)
  include ../Makefile.compile_opts
else
  include test_makefile/Makefile.compile_opts
endif
include test_makefile/Makefile.local

prefix=install
exec_prefix=install

for_eospac:
	$(MAKE) -DHEADER_EOS all

all:
	$(MAKE) -C src

debug:
	$(MAKE) -C src debug

#  perform the tests
PATHS= $(C_TEST_PATHS) \
       $(F90_TEST_PATHS) \
       $(JAVA_TEST_PATHS)

C_TEST_PATHS=test/ascii_tests \
      test/routine_tests/internal_tests \
      test/routine_tests/ses_close_tests/src \
      test/routine_tests/ses_open_tests/src \
      test/routine_tests/user_interface_tests \
      test/use_case_tests/src \
      test/user_manual/C_tests
F90_TEST_PATHS=test/wrapping_tests/fortran \
      test/user_manual/f90_tests
JAVA_TEST_PATHS=test/user_manual/java_tests

check test: all
	@(for d in $(PATHS); do \
            $(MAKE) -C $$d test; \
          done; \
         )
	$(MAKE) compare

compare:
	@echo '-------------------------------------------------------------------'
	@(for d in $(PATHS); do \
	    $(MAKE) -C $$d compare > $$d/outer; \
            grep PASSED $$d/outer; \
          done; \
         )
	@echo '-------------------------------------------------------------------'

#  move libses.a to where it will be installed, and ses_defines.h to where the user will expect *.h files
install:
	mkdir $(prefix);mkdir $(prefix)/lib;mkdir $(prefix)/include;mkdir $(prefix)/MySesIO; cp src/libses.a $(prefix)/lib;cp include/ses_defines.h $(prefix)/include;cp src/wrappers/fortran/libsesf.a $(prefix)/lib;cp src/wrappers/java/libsesj.so $(prefix)/lib; cp src/wrappers/java/libsesj.dylib $(prefix)/lib;cp src/wrappers/fortran/libsesw.a $(prefix)/lib;cp include/ses_defines_f90.h $(prefix)/include;cp src/wrappers/java/MySesIO/SesIO.java $(prefix)/MySesIO; cp -r src/wrappers/java/ses_defines $(prefix)/MySesIO;

uninstall:
	rm $(prefix)/lib/libses.a; rm $(prefix)/lib/libsesf.a; rm $(prefix)/lib/libsesw.a; rm $(prefix)/include/ses_defines.h;rm $(prefix)/include/ses_defines_f90.h;


clean:
	rm -f libses.a; rm -f ses_io*.tar; rm -f *~; rm -r src; rm -r include; rm -r test; rm -r test_makefile; rm -r test_files;
	find . -name '*.a' -exec rm -f {} \;
	find . -name '*.o' -exec rm -f {} \;
