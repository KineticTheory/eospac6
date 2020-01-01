##################################################################
#  
#  Copyright -- see file named COPYRIGHTNOTICE
#  
##################################################################
### Master Makefile.                                           ###
###                                                            ###
### Use 'make help' to get a list of valid targets and their   ###
### descriptions.                                              ###
###                                                            ###
##################################################################
SHELL = /bin/sh

include Makefile.compile_opts

# Define which variables need to be available to the general environment
export LIBRARY_AR

##################################################################
### Target definitions                                         ###
##################################################################
all: # Compile the library, all examples, and all test codes
	( $(MAKE) -C $(BASEDIR)/Examples $@ 2>&1 ) | tee $(BASEDIR)/$(BUILD_LOG)
	( $(MAKE) -C $(BASEDIR)/utils $@ 2>&1 ) | tee -a $(BASEDIR)/$(BUILD_LOG)
	( $(MAKE) -C $(BASEDIR)/tests $@ 2>&1 ) | tee -a $(BASEDIR)/$(BUILD_LOG)

lib: # Compile only the library
	( $(MAKE) -C $(BASEDIR)/src 2>&1 ) | tee $(BASEDIR)/$(BUILD_LOG)
comma := ,
space :=
space +=
cpuinfo_flags_table = $(shell $(foreach k,$(call keys,_F90-CPUINFO_COMP_FLAGS),echo "                      $(call split,-,$(k))" " ... " "$(call get,_F90-CPUINFO_COMP_FLAGS,$(k))";))
define HELP_ADDENDUM
Additionally, the following Makefile variables may be set on the command line:

  CPUFLAGS          Define optional performance enhancement options for specific CPU
                    instruction sets. Some defaults are defined in ./config/Makefile-*.hashes,
                    and they are used if the current CPU is detected to be in the supported list.
                    If cross-compiling is necessary, then using CPUFLAGS is likely required.
                    The following shows a list of SHORTCUT keys that the user may use to enable
                    associated default compiler options. For example, if CPUFLAGS=avx512pf for
                    ifort version 18, then the "$(call get,_F90-CPUINFO_COMP_FLAGS,ifort-18-avx512pf)"
                    options will be applied. Alternatively, the user may explicitly define
                    CPUFLAGS="$(call get,_F90-CPUINFO_COMP_FLAGS,ifort-18-avx512pf)". A mixture of
                    SHORTCUTs and options may be used (i.e., CPUFLAGS="avx512pf -xAVX").

  EXAMPLETARGETS    Define a specific list of test case Make targets to operate upon.
                    For example:
                      make EXAMPLETARGETS=test_task1606 check
                          builds, runs and checks for PASS/FAIL the test_task1606 code.

  memcheck=1        Use either valgrind or purify to perform memory tests.
                    If both valgrind and purify are found, then valgrind is used.

  NORUN             If this variable is set to any value other than "FALSE", then the tests
                    are not executed, merely compiled.

  NOVECT            Disable automatic vectorization. The CPUFLAGS will be defined as
                    -DDISABLE_OMP_SIMD_PRAGMA.

  prefix            Define an alternative installation directory.

  PURIFY=purify     Force the usage of purify for memory tests. If purify
                    is named something else on your system, then define it with this variable.

  SERIES            Define a specific series of like-named or categorized test case Make
                    targets to operate upon.
                    For example:
                      make SERIES=test_task1606 check
                          builds, runs and checks for PASS/FAIL the test_task1606,
                          test_task1606_2, test_task1606_3, test_task1606_4,
                          test_task1606_5, and test_task1606_6 codes.
                      make SERIES=quick check
                          builds, runs and checks for PASS/FAIL the codes in the 'quick'
                          group (see ingroup Doxygen tag in source files).
                      make SERIES=unit check
                          builds, runs and checks for PASS/FAIL the codes in the 'unit'
                          group (see ingroup Doxygen tag in source files).
                      make SERIES=cpp check
                          builds, runs and checks for PASS/FAIL all of the C++ codes
                          (likewise c, f and f90 for C, F77 and F90 respectively).

  TESTTARGETS       Define a specific list of test case Make targets to operate upon.
                    For example:
                      make TESTTARGETS=test_task1606 check
                          builds, runs and checks for PASS/FAIL the test_task1606 code.

  UTILTARGETS       Define a specific list of utils Make targets to operate upon.
                    For example:
                      make UTILTARGETS=get_sesame_data check
                          builds, runs and checks for PASS/FAIL the get_sesame_data code.

  VALGRIND=valgrind Force the usage of valgrind for memory tests. If valgrind
                    is named something else on your system, then define it with this variable.
endef
export HELP_ADDENDUM

help: # Print this help
	@(echo "$(BASEDIR)/Makefile contains the following valid targets:"; \
          perl $(BASEDIR)/scripts/display_targets $(BASEDIR)/Makefile; \
          echo 'Fortran compiler(s) supported on this architecture:'; \
          echo $(if $(F90_List),$(F90_List),$(F90)) | fmt -w 80 | sed -e 's:^:   :g'; \
          if [ "$(strip $(F90_Found_List))" != "" ]; then \
            echo ' '; \
            echo 'Supported Fortran compiler(s) found on this machine:'; \
            echo $(notdir $(F90_Found_List)) | fmt -w 80 | sed -e 's:^:   :g'; \
          fi; \
          if [ "$(strip $(X_Compile_Module_List))" != "" ]; then \
            echo ' '; \
            echo 'If one of the following modules is loaded on this machine, then cross-compilation will' \
                 'be performed, and the tests will be executed on the backend (assumed to be available)' \
                 'for appropriate Make targets:' | fmt -w 90; \
            echo $(X_Compile_Module_List) | awk '{for (i=1;i<=NF;i=i+3) {print $$(i),$$(i+1),$$(i+2)}}' | column -t -c 80 | sed -e 's:^:   :g'; \
          fi; \
          printf "\n%s\n\n" "$$HELP_ADDENDUM"; \
          (printf "%-22s %-10s %-10s %-10s %s\n%-22s %-10s %-10s %-10s %s\n" " " F90 VERSION SHORTCUT "COMPILER OPTIONS" " " "----------" "----------" "----------" "----------------"); \
          ($(foreach k,$(call keys,_F90-CPUINFO_COMP_FLAGS),printf "%-22s %-10s %-10s %-10s %s\n" " " $(call split,-,$(k)) "$(call get,_F90-CPUINFO_COMP_FLAGS,$(k))";)); \
          echo; \
         ) | more

EXAMPLEFILES := $(foreach ext,c f cpp f90, \
                  $(wildcard Examples/*.$(ext)))
TESTFILES    := $(foreach ext,c f cpp f90, \
                  $(wildcard tests/*.$(ext)))
UTILFILES    := $(foreach ext,c f cpp f90, \
                  $(wildcard utils/*.$(ext)))
EXAMPLETARGETS = $(basename $(notdir $(EXAMPLEFILES)))
TESTTARGETS    = $(basename $(notdir $(TESTFILES)))
UTILTARGETS    = $(basename $(notdir $(UTILFILES)))
ifneq ($(strip $(SERIES)),)
  EXAMPLETARGETS = 
  UTILTARGETS =
endif

check test: # Compile and execute the library, all examples, and all test codes
	$(MAKE) -j1 NODEP=TRUE UTILTARGETS=get_sesame_data utils
	@echo '***********************************************************'
  ifneq ($(strip $(TESTTARGETS)),)
	@$(MAKE) -C $(BASEDIR)/tests NODEP=TRUE EXE="$(TESTTARGETS)" $@
  else
	@$(MAKE) -C $(BASEDIR)/tests NODEP=TRUE $@
  endif

examples: # Execute all examples in the ./Examples subdirectory
	@$(MAKE) -C $(BASEDIR)/Examples check

Test%: # Compile the library, and the example named Test%, given %=C, CPP, F, or F77
	@$(MAKE) -C $(BASEDIR)/Examples $@

test%: # Compile the library, and the test code named test%, given % is either a 3-digit number like 001 or a bug tracker ID like _artf1234
	@$(MAKE) TESTTARGETS=$@ check

utils: # Compile the library, and all of the possible targets within the utils subdirectory
	@$(MAKE) -C $(BASEDIR)/utils all

$(UTILTARGETS): # Compile the library, and the specified target within the utils subdirectory
	@$(MAKE) -C $(BASEDIR)/utils $@

checkutils: # Compile the library, and compile/check all of the possible targets within the utils subdirectory
	@$(MAKE) -C $(BASEDIR)/utils check

check_on_compute_node: all # This is a specialized target that will cross-compile the tests if an appropriate module is loaded, and then execute the tests on the available compute node using MSUB
	@$(MAKE) -C $(BASEDIR)/tests check_on_compute_node

send_report: # Send and HTML status report to eospac_testing@lanl.gov
	@$(MAKE) -C $(BASEDIR)/tests send_report

compare: # Compare existing test code output files to the baseline
	@$(MAKE) -C $(BASEDIR)/tests compare

$(LIBRARY_AR): # Compile the library
	@$(MAKE) -C $(BASEDIR)/src

version: # Generate the src/eos_version.c file that contains the version information for the current source files (REQUIRES CVS repository access)
ifeq (0,${MAKELEVEL})
	@$(MAKE) -C $(BASEDIR)/src version
endif

install: # Install the library, examples and documentation for the current platform/compiler configuration
	@$(MAKE) -C $(BASEDIR)/Examples install

list_my_installed_files: # List the installed files for the current platform/compiler configuration
	@$(MAKE) -C $(BASEDIR)/Examples list_my_installed_files

list_installed_files: # List all of the installed files for the current network
	@$(MAKE) -C $(BASEDIR)/Examples list_installed_files

uninstall: # Uninstall the library, examples and documentation for the current platform/compiler configuration
	@$(MAKE) -C $(BASEDIR)/Examples uninstall

TAGS:
	@$(MAKE) -C $(BASEDIR)/Examples TAGS
	@$(MAKE) -C $(BASEDIR)/src TAGS
	@$(MAKE) -C $(BASEDIR)/tests TAGS

clean: # Delete the files created for the current platform/compiler configuration
	@$(MAKE) -C $(BASEDIR)/Examples clean
	-rm -fr $(BUILD_LOG)

cleanarch: # Delete the files created for the current platform (includes all compiler configurations)
	@echo ' '
	@echo '***********************************************************'
	@echo '***' Delete local build $(BASE_ARCH) directories.
	@echo '***********************************************************'
	-rm -fr $(BASE_EXAMPLEDIR) $(BASE_OBJDIR) $(BASE_LIBDIR) $(BASE_BINDIR) \
                $(BASE_INCDIR) $(BASE_CPPDIR) $(BASE_TESTEXEDIR) \
                `find . -name 'core*'` `find . -name '*.stackdump'`

distclean: # Delete files created for all platform/compiler configurations
	@$(MAKE) -C $(BASEDIR)/Examples distclean
	-rm -f $(BUILD_LOG)

distcleanfast:
	@$(MAKE) -C $(BASEDIR)/Examples distcleanfast
	-rm -f $(BUILD_LOG)

mostlyclean: # Delete files created for all platform/compiler configurations except those used to build $(LIBRARY_AR)
	@$(MAKE) -C $(BASEDIR)/Examples mostlyclean
	-rm -fr $(BUILD_LOG)

partialclean: # Delete test object and executable files created for current platform/compiler configuration
	@$(MAKE) -C $(BASEDIR)/tests partialclean
	-rm -fr $(BUILD_LOG)

doxygen: # Run Doxygen on the Source tree. Output is placed in ../Documentation/doxygen/html
	@(doxy_config_dir=../Documentation/doxygen; \
          dartheos_doxy_config_dir=../Tools/dartheos; \
          if [ -f $$doxy_config_dir/doxy.config ]; then \
            cd $$doxy_config_dir; \
            rm -rf html; \
            doxygen doxy.config; \
          else \
            echo "$$doxy_config_dir/doxy.config file not found"; \
          fi; \
         )

list_tests: # List all available test case Make targets
	@( $(MAKE) -C $(BASEDIR)/tests list_tests 2>&1 ) | grep -v '^$(MAKE)'
	@( $(MAKE) -C $(BASEDIR)/utils list_tests 2>&1 ) | grep -v '^$(MAKE)'

list_utils: # List all available utility code Make targets
	@( $(MAKE) -C $(BASEDIR)/utils list_tests 2>&1 ) | grep -v '^$(MAKE)'

msvs: # Create MS Visual Studio files for all Examples and tests codes
	@(rm -rf msvs/*;)
	@($(BASEDIR)/scripts/create_msvs_files tests/*.[cf]* Examples/*.[cf]*;)
	@(for f in */*.inp; do \
            target=`basename $$f`; \
            echo $$f '->' msvs/$$target; \
            cp $$f msvs/.; \
          done; \
         )

list_compilers:
	@printf "Fortran compiler(s) supported on this architecture: "
	@echo $(if $(F90_List),$(F90_List),$(F90));
ifneq ($(strip $(F90_Found_List)),)
	@printf "Supported Fortran compiler(s) found on this machine: ";
	@echo $(notdir $(F90_Found_List));
endif

list_installdirs: # List the install locations which will be used
	@(cd $(BASEDIR)/Examples; $(MAKE) $@)

COMPILER_INFO:
	@echo '********************************************************'
	@echo '--- C ---'
	-$(CC_VERSION)
	@echo '--- C++ ---'
	-$(CXX_VERSION)
	@echo '--- F77 ---'
	-$(F77_VERSION)
	@echo '--- F90 ---'
	-$(F90_VERSION)
	@echo ' '
	@echo '********************************************************'

.PHONY: all lib check test TAGS install uninstall clean cleanarch distclean compare \
        list_installed_files list_my_installed_files examples tests doxygen \
        list_tests send_report utils $(UTILTARGETS) msvs list_installdirs \
        list_compilers COMPILER_INFO checkutils partialclean mostlyclean

.NOTPARALLEL:
