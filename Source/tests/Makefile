##################################################################
#  
#  Copyright -- see file named COPYRIGHTNOTICE
#  
##################################################################
### Test Code Makefile.                                        ###
##################################################################
SHELL = /bin/sh

include ../Makefile.compile_opts

##################################################################
# define the floating point number comparison (FCMP),
# and its default options, $(FCMP_DEFAULT)
FCMP = $(BASEDIR)/scripts/fcmp
FCMP_DEFAULT = -d -i -e fcmp_ignore

# define the acceptable relative difference for regression testing using $(FCMP)
ACCEPTANCE_REL_DIFF = 1.0e-12

# define the file containing alternative options for $(FCMP)
FCMP_OPT = ./fcmp.opt

##################################################################

NORUN     = FALSE
NODISPLAY = TRUE
EXEDIR    = $(TESTEXEDIR)
OBJDIR    = $(EXEDIR)/_OBJ

# define lowercase variants
ifdef norun
  NORUN     = TRUE
endif
ifdef nodisplay
  NODISPLAY = FALSE
endif

##################################################################

##################################################################
### Create dependency lists, and build object/executable files ###
##################################################################
DEPDIR = $(BASEDIR)/tests/.depends
df = $(DEPDIR)/$(*F)
MAKEDEPEND = $(BASEDIR)/scripts/make_depends -d $(DEPDIR) -I $(BASEDIR)/src -I $(SESIOINC) $<

$(OBJDIR)/%.o : %.c
	$(MAKEDEPEND)
	${CC} $(C_ADDRESS_FLAG) ${CPPFLAGS} ${CFLAGS} $(MORE_COMPILE_OPTS) -c $< -o $@
	$(PURIFY) $(CC) $(C_ADDRESS_FLAG) $(LDFLAGS) $(MORE_LD_OPTS) -o $(EXEDIR)/$* $@ $(LIBS) $(MORELIBS)
	@(if [ ! -d "$(EXEDIR)/$*.workdir" ]; then mkdir -p "$(EXEDIR)/$*.workdir"; fi; \
          d=`echo $(EXEDIR)/$*.workdir | sed -e 's:$(BASEDIR)/*::' | sed -e 's:[^/]:\.\.:g;s:\.\.*:..:g'`; \
          sed -e "/^[[:space:]]*#/!s:<BASEDIR_PATTERN>:$$d:" sesameFilesDir.txt.template > "$(EXEDIR)/$*.workdir/sesameFilesDir.txt" ; \
          if [ -f $*.inp ]; then cp $*.inp "$(EXEDIR)/$*.workdir/." ; fi; \
         )

$(OBJDIR)/%.o : %.cpp
	$(MAKEDEPEND)
	${CXX} $(C_ADDRESS_FLAG) ${CPPFLAGS} ${CXXFLAGS} $(MORE_COMPILE_OPTS) -c $< -o $@
	$(PURIFY) $(CXX) $(C_ADDRESS_FLAG) $(LDFLAGS) $(MORE_LD_OPTS) -o $(EXEDIR)/$* $@ $(LIBS) $(MORELIBS)
	@(if [ ! -d "$(EXEDIR)/$*.workdir" ]; then mkdir -p "$(EXEDIR)/$*.workdir"; fi; \
          d=`echo $(EXEDIR)/$*.workdir | sed -e 's:$(BASEDIR)/*::' | sed -e 's:[^/]:\.\.:g;s:\.\.*:..:g'`; \
          sed -e "/^[[:space:]]*#/!s:<BASEDIR_PATTERN>:$$d:" sesameFilesDir.txt.template > "$(EXEDIR)/$*.workdir/sesameFilesDir.txt" ; \
          if [ -f $*.inp ]; then cp $*.inp "$(EXEDIR)/$*.workdir/." ; fi; \
         )

$(OBJDIR)/%.o : %.f
	$(MAKEDEPEND)
	${F77} $(F_ADDRESS_FLAG) ${FFLAGS} $(MORE_COMPILE_OPTS) -c $< -o $@
	$(PURIFY) $(F77) $(F_ADDRESS_FLAG) $(LDFLAGS) $(MORE_LD_OPTS) -o $(EXEDIR)/$* $@ $(LIBS) $(F77_LIBS) $(MORELIBS)
	@(if [ ! -d "$(EXEDIR)/$*.workdir" ]; then mkdir -p "$(EXEDIR)/$*.workdir"; fi; \
          d=`echo $(EXEDIR)/$*.workdir | sed -e 's:$(BASEDIR)/*::' | sed -e 's:[^/]:\.\.:g;s:\.\.*:..:g'`; \
          sed -e "/^[[:space:]]*#/!s:<BASEDIR_PATTERN>:$$d:" sesameFilesDir.txt.template > "$(EXEDIR)/$*.workdir/sesameFilesDir.txt" ; \
          if [ -f $*.inp ]; then cp $*.inp "$(EXEDIR)/$*.workdir/." ; fi; \
         )

$(OBJDIR)/%.o : %.f90
	$(MAKEDEPEND)
	@(d=`pwd`; \
         cd $(OBJDIR); \
         echo $(F90) $(F_ADDRESS_FLAG) ${F90FLAGS} $(MORE_COMPILE_OPTS) -c $$d/$< -o $@; \
         $(F90) $(F_ADDRESS_FLAG) ${F90FLAGS} $(MORE_COMPILE_OPTS) -c $$d/$< -o $@; \
        )
	$(PURIFY) $(F90) $(F_ADDRESS_FLAG) $(LDFLAGS) $(MORE_LD_OPTS) -o $(EXEDIR)/$* $@ $(PROFILE) $(LIBS) $(F90_LIBS) $(MORELIBS)
	@(if [ ! -d "$(EXEDIR)/$*.workdir" ]; then mkdir -p "$(EXEDIR)/$*.workdir"; fi; \
          d=`echo $(EXEDIR)/$*.workdir | sed -e 's:$(BASEDIR)/*::' | sed -e 's:[^/]:\.\.:g;s:\.\.*:..:g'`; \
          sed -e "/^[[:space:]]*#/!s:<BASEDIR_PATTERN>:$$d:" sesameFilesDir.txt.template > "$(EXEDIR)/$*.workdir/sesameFilesDir.txt" ; \
          if [ -f $*.inp ]; then cp $*.inp "$(EXEDIR)/$*.workdir/." ; fi; \
         )

##################################################################
### Target definitions                                         ###
##################################################################
ifeq ($(USE_F90),TRUE)
  SRCS_F03 = $(notdir $(shell egrep -l eos_Interface2003 $(BASEDIR)/tests/*.f*))
  SRCS_F90 = $(filter-out $(SRCS_F03),$(wildcard *.f90))
#$(warning "***" SRCS_F03 = $(SRCS_F03))
#$(warning "***" SRCS_F90 = $(SRCS_F90))
endif

export _NODEFAULTDATA = $(NODEFAULTDATA)
GET_SESAME_DATA = $(firstword $(wildcard $(BASEDIR)/bin/$(ARCH)/get_sesame_data $(BASEDIR)/bin/$(ARCH)/get_sesame_data.exe))
#$(warning "***" GET_SESAME_DATA = $(GET_SESAME_DATA))
#$(warning XCOMPILE = $(XCOMPILE))
ifeq ($(XCOMPILE),$(false))
  _EXCLUDED_EXE  := $(sort $(basename $(notdir $(shell $(BASEDIR)/scripts/excluded_codes $(GET_SESAME_DATA) $(if $(_NODEFAULTDATA),--noDefaultData,) 2>/dev/null))))
  #$(warning "***" $(BASEDIR)/scripts/excluded_codes $(GET_SESAME_DATA) --included $(if $(_NODEFAULTDATA),--noDefaultData,))
  _INCLUDED_EXE  = $(sort $(basename $(notdir $(shell $(BASEDIR)/scripts/excluded_codes $(GET_SESAME_DATA) --included $(if $(_NODEFAULTDATA),--noDefaultData,) 2>/dev/null))))
else
  # force all tests to be included for cross-compilation and backend execution
  $(warning force all tests to be included for cross-compilation and backend execution)
  _EXCLUDED_EXE  :=
  _INCLUDED_EXE  := $(sort $(basename $(notdir $(wildcard $(BASEDIR)/tests/test*.[fc]*))))
endif
#$(warning "***" _EXCLUDED_EXE = $(_EXCLUDED_EXE))
#$(warning "***" _INCLUDED_EXE = $(_INCLUDED_EXE))
EXCLUDED_EXE ?=$(sort $(if $(wildcard $(INCDIR)/*2003.[Mm][Oo][Dd]),,$(basename $(SRCS_F03))) $(_EXCLUDED_EXE))
INCLUDED_EXE ?= $(filter $(EXE),$(sort $(_INCLUDED_EXE)))
#$(warning "***" $(BASEDIR)/scripts/excluded_codes $(GET_SESAME_DATA) $(if $(_NODEFAULTDATA),--noDefaultData,))
#$(warning "***" EXCLUDED_EXE = $(EXCLUDED_EXE))
#$(warning "***" $(BASEDIR)/scripts/excluded_codes $(GET_SESAME_DATA) --included $(if $(_NODEFAULTDATA),--noDefaultData,))
#(warning "***" INCLUDED_EXE = $(INCLUDED_EXE))

EXCLUDED_OBJS            ?= $(sort $(addprefix $(OBJDIR)/,$(addsuffix .o,$(EXCLUDED_EXE))))
#$(warning "***" EXCLUDED_OBJS = $(EXCLUDED_OBJS))
FILTERED_OBJS            ?= $(sort $(addprefix $(OBJDIR)/,$(addsuffix .o,$(INCLUDED_EXE))))
#$(warning "***" FILTERED_OBJS = $(FILTERED_OBJS))
FILTERED_EXE             ?= $(sort $(addprefix $(EXEDIR)/,$(INCLUDED_EXE)))
#(warning "***" FILTERED_EXE = $(FILTERED_EXE))
COMPARE_TARGETS          ?= $(addsuffix compare,$(sort $(addprefix $(EXEDIR)/,$(EXE))))
#$(warning "***" COMPARE_TARGETS = $(COMPARE_TARGETS))
EXCEPTION_TARGETS        ?= $(addsuffix exception,$(sort $(addprefix $(EXEDIR)/,$(EXE))))
#$(warning "***" EXCEPTION_TARGETS = $(EXCEPTION_TARGETS))
FILTERED_COMPARE_TARGETS ?= $(addsuffix compare,$(addprefix $(EXEDIR)/,$(filter-out $(EXCLUDED_EXE),$(INCLUDED_EXE))))
#$(warning "***" FILTERED_COMPARE_TARGETS = $(FILTERED_COMPARE_TARGETS))
EXCLUDED_COMPARE_TARGETS ?= $(addsuffix compare,$(addprefix $(EXEDIR)/,$(EXCLUDED_EXE)))
#$(warning "***" EXCLUDED_COMPARE_TARGETS = $(EXCLUDED_COMPARE_TARGETS))

.PHONY: all
ifdef NODEP
all: MAKEDIRS
else
all: MAKEDIRS $(LIBRARY_AR) $(GET_MOD_FILES)
endif
ifeq ($(wildcard $(INCDIR)/*2003.[Mm][Oo][Dd]),)
	@$(MAKE) BUILD_EXECUTE_MESSAGE $(filter-out %Fortran2003_APItest.o,$(FILTERED_OBJS))
else
	@$(MAKE) BUILD_EXECUTE_MESSAGE $(FILTERED_OBJS)
endif

COMPILER_INFO_FILE = $(LIBDIR)/compiler_info.txt
PASSFAILOUTPUTFILE = $(EXEDIR)/pass_fail_output_file.txt
PASSFAILOUTPUTHTML = $(EXEDIR)/pass_fail_output_file.html
PASSFAILOUTPUTXML  = $(EXEDIR)/pass_fail_output_file.xml
SPLITDUMPFILES = EOS_SPLIT_NUM_PROP.txt EOS_SPLIT_COWAN.txt EOS_SPLIT_IDEAL_GAS.txt
#MAKE_HTML_OPT="--totaltests=$(words $(EXE) $(EXE))"

.PHONY: $(EXE)
ifdef NODEP
$(EXE): CLEAN_WORK_DIRS MAKEDIRS
else
$(EXE): CLEAN_WORK_DIRS MAKEDIRS $(LIBRARY_AR) $(GET_MOD_FILES)
endif
	@echo ' '
	-rm -rf $(OBJDIR)/$@.o \
                $(EXEDIR)/$(@) \
                $(EXEDIR)/$(@).exe \
                $(EXEDIR)/$@.$(DATADUMPFILE) \
                $(EXEDIR)/$@.stdout \
                $(EXEDIR)/$(@).workdir
	@echo '**************************************************************'
	@echo '*** Creating ' $(OBJDIR)/$@.o
	@echo '**************************************************************'
	@($(MAKE) $(OBJDIR)/$@.o; \
          if [ "$(NORUN)" = "FALSE" ]; then \
             $(MAKE) EXECUTE_MESSAGE $(addprefix $(EXEDIR)/,$@); \
             $(MAKE) $(addsuffix compare,$(addprefix $(EXEDIR)/,$@)); \
             $(MAKE) EXE=$@; \
             $(MAKE) GATHER_COMPARE_DATA; \
             $(MAKE) -j1 EXE=$@ MAKE_HTML_OPT=--totaltests=$(words $(@) $(@)) make_html make_xml; \
          fi; \
         )

.PHONY: $(addprefix $(EXEDIR)/,$(EXE))
$(addprefix $(EXEDIR)/,$(EXE)):
	@-(exe=`basename $@`; \
           rm -f $(DATADUMPFILE) $$exe.$(DATADUMPFILE) $$exe.status_error > $(JUNKFILE) 2>&1; \
           cd "$(EXEDIR)/$${exe}.workdir"; \
           if [ -f "../$${exe}" -o -f "../$${exe}.exe" ]; then \
              printf "*** Execute %s\n" $$exe; \
              rm -f $(DATADUMPFILE) $$exe.$(DATADUMPFILE) $$exe.status_error > $(JUNKFILE) 2>&1; \
              if [ -n "$(VALGRIND)" ]; then \
                valgrind_opt="--leak-check=full --log-file=$$exe.valgrind"; \
                printf "\t($(VALGRIND) $$valgrind_opt ./$$exe)\n"; \
              else \
                valgrind_opt=""; \
              fi; \
              if [ -n "$(DEBUGGER)" ]; then \
                $(DEBUGGER) ../$$exe ../$$exe > ../$$exe.stdout 2>&1; \
              else \
                if [ -x /usr/bin/time ]; then \
                  timer='/usr/bin/time -p'; \
                else \
                  timer=''; \
                  start_time=`perl -e 'print time'`; \
                fi;\
                echo name: $$exe > $$exe.$(TIMINGFILE_P); \
                if [ -n "$(VALGRIND)" ]; then \
                  $$timer $(VALGRIND) $$valgrind_opt ../$$exe > ../$$exe.stdout 2>> $$exe.$(TIMINGFILE_P); \
                else \
                  $$timer ../$$exe > ../$$exe.stdout 2>> $$exe.$(TIMINGFILE_P); \
                fi;\
              fi; \
              exe_status=$$?; \
              if [ ! -x /usr/bin/time ]; then \
                perl -e 'print shift;$$t=eval time - shift;print "\t",(($$t<=0)?1:$$t),"\n"' $$exe $$start_time > $$exe.$(TIMINGFILE); \
              fi; \
              if [ $$exe_status -ne 0 ]; then \
                 printf '    EXECUTION STATUS ERROR for %s (status=%d)\n' $$exe $$exe_status; \
                 printf 'EXECUTION STATUS ERROR: %d' $$exe_status > $$exe.status_error 2>&1; \
              fi; \
              mv -f $(DATADUMPFILE) ../$$exe.$(DATADUMPFILE) > $(JUNKFILE) 2>&1; \
              mv -f *.valgrind* $$exe.status_error ../. > $(JUNKFILE) 2>&1; \
              for spdf in $(SPLITDUMPFILES); do \
                 if [ -f $$spdf ]; then mv -f $$spdf ../$$exe.$$spdf > $(JUNKFILE) 2>&1; fi; \
              done; \
              cd ..; \
              if [ -n "$(VALGRIND)" ]; then \
                grep '=  *[^ ][^ ]* *lost:  *[^0]' $(EXEDIR)/$$exe.valgrind | sed 's/^.*= / /' > $(EXEDIR)/$$exe.leak.summary 2>&1; \
                grep 'Process terminating' $(EXEDIR)/$$exe.valgrind | sed 's/^.*= / /' >> $(EXEDIR)/$$exe.leak.summary 2>&1; \
                egrep -C 1 '=  *[^ ][^ ]* *\(at|by\)' $(EXEDIR)/$$exe.valgrind | sed 's/^.*= / /' >> $(EXEDIR)/$$exe.leak.summary 2>&1; \
                if [ -z "$(EXEDIR)/$$exe.leak.summary" ]; then \
                  rm -f $(EXEDIR)/$$exe.leak.summary $(EXEDIR)/$$exe.valgrind; \
                fi; \
              fi; \
           else \
              printf "*** Execute %s ... MISSING EXECUTABLE\n" $$exe; \
           fi; \
          )

DATADUMPFILE = TablesLoaded.dat
TIMINGFILE   = test_times.txt
TIMINGFILE_P = test_times_portable.txt
TIMINGXML    = test_times.xml

.PHONY: check test
check test: all
  ifeq ($(XCOMPILE),$(false))
	@$(MAKE) execute_only
  else
	@$(MAKE) EXECUTE_MESSAGE check_on_compute_node
  endif

.PHONY: execute_only
execute_only: CLEAN_WORK_DIRS
    ifeq ($(wildcard $(INCDIR)/*2003.[Mm][Oo][Dd]),)
	@$(MAKE) EXECUTE_MESSAGE $(filter-out %Fortran2003_APItest,$(FILTERED_EXE))
    else
	@$(MAKE) EXECUTE_MESSAGE $(FILTERED_EXE)
    endif
    ifeq ($(wildcard $(INCDIR)/*2003.[Mm][Oo][Dd]),)
	@$(MAKE) EXCEPTION_MESSAGE $(filter-out %Fortran2003_APItest%,$(EXCEPTION_TARGETS))
    else
	@$(MAKE) EXCEPTION_MESSAGE $(EXCEPTION_TARGETS)
    endif
	@(echo ' '; \
          echo '**************************************************************'; \
          echo '***' Compare $(ARCH) to regression baseline results; \
          echo '**************************************************************'; \
         )
	@$(MAKE) $(COMPARE_TARGETS)
	@$(MAKE) GATHER_COMPARE_DATA
	$(MAKE) -j1 MAKE_HTML_OPT=$(MAKE_HTML_OPT) make_html make_xml
	@(if [ -n "$(VALGRIND)" ]; then \
            $(MAKE) GATHER_VALGRIND_DATA; \
          fi;)
#	@(echo '--- BEGIN XML ($(MAKEFILE_LIST)) ---'; cat $(PASSFAILOUTPUTXML); echo '---- END XML ($(MAKEFILE_LIST)) ----';)

.PHONY: check_on_compute_node
check_on_compute_node: create_parallel_job_list
	@-rm -f $(EXEDIR)/run_tests_on_compute_node.msub.out
	@echo 'START:' `date`
	@STARTDIR=$(EXEDIR);export STARTDIR;pwd;set -x;msub -d ${EXEDIR} -K $(BASEDIR)/scripts/run_tests_on_$(XCOMPILE)_node.msub
	@echo '  END:' `date`
	@$(MAKE) compare
	@$(MAKE) -j1 MAKE_HTML_OPT=$(MAKE_HTML_OPT) make_html make_xml

.PHONY: create_parallel_job_list
create_parallel_job_list:
	@(for f in $(FILTERED_EXE); do \
            t=`basename $${f}`; \
            echo "cd $(EXEDIR)/$${t}.workdir; ( echo name: $${t}; /usr/bin/time -p ../$${t} > ../$${t}.stdout ) >& $${t}.test_times_portable.txt"; \
          done; \
         ) > ${EXEDIR}/parallel.job.list

.PHONY: GATHER_VALGRIND_DATA
GATHER_VALGRIND_DATA:
	@(if [ -n "$(VALGRIND)" ]; then \
            h="VALGRIND SUMMARY"; \
            for f in $(EXEDIR)/*.leak.summary; do \
              f_root=`basename $$f .leak.summary`; \
              if [ -s "$$f" ]; then \
                if [ -n "$$h" ]; then printf "\n%s\n" "VALGRIND SUMMARY:"; fi; \
                printf "  %s:\n" "$${f_root}"; \
                cat $$f | sed -e 's/^/\t/'; \
                printf "\t***see %s for details\n\n" "$(EXEDIR)/$${f_root}.valgrind"; \
                h=""; \
              else \
                rm -f $$f $(EXEDIR)/$${f_root}.valgrind; \
              fi; \
            done; \
          fi; \
         )

.PHONY: CLEAN_WORK_DIRS
CLEAN_WORK_DIRS:
	@rm -f $(EXEDIR)/*.workdir/*.test_times*.txt
	@rm -f $(EXEDIR)/*.TablesLoaded.dat
	@rm -f $(EXEDIR)/*.stdout
	@rm -f $(EXEDIR)/*.workdir/TablesLoaded.dat

.PHONY: GATHER_TIMING_DATA
GATHER_TIMING_DATA:
	@-(echo '#'; \
           echo '# This file contains a tab-delimited list of the latest test execution'; \
           echo '# times, which will be rounded to the next second on systems without the'; \
           echo '# /usr/bin/time utility.'; \
           echo '#'; \
           awk '/^name:/{t=$$2} /^real/{print t "\t" $$2}' $(EXEDIR)/*.workdir/*.$(TIMINGFILE_P); \
          ) > $(EXEDIR)/$(TIMINGFILE)
	@-(echo '<timer>'; \
           echo '  <date>' `date` '</date>'; \
           perl -MFile::Basename -e 'for $$f (@ARGV) {open FH,$$f or next;$$r=basename $$f,".test_times_portable.txt";print "  <$$r>\n"; while (<FH>) {@f=split " ";print "    <$$f[0]>$$f[1]</$$f[0]>\n" if scalar(@f) eq 2} print "  </$$r>\n"}' \
                 $(EXEDIR)/*.workdir/*.$(TIMINGFILE_P); \
           echo '</timer>'; \
          ) > $(EXEDIR)/$(TIMINGXML)

.PHONY: EXCEPTION_MESSAGE
EXCEPTION_MESSAGE:
	@(echo  ' '; \
          echo 'Apply applicable regression baseline patches:'; \
         )

.PHONY: EXECUTE_MESSAGE
EXECUTE_MESSAGE:
	@(echo  ' '; \
          echo 'Execute all tests:'; \
         )

.PHONY: BUILD_EXECUTE_MESSAGE
BUILD_EXECUTE_MESSAGE:
	@(echo  ' '; \
          echo 'Build all executables:'; \
         )

.PHONY: $(EXCLUDED_COMPARE_TARGETS)
$(EXCLUDED_COMPARE_TARGETS):
	@(exe=`basename $@ | sed -e 's/compare//'`; \
          if [ ! -d "$(EXEDIR)/$${exe}.workdir" ]; then \
             mkdir -p "$(EXEDIR)/$${exe}.workdir"; \
          fi; \
          test_result_file=$(EXEDIR)/$${exe}.workdir/test_result.txt; \
          rm $${test_result_file}; \
          for ext in stdout ${DATADUMPFILE}; do \
            echo "IGNORED:  $$exe.$$ext was ignored because either the required data is unavailable or an incompatible compiler was used" | tee -a $${test_result_file}; \
          done; \
         )

.PHONY: $(EXCEPTION_TARGETS)
$(EXCEPTION_TARGETS):
	@(exe=`basename $@ | sed -e 's/exception//'`; \
          for ext in stdout $(DATADUMPFILE); do \
            f=`${BASEDIR}/scripts/find_exception $(REGRESSION_DATA_EXCEPTIONS_DIR_NEW) $(REGRESSION_DATA_EXCEPTIONS_DIR) -r $(REGRESSION_DATA_DIR_PARENT) $$exe.$${ext}`; \
            if [ -n "$$f" ]; then \
              f0=$(REGRESSION_DATA_DIR)/`basename $$f | sed 's/\.[a-zA-Z0-9]\{1,\}$$//'`; \
              f1=`echo $$f | sed 's/\.[a-zA-Z0-9]\{1,\}$$//'`; \
              echo "+ patch -o $$f1 -i $$f $$f0"; \
              patch -o $$f1 -i $$f $$f0 > $(JUNKFILE) 2>&1; \
            fi; \
          done; \
         )

.PHONY: $(FILTERED_COMPARE_TARGETS)
$(FILTERED_COMPARE_TARGETS):
	@(exe=`basename $@ | sed -e 's/compare//'`; \
          if [ ! -d "$(EXEDIR)/$${exe}.workdir" ]; then \
             mkdir -p "$(EXEDIR)/$${exe}.workdir"; \
          fi; \
          if [ -f "$(EXEDIR)/$${exe}.workdir/TablesLoaded.dat" ]; then \
             cp "$(EXEDIR)/$${exe}.workdir/TablesLoaded.dat" "$(EXEDIR)/$${exe}.TablesLoaded.dat"; \
          fi; \
          test_result_file=$(EXEDIR)/$${exe}.workdir/test_result.txt; \
          rm -f $${test_result_file}; \
          touch $${test_result_file}; \
          ACCEPTANCE_REL_DIFF="$(ACCEPTANCE_REL_DIFF)"; \
          ARCH="$(ARCH)"; \
          BASEDIR="$(BASEDIR)"; \
          DATADUMPFILE="$(DATADUMPFILE)"; \
          EXEDIR="$(EXEDIR)"; \
          FCMP_DEFAULT="$(FCMP_DEFAULT)"; \
          FCMP_OPT="$(FCMP_OPT)"; \
          FCMP="$(FCMP)"; \
          JUNKFILE="$(JUNKFILE)"; \
          REGRESSION_DATA_DIR="$(REGRESSION_DATA_DIR)"; \
          REGRESSION_DATA_EXCEPTIONS_DIR="$(REGRESSION_DATA_EXCEPTIONS_DIR)"; \
          REGRESSION_DATA_EXCEPTIONS_DIR_NEW="$(REGRESSION_DATA_EXCEPTIONS_DIR_NEW)"; \
          _MANDATORY_FCMP_FLAGS="$(_MANDATORY_FCMP_FLAGS)"; \
          export ACCEPTANCE_REL_DIFF; \
          export ARCH; \
          export BASEDIR; \
          export DATADUMPFILE; \
          export EXEDIR; \
          export FCMP_DEFAULT; \
          export FCMP_OPT; \
          export FCMP; \
          export JUNKFILE; \
          export REGRESSION_DATA_DIR; \
          export REGRESSION_DATA_EXCEPTIONS_DIR; \
          export REGRESSION_DATA_EXCEPTIONS_DIR_NEW; \
          export _MANDATORY_FCMP_FLAGS; \
          $(BASEDIR)/scripts/compare.sh -t ndiff $$exe; \
         )

.PHONY: GATHER_COMPARE_DATA
GATHER_COMPARE_DATA: GATHER_TIMING_DATA
	@(d=`dirname $(PASSFAILOUTPUTFILE)`; \
          if [ -n $$d -a -d $$d ]; then \
            date > $(PASSFAILOUTPUTFILE); \
            echo EOSPAC $(CVS_BASE_TAG_NAME) >> $(PASSFAILOUTPUTFILE); \
            uname -a >> $(PASSFAILOUTPUTFILE); \
            echo ' ' >> $(PASSFAILOUTPUTFILE); \
            for exe in $(sort $(EXE)); do \
              test_result_file=$(EXEDIR)/$${exe}.workdir/test_result.txt; \
              cat $${test_result_file} >> $(PASSFAILOUTPUTFILE); \
            done; \
          fi;)
	@(d=`dirname $(PASSFAILOUTPUTFILE)`; \
          if [ -n $$d -a -d $$d ]; then \
            echo ' ' | tee -a $(PASSFAILOUTPUTFILE); \
            cntr=`grep -c FAILED $(PASSFAILOUTPUTFILE)`; \
            tmpcntr=`grep -c PASSED $(PASSFAILOUTPUTFILE)`; \
            cntr_ignored=`grep -c IGNORED $(PASSFAILOUTPUTFILE)`; \
            cntr_warning=`grep -c WARNING $(PASSFAILOUTPUTFILE)`; \
            cntr_excluded=`grep -c excluded $(PASSFAILOUTPUTFILE)`; \
            cntr_exceptions=`grep -c exception $(PASSFAILOUTPUTFILE)`; \
            cntr_test_times_file_wc=`grep -v '#' $(EXEDIR)/$(TIMINGFILE) | wc -l`; \
            cntr_test_times_total_sec=`grep -v '^#' $(EXEDIR)/$(TIMINGFILE) | awk '{ SUM += $$2} END { print SUM }'`; \
            cntr_test_times_avg_sec=`echo $$cntr_test_times_total_sec $$cntr_test_times_file_wc | awk '{ printf "%.2f", $$1/$$2 }'`; \
            cntr_test_times_total_str=`echo $$cntr_test_times_total_sec | awk '{ SUM += $$1} END { print int(SUM/60) " min " (SUM/60-int($$1/60))*60 " sec" }'`; \
            totcntr=`expr $$tmpcntr + $$cntr + $$cntr_warning`; \
            grandtotcntr=`expr $$tmpcntr + $$cntr + $$cntr_warning + $$cntr_ignored + $$cntr_excluded`; \
            if [ $$cntr -ne 0 ]; then \
              echo "   *** $$cntr of $$totcntr TESTS FAILED!!! ***"; \
            fi; \
            if [ $$tmpcntr -ne 0 ]; then \
              echo "   *** $$tmpcntr of $$totcntr TESTS PASSED!!! ***"; \
            fi; \
            if [ $$cntr_warning -ne 0 ]; then \
              echo "    WARNING:   $$cntr_warning of $$grandtotcntr have no baseline output."; \
            fi; \
            if [ $$cntr_ignored -ne 0 ]; then \
              was='was'; \
              if [ $$cntr_ignored -gt 1 ]; then \
                was='were'; \
              fi; \
              echo "       NOTE: $$cntr_ignored of $$grandtotcntr $$was ignored due to either a lack of required data or an incompatible compiler. (not included in total)"; \
            fi; \
            if [ $$cntr_excluded -ne 0 ]; then \
              was='was'; \
              if [ $$cntr_excluded -gt 1 ]; then \
                was='were'; \
              fi; \
              echo "       NOTE: $$cntr_excluded of $$grandtotcntr $$was purposely excluded according to fcmp options in fcmp.opt. (not included in total)"; \
            fi; \
            if [ $$cntr_exceptions -ne 0 ]; then \
              was='was'; \
              if [ $$cntr_exceptions -gt 1 ]; then \
                was='were'; \
              fi; \
              echo "       NOTE: $$cntr_exceptions of $$grandtotcntr $$was compared to baseline exceptions."; \
            fi; \
            if [ $$cntr_test_times_file_wc -ne 0 ]; then \
              echo "       NOTE: $$cntr_test_times_file_wc test codes took $$cntr_test_times_total_str with an average time of $$cntr_test_times_avg_sec sec/test."; \
            fi; \
            echo 'END_PASSFAILOUTPUT' >> $(PASSFAILOUTPUTFILE); \
            echo ' '; \
          fi;)

.PHONY: compare
compare:
	@( $(MAKE) COMPILER_INFO 2>&1 ) | tee $(COMPILER_INFO_FILE)
	$(MAKE) EXCEPTION_MESSAGE $(EXCEPTION_TARGETS)
	@-(echo ' '; \
           echo '**************************************************************'; \
           echo '***' Compare $(ARCH) to regression baseline results; \
           echo '**************************************************************'; \
           date > $(PASSFAILOUTPUTFILE); \
           echo EOSPAC $(CVS_BASE_TAG_NAME) >> $(PASSFAILOUTPUTFILE); \
           uname -a >> $(PASSFAILOUTPUTFILE); \
           echo ' ' | tee -a $(PASSFAILOUTPUTFILE); \
           for exe in $(sort $(EXE)); do \
             exe_in_str=`echo $(FILTERED_EXE) | grep $$exe`; \
             if [ -z "$$exe_in_str" ]; then \
               continue; \
             fi; \
             if [ -n "$(VALGRIND)" ]; then \
               grep '=  *[^ ][^ ]* *lost:  *[^0]' $(EXEDIR)/$$exe.valgrind | sed 's/^.*= / /' > $(EXEDIR)/$${exe}.workdir/leak.summary 2>&1; \
               grep 'Process terminating' $(EXEDIR)/$$exe.valgrind | sed 's/^.*= / /' >> $(EXEDIR)/$${exe}.workdir/leak.summary 2>&1; \
               egrep -C 1 '=  *(at|by)' $(EXEDIR)/$$exe.valgrind | sed 's/^.*= / /' >> $(EXEDIR)/$${exe}.workdir/leak.summary 2>&1; \
               if [ -s "$(EXEDIR)/$${exe}.workdir/leak.summary" ]; then \
                 printf "\tMEMORY LEAK SUMMARY:\n"; \
                 cat $(EXEDIR)/$${exe}.workdir/leak.summary; \
               else \
                 rm -f $(EXEDIR)/$${exe}.workdir/leak.summary $(EXEDIR)/$$exe.valgrind; \
               fi; \
             fi; \
           done;)
	@-$(MAKE) $(COMPARE_TARGETS)
	@-$(MAKE) GATHER_COMPARE_DATA
	@-$(MAKE) GATHER_VALGRIND_DATA

.PHONY: list_exception_patch_files
list_exception_patch_files: # these files shall be created either using "diff -e old new" and named like old.ed or using "diff old new" and named like old.ndiff
	@-(echo; \
           echo "$@:"; \
           edfiles=`$(BASEDIR)/scripts/find_exception $(REGRESSION_DATA_EXCEPTIONS_DIR_NEW) $(REGRESSION_DATA_EXCEPTIONS_DIR) -r $(REGRESSION_DATA_DIR_PARENT) -a`; \
           printf "\t%s\n" `echo $$edfiles | tr " " "\n"`; \
           echo; \
          )

.PHONY: create_exception_files
create_exception_files: # these files shall be created either using "diff -e old new" and named like old.ed or using "diff old new" and named like old.ndiff
	@-(set -x;edfiles=`$(BASEDIR)/scripts/find_exception $(REGRESSION_DATA_EXCEPTIONS_DIR_NEW) $(REGRESSION_DATA_EXCEPTIONS_DIR) -r $(REGRESSION_DATA_DIR_PARENT) -a`; \
           for f in $$edfiles; do \
             f0=$(REGRESSION_DATA_DIR)/`basename $$f | sed 's/\.[a-zA-Z0-9]\{1,\}$$//'`; \
             f1=`dirname $$f`/`basename $$f | sed 's/\.[a-zA-Z0-9]\{1,\}$$//'`; \
             patch -o $$f1 $$f0 $$f; \
             if [ -f $$f1 ]; then \
               : '           ->' $$f1; echo '        using' $$f; \
             fi; \
           done; \
           rm -f $(JUNKFILE); \
          )
#             patch -o $$f1 $$f0 $$f > $(JUNKFILE) 2>&1; \

.PHONY: make_html
make_html:
	@$(BASEDIR)/scripts/make_html $(MAKE_HTML_OPT) --preamble '$(ORIGINAL_COMMAND_LINE)' --verbose $(PASSFAILOUTPUTFILE) > $(PASSFAILOUTPUTHTML)

.PHONY: make_xml
make_xml:
	@$(BASEDIR)/scripts/make_html $(MAKE_HTML_OPT) --verbose --xml $(PASSFAILOUTPUTFILE) --compiler_info_file $(COMPILER_INFO_FILE) --test_times_file $(EXEDIR)/$(TIMINGFILE) > $(PASSFAILOUTPUTXML)

.PHONY: send_report
send_report: make_html make_xml
	@-(my_system_name=`uname -n`; \
           cntr=`grep -c FAILED $(PASSFAILOUTPUTFILE)`; \
           tmpcntr=`grep -c PASSED $(PASSFAILOUTPUTFILE)`; \
           cntr_ignored=`grep -c IGNORED $(PASSFAILOUTPUTFILE)`; \
           cntr_warning=`grep -c WARNING $(PASSFAILOUTPUTFILE)`; \
           totcntr=`expr $$tmpcntr + $$cntr + $$cntr_warning`; \
           $(BASEDIR)/scripts/send_html_mail -p $(PASSFAILOUTPUTFILE) \
                 -s "EOSPAC 6: $$tmpcntr of $$totcntr PASSED ($$my_system_name: $(ARCH))" $(PASSFAILOUTPUTHTML); \
         )

.PHONY: $(GET_MOD_FILES)
$(GET_MOD_FILES):
# this is used to put all *.mod or *.MOD into CWD -- Compaq Dev Studio f90 requires this
	@-cp -f $(INCDIR)/*.mod $(INCDIR)/*.MOD .

ARCHROOT = $(subst $(ALT_SUBDIR),,$(EXEDIR))
.PHONY: MAKEDIRS
MAKEDIRS:
	mkdir -p $(OBJDIR)
	mkdir -p $(EXEDIR)
	-touch $(ARCHROOT)/.distclean
#	:     EXEDIR = $(EXEDIR)
#	: ALT_SUBDIR = $(ALT_SUBDIR)
#	:   ARCHROOT = $(ARCHROOT)

.PHONY: $(LIBRARY_AR)
$(LIBRARY_AR):
	@$(MAKE) -C $(BASEDIR)/src

.PHONY: version
version:
ifeq (0,${MAKELEVEL})
	@$(MAKE) -C $(BASEDIR)/src version
endif

.PHONY: install
install: clean all
ifeq (0,${MAKELEVEL})
	@$(MAKE) -C $(BASEDIR)/Examples install
endif

.PHONY: list_my_installed_files
list_my_installed_files:
ifeq (0,${MAKELEVEL})
	@$(MAKE) -C $(BASEDIR)/Examples list_my_installed_files
endif

.PHONY: list_installed_files
list_installed_files:
ifeq (0,${MAKELEVEL})
	@$(MAKE) -C $(BASEDIR)/Examples list_installed_files
endif

.PHONY: uninstall
uninstall:
ifeq (0,${MAKELEVEL})
	@$(MAKE) -C $(BASEDIR)/Examples uninstall
endif

TAGS: $(HEADERS) $(SRCS)
	etags $(HEADERS) $(SRCS)

.PHONY: clean
clean:
ifeq (0,${MAKELEVEL})
	@$(MAKE) -C $(BASEDIR)/Examples clean
else
	-rm -fr $(EXAMPLEDIR) $(DEPDIR) $(EXEDIR) $(LIBDIR) \
                `find . -name 'core*'` `find . -name '*.stackdump'`
	@$(MAKE) -C $(BASEDIR)/src clean
endif

.PHONY: cleanarch
cleanarch:
ifeq (0,${MAKELEVEL})
	@$(MAKE) -C $(BASEDIR) cleanarch
endif

ifeq (0,${MAKELEVEL})
.PHONY: distclean
distclean:
	@$(MAKE) -C $(BASEDIR)/Examples distclean

.PHONY: distcleanfast
distcleanfast:
	@$(MAKE) -C $(BASEDIR)/Examples distcleanfast
else

DISTCLEAN_DIRS = $(patsubst %/.distclean,%,$(wildcard $(BASEDIR)/tests/*/.distclean))
DISTCLEAN_SUBDIRS = $(notdir $(DISTCLEAN_DIRS))

.PHONY: distclean
distclean: $(addprefix distclean_,$(DISTCLEAN_SUBDIRS))
	-rm -fr $(DEPDIR) $(BUILD_LOG) \
                `find . -name '$(JUNKFILE)*'` \
                `find . -name data -prune -o -name '*.MOD' -o -name '*.mod' -print` \
                `find . -name data -prune -o -name 'core*' -o -name '*.stackdump' -print`
	-rm -f  `find ./data -name '*.ed' -o -name '*.?diff' | sed 's/\.[a-zA-Z0-9]\{1,\}$$//'`

.PHONY: distclean_%
distclean_%:
	@(loc=`pwd`; \
          set -x; \
          rm -rf $$loc/$*; \
         )

#
# The distcleanfast target is designed to accelerate the removal of the tests/$(ARCH)
# subdirectories, which can be very large and slow to delete on some file systems, by
# moving them to a local $(TRASHDIR) for later deletion.
#
DISTCLEANFAST_DIRS = $(patsubst %/.distclean,%,$(wildcard $(BASEDIR)/tests/*/.distclean))
DISTCLEANFAST_SUBDIRS = $(notdir $(DISTCLEANFAST_DIRS))
.PHONY: distcleanfast
distcleanfast: TRASHDIR := $(shell mktemp -d $(BASEDIR)/.distcleanfast.trash.XXXXXXXXXX)
distcleanfast: DELETED  := $(shell rmdir $(TRASHDIR))
distcleanfast: $(addprefix distcleanfast_,$(DISTCLEANFAST_SUBDIRS))
	@echo "*******************************************************************"
	@echo "* For the sake of expedience, all of the tests' working files and"
	@echo "* directories have been moved into"
	@echo "* $(TRASHDIR)"
	@echo "* You must delete them to be permanently rid of them."
	@echo "*******************************************************************"

.PHONY: distcleanfast_%
distcleanfast_%:
	@(loc=`pwd`; \
          set -x; \
          mkdir -vp $(TRASHDIR)/tests; \
          mv -f $$loc/$* $(TRASHDIR)/tests/.; \
         )

endif

.PHONY: mostlyclean
mostlyclean:
	@(loc=`pwd`; \
          for f in `find $$loc -name .distclean`; do \
            d=`dirname $$f`; \
            if [ -d $$d ]; then \
              printf "Removing %s" $$d; \
              rm -r -f $$d; \
              if [ $$? -eq 0 ]; then \
                echo ' ... ok'; \
              else \
                echo ' ... failed'; \
              fi; \
            fi; \
          done; \
         )
	-rm -fr $(DEPDIR) $(BUILD_LOG) \
                `find . -name data -prune -o -name '*.MOD' -o -name '*.mod' -print` \
                `find . -name data -prune -o -name 'core*' -o -name '*.stackdump' -print`

.PHONY: list_tests
list_tests:
	@-(printf "\nGiven the SESAME data found on your system, the available test case Make targets are:\n"; \
           $(BASEDIR)/scripts/fmt_list $(notdir $(FILTERED_EXE)); \
           printf "\n";)

define HELP_ADDENDUM
Additionally, the following Makefile variables may be set on the command line:

  memcheck=1        Use either valgrind or purify to perform memory tests.
                    If both valgrind and purify are found, then valgrind is used.

  PURIFY=purify     Force the usage of purify for memory tests. If purify
                    is named something else on your system, then define it with this variable.

  VALGRIND=valgrind Force the usage of valgrind for memory tests. If valgrind
                    is named something else on your system, then define it with this variable.
endef
export HELP_ADDENDUM

.PHONY: help
help: # Print this help
	@(printf "\n%s\n" "$(BASEDIR)/tests/Makefile contains the following valid targets:"; \
          perl $(BASEDIR)/scripts/display_targets $(BASEDIR)/tests/Makefile; \
          if [ 0 -eq ${MAKELEVEL} ]; then \
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
          fi; \
         ) | more

.PHONY: COMPILER_INFO
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

.PHONY: all check test MAKEDIRS install TAGS clean cleanarch distclean mostlyclean \
        help list_installed_files list_my_installed_files list_tests send_report \
        make_html make_xml $(addprefix $(EXEDIR)/,$(EXE)) EXECUTE_MESSAGE \
        GATHER_TIMING_DATA GATHER_COMPARE_DATA CLEAN_WORK_DIRS create_exception_files \
        GATHER_VALGRIND_DATA show_distclean \
        $(COMPARE_TARGETS) $(EXCLUDED_COMPARE_TARGETS) $(FILTERED_COMPARE_TARGETS)
