##################################################################
#
#  Copyright -- see file named COPYRIGHTNOTICE
#
##################################################################
### Example Code Makefile.                                     ###
##################################################################
SHELL = /bin/sh

include ../Makefile.compile_opts

##################################################################

EXEDIR = $(EXAMPLEDIR)

##################################################################
### Create dependency lists, and build object/executable files ###
##################################################################
DEPDIR = $(BASEDIR)/Examples/.depends
df = $(DEPDIR)/$(*F)
MAKEDEPEND = $(BASEDIR)/scripts/make_depends -d $(DEPDIR) -I $(SRCDIR) -I $(SESIOINC) $<
$(OBJDIR)/%.o : %.c
	-$(MAKEDEPEND)
	${CC} $(C_ADDRESS_FLAG) ${CPPFLAGS} ${CFLAGS} -I$(SRCDIR) -c $< -o $@
	$(PURIFY) $(CC) $(C_ADDRESS_FLAG) $(LDFLAGS) -o $(EXEDIR)/$* $@ $(LIBS)
	-rm $@
	@echo ' '

$(OBJDIR)/%.o : %.cpp
	-$(MAKEDEPEND)
	${CXX} $(C_ADDRESS_FLAG) ${CPPFLAGS} ${CXXFLAGS} -I$(SRCDIR) -c $< -o $@
	$(PURIFY) $(CXX) $(C_ADDRESS_FLAG) $(LDFLAGS) -o $(EXEDIR)/$* $@ $(LIBS)
	-rm $@
	@echo ' '

$(OBJDIR)/%.o : %.f
	-$(MAKEDEPEND)
	${F77} $(F_ADDRESS_FLAG) ${FFLAGS} -I$(SRCDIR) -c $< -o $@
	$(PURIFY) $(F77) $(F_ADDRESS_FLAG) $(LDFLAGS) -o $(EXEDIR)/$* $@ $(LIBS) $(F77_LIBS)
	-rm $@
	@echo ' '

$(OBJDIR)/%.o : %.f90
	-$(MAKEDEPEND)
	@(d=`pwd`; \
         cd $(OBJDIR); \
         echo $(F90) $(F_ADDRESS_FLAG) ${F90FLAGS} -I$(SRCDIR) -c $$d/$< -o $@; \
         $(F90) $(F_ADDRESS_FLAG) ${F90FLAGS} -c $$d/$< -o $@; \
        )
	$(PURIFY) $(F90) $(F_ADDRESS_FLAG) $(LDFLAGS) -o $(EXEDIR)/$* $@ $(PROFILE) $(LIBS) $(F90_LIBS)
	-rm $@
	@echo ' '

##################################################################
### Target definitions                                         ###
##################################################################
all: MAKEDIRS
	( $(MAKE) all_child 2>&1 ) | tee ${EXEDIR}/$(BUILD_LOG) 2>&1

all_child:
	$(MAKE) build_lib
	@echo ' '
	@echo '***********************************************************'
	@echo '***' Build example executables
	@echo '***********************************************************'
	$(MAKE) LISTOBJS $(GET_MOD_FILES) $(OBJS) COPYFILES

Test%:
	$(MAKE) build_lib
	@echo ' '
	@echo '***********************************************************'
	@echo '***' Build example ... $@
	@echo '***********************************************************'
	$(MAKE) MAKEDIRS $(GET_MOD_FILES) $(OBJDIR)/$@.o COPYFILES
	@echo ' '
	@echo '**************************************************************'
	@echo '***' Execute example ... $@
	@echo '**************************************************************'
	@-(cd $(EXEDIR); \
           if [ -n "$(VALGRIND)" ]; then \
             valgrind_opt="--leak-check=full --log-file=$@.valgrind"; \
             printf "\t($(VALGRIND) $$valgrind_opt $@)\n"; \
           else \
             valgrind_opt=""; \
           fi; \
           $(VALGRIND) $$valgrind_opt $@ | tee $@.stdout 2>&1; \
           if [ -f $@.valgrind ]; then \
             printf "$@:\n" > $@.leak.summary 2>&1; \
             grep '=  *[^ ][^ ]* *lost' $@.valgrind | sed 's/^.*= / /' >> $@.leak.summary 2>&1; \
           fi; \
           if [ -n "$(VALGRIND)" ]; then \
             grep '=  *[^ ][^ ]* *lost:  *[^0]' $(EXEDIR)/$$exe.valgrind | sed 's/^.*= / /' > $(EXEDIR)/$$exe.leak.summary 2>&1; \
             grep 'Process terminating' $(EXEDIR)/$$exe.valgrind | sed 's/^.*= / /' >> $(EXEDIR)/$$exe.leak.summary 2>&1; \
             egrep -C 1 '=  *[^ ][^ ]* *\(at|by\)' $(EXEDIR)/$$exe.valgrind | sed 's/^.*= / /' >> $(EXEDIR)/$$exe.leak.summary 2>&1; \
             if [ -s "$(EXEDIR)/$$exe.leak.summary" ]; then \
               cat $(EXEDIR)/$$exe.leak.summary | sed -e 's/^/\t/'; \
               printf "\t***see %s for details\n\n" "$(EXEDIR)/$$exe.valgrind"; \
             else \
               rm -f $(EXEDIR)/$$exe.leak.summary $(EXEDIR)/$$exe.valgrind; \
             fi; \
           fi;)

build_lib:
	@echo ' '
	@echo '***********************************************************'
	@echo '***' Build ${LIBRARY}$(MAJOR_VER) '(' $(CVS_BASE_TAG_NAME) ')'
	@echo '***********************************************************'
	( $(MAKE) MAKEDIRS $(LIBRARY_AR) 2>&1 ) | tee $(LIBDIR)/$(BUILD_LOG) 2>&1

LISTOBJS:
	@echo ' '
	@echo '***' Object files: $(OBJS)
	@echo '***' Executable files: $(EXE)
	@echo ' '

check test: MAKEDIRS
	( $(MAKE) all_child check_child 2>&1 ) | tee ${EXEDIR}/$(BUILD_LOG) 2>&1

check_child:
	@(for exe in $(EXE); do \
             echo ' '; \
             echo '**************************************************************'; \
             echo '***' Execute $$exe example; \
             echo '**************************************************************'; \
             popdir=`pwd`; \
             echo '***' Entering $(EXEDIR); \
             cd $(EXEDIR); \
             if [ -n "$(VALGRIND)" ]; then \
               valgrind_opt="--log-file-exactly=$$exe.valgrind"; \
               printf "\t($(VALGRIND) $$valgrind_opt ./$$exe)\n"; \
             else \
               valgrind_opt=""; \
             fi; \
             $(VALGRIND) $$valgrind_opt ./$$exe | tee $$exe.stdout 2>&1; \
             if [ -f $$exe.valgrind ]; then \
               printf "$$exe:\n" > $$exe.leak.summary 2>&1; \
               grep '=  *[^ ][^ ]* *lost' $$exe.valgrind | sed 's/^.*= / /' >> $$exe.leak.summary 2>&1; \
             fi; \
             echo '***' Leaving $(EXEDIR); \
             cd "$$popdir"; \
          done;)
	@$(MAKE) compare
	@(if [ -n "$(VALGRIND)" ]; then \
            for exe in $(EXE); do \
              grep '=  *[^ ][^ ]* *lost:  *[^0]' $(EXEDIR)/$$exe.valgrind | sed 's/^.*= / /' > $(EXEDIR)/$$exe.leak.summary 2>&1; \
              grep 'Process terminating' $(EXEDIR)/$$exe.valgrind | sed 's/^.*= / /' >> $(EXEDIR)/$$exe.leak.summary 2>&1; \
              egrep -C 1 '=  *(at|by)' $(EXEDIR)/$$exe.valgrind | sed 's/^.*= / /' >> $(EXEDIR)/$$exe.leak.summary 2>&1; \
              if [ -s "$(EXEDIR)/$$exe.leak.summary" ]; then \
                printf "\tMEMORY LEAK SUMMARY:\n"; \
                cat $(EXEDIR)/$$exe.leak.summary; \
              else \
                rm -f $(EXEDIR)/$$exe.leak.summary $(EXEDIR)/$$exe.valgrind; \
              fi; \
            done; \
            printf "\n"; \
          fi;)

compare:
	@-(if [ -s $(EXEDIR)/TestC.stdout ]; then \
             echo ' '; \
             echo '**************************************************************'; \
             echo '***' Compare results to TestC example results; \
             echo '**************************************************************'; \
             cntr=0; \
             for exe in $(EXE); do \
               if [ -s $(EXEDIR)/$$exe.stdout ]; then \
                 if diff -wib $(EXEDIR)/TestC.stdout $(EXEDIR)/$$exe.stdout > /dev/null 2>&1; \
                 then \
                   printf "%15s PASSED: %s and %s are identical\n" \
                           $$exe.stdout "$(EXEDIR)/$$exe.stdout" "$(EXEDIR)/TestC.stdout"; \
                 else \
                   printf "%15s FAILED: %s and %s differ\n" \
                           $$exe.stdout "$(EXEDIR)/$$exe.stdout" "$(EXEDIR)/TestC.stdout"; \
                   cntr=`expr $$cntr + 1`; \
                 fi; \
               else \
                 printf "%15s FAILED: %s is missing or empty\n" \
                         $$exe.stdout "$(EXEDIR)/$$exe.stdout"; \
                 cntr=`expr $$cntr + 1`; \
               fi; \
             done; \
           else \
             echo File $(EXEDIR)/TextC.stdout is missing or empty; \
             cntr=`expr $$cntr + 1`; \
           fi; \
           if [ $$cntr -ne 0 ]; then \
             echo ' '; \
             echo "   *** EXAMPLE(S) FAILED!!! ***"; \
             echo ' '; \
           else \
             echo ' '; \
             echo '   *** ALL EXAMPLES PASSED!!! ***'; \
             echo ' '; \
           fi;)

GET_MOD_FILES:
# this is used to put all *.mod or *.MOD into CWD -- Compaq Dev Studio f90 requires this
	-cp -f $(INCDIR)/*.mod $(INCDIR)/*.MOD .

MAKEDIRS:
	-mkdir -p $(LIBDIR)
	-mkdir -p $(OBJDIR)
	-mkdir -p $(EXEDIR)
	-mkdir -p $(DOCDIR)
	-mkdir -p $(BINDIR)

COPYFILES:
	-cp -f -p $(BASEDIR)/Examples/Makefile $(EXEDIR)
	-cp -f -p $(SRCS_ALL) $(EXEDIR)
	-cp -f -p $(DOCS) $(DOCDIR)

$(LIBRARY_AR):
	@$(MAKE) -C $(BASEDIR)/src

version:
ifeq (0,${MAKELEVEL})
	@$(MAKE) -C $(BASEDIR)/src version
endif

INSTALLED_DOCS_DIR = ${prefix}/docs
INSTALLED_LIBRARY_DIR = ${prefix}/lib/$(ARCH)
INSTALLED_INCLUDE_DIR = ${prefix}/include/$(ARCH)
INSTALLED_EXAMPLE_DIR = ${prefix}/example/$(ARCH)
INSTALLED_BIN_DIR = ${prefix}/bin/$(ARCH)
install: clean MAKEDIRS
	( $(MAKE) DEBUG=-O install_child 2>&1 ) | tee ${EXEDIR}/$(BUILD_LOG) 2>&1
	@cp $(EXEDIR)/$(BUILD_LOG) ${INSTALLED_EXAMPLE_DIR}
	@chmod a+u,o-w ${INSTALLED_EXAMPLE_DIR}/$(BUILD_LOG)
ifneq ($(strip $(INSTALLEDFILESGROUP)),)
	@chgrp $(INSTALLEDFILESGROUP) ${INSTALLED_EXAMPLE_DIR}/$(BUILD_LOG)
endif
	@(if [ -d $(prefix0) -a -w $(prefix0) ]; then \
        mkdir -p $(prefix0)/bin > /dev/null 2>&1; \
        cp $(BASEDIR)/scripts/config.guess $(BASEDIR)/scripts/config.guess.wrapper $(prefix0)/bin; \
        chmod a+u,a+x,o-w $(prefix0)/bin/config.guess $(prefix0)/bin/config.guess.wrapper; \
      fi; \
     )
ifneq ($(strip $(INSTALLEDFILESGROUP)),)
	@(if [ -d $(prefix0) -a -w $(prefix0) ]; then \
        chgrp $(INSTALLEDFILESGROUP) $(prefix0)/bin/config.guess $(prefix0)/bin/config.guess.wrapper; \
      fi; \
     )
endif

install_child: all_child check_child installdirs
	@echo ' '
	@echo '***********************************************************'
	@echo '***' Build utilities
	@echo '***********************************************************'
	@$(MAKE) -C $(BASEDIR)/utils
	@echo ' '
	@echo '***********************************************************'
	@echo '***' Install ${LIBRARY}$(MAJOR_VER) into installation directory:
	@echo '***  ' ${prefix}
	@echo '***********************************************************'
	-rm -rf ${INSTALLED_LIBRARY_DIR}
	-rm -rf ${INSTALLED_INCLUDE_DIR}
	-rm -rf ${INSTALLED_EXAMPLE_DIR}
	-rm -rf ${INSTALLED_BIN_DIR}
	-mkdir -p ${INSTALLED_LIBRARY_DIR}
	-mkdir -p ${INSTALLED_INCLUDE_DIR}
	-mkdir -p ${INSTALLED_EXAMPLE_DIR}
	-mkdir -p ${INSTALLED_BIN_DIR}
	-cp -r -p $(LIBDIR)/* ${INSTALLED_LIBRARY_DIR}
	-cp -r -p $(INCDIR)/* ${INSTALLED_INCLUDE_DIR}
	-cp -r -p $(EXEDIR)/* ${INSTALLED_EXAMPLE_DIR}
	-cp -r -p $(BINDIR)/* ${INSTALLED_BIN_DIR}
	-cp -r -p $(DOCDIR) ${prefix}
	-chmod -R a+u,o-w ${prefix}
ifneq ($(strip $(INSTALLEDFILESGROUP)),)
	-chgrp -R $(INSTALLEDFILESGROUP) ${prefix}
endif
	$(MAKE) list_my_installed_files

list_my_installed_files:
	@echo ' '
	@echo '***********************************************************'
	@echo '***' Installed files list:
	@echo '***********************************************************'
	@-\ls -lFR ${INSTALLED_DOCS_DIR} ${INSTALLED_LIBRARY_DIR} \
                   ${INSTALLED_INCLUDE_DIR} ${INSTALLED_EXAMPLE_DIR} \
                   ${INSTALLED_BIN_DIR}

list_installed_files:
	@echo ' '
	@echo '***********************************************************'
	@echo '***' Installed files list:
	@echo '***********************************************************'
	@-\ls -lFR $(prefix)/lib ${prefix}/include ${prefix}/example \
                    $(prefix)/docs $(prefix)/bin

uninstall:
	@-(if [ -d ${INSTALLED_LIBRARY_DIR} ] || [ -d ${INSTALLED_INCLUDE_DIR} ] \
           || [ -d ${INSTALLED_EXAMPLE_DIR} ] || [ -d ${INSTALLED_BIN_DIR} ] \
           || [ -d ${INSTALLED_DOCS_DIR} ]; then \
             echo ' '; \
             echo '***********************************************************'; \
             echo '***' Uninstall ${LIBRARY}$(MAJOR_VER) '(' $(CVS_BASE_TAG_NAME) '):'; \
             echo '***********************************************************'; \
             rm -rfv ${INSTALLED_LIBRARY_DIR} ${INSTALLED_INCLUDE_DIR} \
                     ${INSTALLED_EXAMPLE_DIR} ${INSTALLED_BIN_DIR}; \
             rmdir -p -v ${prefix}/lib ${prefix}/include ${prefix}/example ${prefix}/bin; \
             echo "find ${prefix} -type f | wc -l 2>&1"; \
             file_cnt=`find ${prefix} -type f | grep -v '${INSTALLED_DOCS_DIR}' | wc -l 2>&1`; \
             echo "remaining file_cnt: $${file_cnt}"; \
             if [ $$file_cnt -eq 0 ]; then \
               rm -rfv ${prefix}; \
             else \
               echo "'$${file_cnt}' != '1'"; \
             fi; \
           fi;)

installdirs:
	@echo ' '
	@echo '***********************************************************'
	@echo '***' Create/Verify ${LIBRARY}$(MAJOR_VER) installation directories.
	@echo '***********************************************************'
	@$(BASEDIR)/scripts/makeinstalldirs $(prefix)/lib ${prefix}/include ${prefix}/example ${prefix}/bin

list_installdirs:
	@(echo $(INSTALLED_DOCS_DIR); \
          echo $(INSTALLED_LIBRARY_DIR); \
          echo $(INSTALLED_INCLUDE_DIR); \
          echo $(INSTALLED_EXAMPLE_DIR); \
          echo $(INSTALLED_BIN_DIR); \
         )

TAGS: $(HEADERS) $(SRCS)
	etags $(HEADERS) $(SRCS)

clean:
	@echo ' '
	@echo '***********************************************************'
	@echo '***' Delete local build $(ARCH) directories.
	@echo '***********************************************************'
	-rm -fr $(EXEDIR) $(DEPDIR) \
                `find . -name 'core*' -o -name '*.stackdump' -o -name '*.lst' -o -name '*.gcov'` $(BUILD_LOG)
	@(if [ -d $(BASEDIR)/src ]; then $(MAKE) -C $(BASEDIR)/src clean; fi)
	@(if [ -d $(BASEDIR)/tests ]; then $(MAKE) -C $(BASEDIR)/tests clean; fi)
	@(if [ -d $(BASEDIR)/utils ]; then $(MAKE) -C $(BASEDIR)/utils clean; fi)

cleanarch:
ifeq (0,${MAKELEVEL})
	@$(MAKE) -C $(BASEDIR) cleanarch
endif

distclean:     distclean_msg rm_include rm_lib rm__OBJ rm_bin rm_example rm_MSVS distclean_src distclean_tests     distclean_utils
	-rm -f $(BUILD_LOG)
	-rm -f `find . -name 'core*' -o -name '*.stackdump' -o -name '*.lst' -o -name '*.gcov'`
	-rm -rf $(DEPDIR)

distcleanfast: distclean_msg rm_include rm_lib rm__OBJ rm_bin rm_example rm_MSVS distclean_src distcleanfast_tests distclean_utils
	-rm -f $(BUILD_LOG)
	-rm -rf $(DEPDIR)

distclean_msg:
	@echo ' '
	@echo '***********************************************************'
	@echo '***' Delete all local build directories.
	@echo '***********************************************************'

rm_%:
	-rm -fr $(BASEDIR)/$*

distclean_%:
	@echo '*** distclean' $*
	-$(MAKE) -C $(BASEDIR)/$* distclean

distcleanfast_%:
	@echo '*** distcleanfast' $*
	-$(MAKE) -C $(BASEDIR)/$* distcleanfast

mostlyclean:
	@echo ' '
	@echo '***********************************************************'
	@echo '***' Delete example, tests and utils build directories.
	@echo '***********************************************************'
	-rm -fr $(BASEDIR)/bin $(BUILD_LOG)
	@(if [ -d $(BASEDIR)/tests ]; then $(MAKE) -C $(BASEDIR)/tests mostlyclean; fi)
	@(if [ -d $(BASEDIR)/utils ]; then $(MAKE) -C $(BASEDIR)/utils mostlyclean; fi)

.PHONY: all all_child LISTOBJS check check_child test MAKEDIRS COPYFILES \
        install install_child uninstall \
        TAGS clean cleanarch distclean mostlyclean installdirs compare list_installed_files \
        list_my_installed_files list_installdirs

.NOTPARALLEL:
