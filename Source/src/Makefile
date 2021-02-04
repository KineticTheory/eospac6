##################################################################
#  
#  Copyright -- see file named COPYRIGHTNOTICE
#  
##################################################################
### Shared Library Makefile.                                   ###
##################################################################
SHELL = /bin/sh

include ../Makefile.compile_opts

ARFLAGS = r

##################################################################
### Define source and object file names                        ###
### HMA -- added ses_io defines directory
##################################################################
ifeq ($(USE_C),TRUE)
  INTERFACE_SRCS = eos_FC_Interface.c TEST_FUNCTIONS.c

  ifeq ($(SESIO_DEF),)
    # Compile a subset of the ses_io source files
    SESIO_SRC_DIR = $(BASEDIR)/ses_io/src/internals \
                    $(BASEDIR)/ses_io/src/user_interface \
                    $(BASEDIR)/ses_io/src/internals/ascii \
                    $(BASEDIR)/ses_io/src/internals/binary \
                    $(BASEDIR)/ses_io/src/internals/json_parser \
                    $(BASEDIR)/ses_io/src/internals/xml \
                    $(BASEDIR)/ses_io/src/internals/llnl_xml
    SESIO_SRCS  = eos_SesUtils.c $(wildcard $(addsuffix /*.c,$(SESIO_SRC_DIR)))
    SESIO_HDRS  = $(shell find $(SESIO_SRC_DIR) -name '*.h')
  endif
  SESIO_VPATH = $(call uniq,$(dir $(SESIO_SRCS)))
  SESIO_IPATH = $(call uniq,$(dir $(SESIO_HDRS)))
  SESIO_IDIRS = $(if $(SESIO_DEF),,$(addprefix -I ,$(SESIO_IPATH)))
  VPATH += $(SESIO_VPATH)

  SRCS = eos_Access.c \
         eos_RecordType1.c \
         eos_RecordType1Interp.c \
         eos_RecordType2.c \
         eos_RecordType2Interp.c \
         eos_RecordType3.c \
         eos_RecordType4.c \
         eos_RecordType5.c \
         eos_RecordType6.c \
         eos_RecordType6Interp.c \
         eos_Data.c \
         eos_HashTable.c \
         eos_DataMap.c \
         eos_ErrorHandler.c \
         eos_Utils.c \
         eos_UtilsRage.c \
         TEST_FUNCTIONS.c \
         eos_Interpolation.c \
         eos_Taylor.c \
         eos_SaferMemoryAllocation.c \
         eos_Splunk.c \
         $(notdir $(SESIO_SRCS))

  INCS = eos_Interface.f90 eos_Interface2003.f90 eos_InterfaceFortranCommon.f90 \
         eos_Interface.fi \
         eos_universal_types.h \
         eos_Interface.h \
         eos_Interface.proto.h \
         eos_SaferMemoryAllocation.h \
         eos_SaferMemoryAllocation.proto.h \
         $(SESIOINC)/ses_defines.h
endif
ifeq ($(USE_F90),TRUE)
  SRCS_F90 = eos_Interface.f90
  SRCS_F03 = $(notdir $(shell egrep -l eos_Interface2003 $(SRCDIR)/*.f*))
endif

# -----------------------------------------------------------------------------------------------
# Define which source files require AVX optimizations.
# This is necessary to avoid unwanted vectorization in much of the setup code in EOSPAC, that has
#proved to be not easily vectorized. This list may grow as code is reworked in the future.
$(call set,_APPLY_CPUINFO_COMP_FLAGS,eos_Interpolation.c, $(CPUFLAGS))
$(call set,_APPLY_CPUINFO_COMP_FLAGS,eos_RecordType1Interp.c, $(CPUFLAGS))
$(call set,_APPLY_CPUINFO_COMP_FLAGS,eos_RecordType2Interp.c, $(CPUFLAGS))
$(call set,_APPLY_CPUINFO_COMP_FLAGS,eos_RecordType6Interp.c, $(CPUFLAGS))
$(call set,_APPLY_CPUINFO_COMP_FLAGS,eos_Utils.c, $(CPUFLAGS))
# -----------------------------------------------------------------------------------------------

INTERFACE_OBJS = $(INTERFACE_SRCS:%.c=$(OBJDIR)/%.o)
OBJS     = $(SRCS:%.c=$(OBJDIR)/%.o)
OBJS_F90 = $(SRCS_F90:%.f90=$(OBJDIR)/%.f90.o)
OBJS_F03 = $(SRCS_F03:%.f90=$(OBJDIR)/%.f90.o)
OBJS_LIB = $(OBJS:$(OBJDIR)/%=%)

MANGLE_DEF_LIST = MixedCase   \
                  UPPERCASE   \
                  lowercase   \
                  lowercase_  \
                  lowercase__ \
                  _lowercase  \
                  __lowercase

# Create list of mangled interface object file names for each $(MANGLE_DEF_LIST)
MANGLED_OBJS = $(foreach f,$(INTERFACE_OBJS), \
                 $(foreach s,$(MANGLE_DEF_LIST), \
                   $(subst $(basename $(f)),$(basename $(f)).$(s),$f) \
                  ) \
                )

# Create list of mangled interface source file names for each $(MANGLE_DEF_LIST)
MANGLED_SRCS = $(foreach f,$(INTERFACE_SRCS), \
                 $(foreach s,$(MANGLE_DEF_LIST), \
                   $(subst $(basename $(f)),$(CPPDIR)/$(basename $(f)).$(s),$f) \
                  ) \
                )

EOS_VERSION_SRC = $(if $(wildcard eos_version.c),eos_version.c,)

##################################################################
### Create dependency lists, and build object/executable files ###
##################################################################
DEPDIR = $(BASEDIR)/src/.depends
MAKEDEPEND = $(BASEDIR)/scripts/make_depends -o $(OBJDIR) -d $(DEPDIR) -I $(SRCDIR) -I $(SESIOINC) $<

$(OBJDIR)/%.o : $(CPPDIR)/%.c
	$(MAKEDEPEND)
	${CC} -I$(SRCDIR) $(SESIO_IDIRS) ${CFLAGS} $(call get,_APPLY_CPUINFO_COMP_FLAGS,$(notdir $<)) $(MORE_COMPILE_OPTS) $(if $(MANGLE_DEF),-D$(MANGLE_DEF),) $(SESIO_DEF) -c $< -o $@

$(OBJDIR)/%.o : %.c
	$(MAKEDEPEND)
	${CC} ${CFLAGS} $(call get,_APPLY_CPUINFO_COMP_FLAGS,$(notdir $<)) $(MORE_COMPILE_OPTS) $(SESIO_IDIRS) $(if $(MANGLE_DEF),-D$(MANGLE_DEF),) $(SESIO_DEF) -c $< -o $@

$(OBJDIR)/%2003.f90.o : %2003.f90
	$(MAKEDEPEND)
	@which cp
	-cp $< $(INCDIR)
	@-(popdir=`pwd`; cd $(INCDIR); echo 'popdir:' $$popdir; echo 'CWD:' `pwd`; set -x; \
         ${F90} ${F90FLAGS} $(call get,_APPLY_CPUINFO_COMP_FLAGS,$(notdir $<)) -I"$$popdir" $(if $(MANGLE_DEF),-D$(MANGLE_DEF),) $(SESIO_DEF) -c "$<" -o "$@" )
	@-if [ -f $(OBJDIR)/*.[Mm][Oo][Dd] ]; then set -x; mv $(OBJDIR)/*.[Mm][Oo][Dd] $(INCDIR); fi

$(OBJDIR)/%.f90.o : %.f90
	$(MAKEDEPEND)
	@which cp
	-cp $< $(INCDIR)
	@(popdir=`pwd`; cd $(INCDIR); echo 'popdir:' $$popdir; echo 'CWD:' `pwd`; set -x; \
         ${F90} ${F90FLAGS} $(call get,_APPLY_CPUINFO_COMP_FLAGS,$(notdir $<)) -I"$$popdir" $(if $(MANGLE_DEF),-D$(MANGLE_DEF),) $(SESIO_DEF) -c "$<" -o "$@" )
	@-if [ -f $(OBJDIR)/*.[Mm][Oo][Dd] ]; then set -x; mv $(OBJDIR)/*.[Mm][Oo][Dd] $(INCDIR); fi

##################################################################
### Target definitions                                         ###
##################################################################
all: MAKEDIRS
	@( $(MAKE) COMPILER_INFO 2>&1 ) | tee $(LIBDIR)/compiler_info.txt
	( $(MAKE) all_child 2>&1 ) | tee $(LIBDIR)/$(BUILD_LOG)

all_child:
	@-echo ' '
	@-echo '***********************************************************'
	@-echo '***' Compiling library object files '(' $(CVS_BASE_TAG_NAME) ')'
	@-echo '***********************************************************'
	$(MAKE) $(OBJS_F90) $(OBJS_F03) $(LIBRARY_AR)

$(INTERFACE_OBJS): $(INTERFACE_SRCS)

LIST_MANGLED_DEPS:
	@(echo 'MANGLED_OBJS dependencies ...'; set $(strip $(MANGLED_SRCS)); \
          for f in $(MANGLED_OBJS); do \
            echo $$f : $$1; \
            shift; \
          done;)

$(MANGLED_OBJS): $(MANGLED_SRCS)

$(MANGLED_SRCS): %.c : $(INTERFACE_SRCS)
	@echo '----- Creating' $@
	@echo '#define' $(subst .,,$(suffix $*)) > $@
	@(if echo $@ | grep TEST_FUNCTIONS > /dev/null; then \
        (echo '#define EOSPAC6_TEST_FUNCTIONS_MANGLED' >> $@; ) \
      fi;)
   ifneq ($(EOS_VERSION_SRC),)
	@echo '#define EOSPAC6_VERSION_FUNCTIONS_REDEFINED' >> $@
	@(if [ "$(strip $(firstword $(MANGLE_DEF_LIST)))" = "$(strip $(subst .,,$(suffix $*)))" ]; then \
         echo '#define _EOS_INCLUDE_SPECIAL_FUNCTION' >> $@; \
      fi;)
   endif
	@(f=`basename $@ | sed -e 's/\\([^\\.]*\\)\\.[a-zA-Z0-9_][a-zA-Z0-9_]*\\(\\..*\\)$$/\\1\\2/'`; \
      if echo $@ | grep eos_FC_Interface > /dev/null; then \
        sed 's/__STATIC_MANGLE_NAME__/$(subst .,,$(suffix $*))/' $$f $(EOS_VERSION_SRC) >> $@; \
      else \
        sed 's/__STATIC_MANGLE_NAME__/$(subst .,,$(suffix $*))/' $$f >> $@; \
      fi;)

SESIO_OBJ_TMP_DIR = .sesio.tmp.dir
$(LIBRARY_AR): MAKEDIRS check_lib LIST_MANGLED_DEPS $(OBJS) #$(if $(SESIO_DEF),libsesio,)
	$(MAKE) $(INTERFACE_OBJS) $(MANGLED_OBJS)
ifneq ($(MAKE_WINLIB_AR),)
	$(MAKE) $(MAKE_WINLIB_AR)
endif
	@-echo ' '
	@-echo '***********************************************************'
	@-echo '***' Building static library: $(LIBRARY_AR)
	@-echo '***********************************************************'
	@$(AR) $(ARFLAGS) $(LIBDIR)/$(LIBRARY_AR) $(OBJS_F90) `ls $(OBJS_F03) 2>$(JUNKFILE)` $(OBJS) $(MANGLED_OBJS)
	@printf "%s" $(LIBDIR)/$(LIBRARY_AR); if [ -f $(LIBDIR)/$(LIBRARY_AR) ]; then echo ' ... OK'; else echo ' ... FAILED'; fi
	rm -rf $(SRCDIR)/$(SESIO_OBJ_TMP_DIR)
	$(RANLIB) $(LIBDIR)/$(LIBRARY_AR)
	cp $(INCS) $(INCDIR)
	@-(if [ ! -f $(INCDIR)/*2003.[Mm][Oo][Dd] -o ! -f $(OBJDIR)/*2003.f90.o ]; then \
             echo; \
             echo '*** WARNING: You are not using a Fortran 2003 compliant compiler!'; \
             echo '             Therefore, the $(basename $(SRCS_F03)) module was not created/included.'; \
             echo; \
             rm -f $(INCDIR)/*2003.[Mm][Oo][Dd] $(OBJDIR)/*2003.f90.o; \
           fi; \
          )

libsesio:
	$(MAKE) -C $(BASEDIR)/ses_io/src libses.a
	pwd
	(if [ ! -d $(SRCDIR)/$(SESIO_OBJ_TMP_DIR) ]; then \
           mkdir $(SRCDIR)/$(SESIO_OBJ_TMP_DIR); \
         fi; \
	 if [ -d $(SRCDIR)/$(SESIO_OBJ_TMP_DIR) ]; then \
           cd $(SRCDIR)/$(SESIO_OBJ_TMP_DIR); \
           $(AR) x $(BASEDIR)/ses_io/src/libses.a; \
           cd ..; \
         fi; \
        )

$(WINLIB_AR):
	@-echo ' '
	@-echo '***********************************************************'
	@-echo '***' Building Windows compatible static library: $(WINLIB_AR)
	@-echo '***********************************************************'
	-(loc=`pwd`; \
          popdir=`cygpath -wa $$loc`; \
          cd $(OBJDIR); \
          lib -out:$(WINLIB_AR) \
             $(foreach obj,$(strip $(OBJS_F90) $(OBJS) $(MANGLED_OBJS)),"$$popdir/$(obj)"); \
          mv -f $(WINLIB_AR) "$$popdir/$(LIBDIR)"; \
         )

version:
	@-echo ' '
	@-echo '***********************************************************'
	@-echo '***' Creating version query interface functions.
	@-echo '***********************************************************'
	@-rm -f eos_version.c
	-perl "$(BASEDIR)/scripts/eos_version.pl"
	@-cat eos_version.c

install: clean all
ifeq (0,${MAKELEVEL})
	@$(MAKE) -C $(BASEDIR)/Examples install
endif

list_my_installed_files:
ifeq (0,${MAKELEVEL})
	@$(MAKE) -C $(BASEDIR)/Examples list_my_installed_files
endif

list_installed_files:
ifeq (0,${MAKELEVEL})
	@$(MAKE) -C $(BASEDIR)/Examples list_installed_files
endif

uninstall:
ifeq (0,${MAKELEVEL})
	@$(MAKE) -C $(BASEDIR)/Examples uninstall
endif

check_lib:
	-(cd $(OBJDIR); \
          $(AR) xo $(LIBDIR)/$(LIBRARY_AR); \
          )
MAKEDIRS:
	mkdir -p $(LIBDIR)
	mkdir -p $(INCDIR)
	mkdir -p $(OBJDIR)
	mkdir -p $(CPPDIR)
	@-touch $(BASE_CPPDIR)/.distclean

TAGS: $(HEADERS) $(SRCS)
	etags $(HEADERS) $(SRCS)

clean:
ifeq (0,${MAKELEVEL})
	@$(MAKE) -C $(BASEDIR)/Examples clean
else
	-rm -fr $(INCDIR) $(DEPDIR) $(OBJDIR) $(LIBDIR) \
                $(MANGLED_SRCS) core* *.stackdump *.lst *.gcov $(JUNKFILE) $(BUILD_LOG) eos_version.c
endif
	-$(MAKE) -C $(BASEDIR)/ses_io/src clean

cleanarch:
ifeq (0,${MAKELEVEL})
	@$(MAKE) -C $(BASEDIR) cleanarch
endif

ifeq (0,${MAKELEVEL})
distclean:
	@$(MAKE) -C $(BASEDIR)/Examples distclean

distcleanfast:
	@$(MAKE) -C $(BASEDIR)/Examples distcleanfast
else

DISTCLEAN_DIRS = $(patsubst %/.distclean,%,$(wildcard $(BASEDIR)/src/*/.distclean))
DISTCLEAN_SUBDIRS = $(notdir $(DISTCLEAN_DIRS))

distclean: $(addprefix distclean_,$(DISTCLEAN_SUBDIRS))
	-rm -fr $(DEPDIR) $(JUNKFILE) $(BUILD_LOG) core* *.stackdump *.lst eos_version.c

distclean_%:
	@(loc=`pwd`; \
          set -x; \
          rm -rf $$loc/$*; \
         )
endif

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

.PHONY: all all_child install uninstall check_lib MAKEDIRS TAGS clean \
        cleanarch distclean COMPILER_INFO \
        list_installed_files list_my_installed_files

-include $(SRCS:%.c=$(DEPDIR)%.d)
