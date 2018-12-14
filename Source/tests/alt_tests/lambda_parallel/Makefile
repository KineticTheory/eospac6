##################################################################
#  
#  Copyright -- see file named COPYRIGHTNOTICE
#  
##################################################################
NPES        = 1
F77         = mpif77
F90         = mpif90
CC          = mpicc
CXX         = mpiCC
F77FLAGS    = -g
F90FLAGS    = -g
CCFLAGS     = -g
CXXFLAGS    = -g
MPI_INCLUDE = -I$(MPICH_ROOT)/include
MPI_LIBS    = -L$(MPICH_ROOT)/lib -lmpich
LIBS        = -L. -leospac $(MPI_LIBS) -lm

DISTNAME  = matptest
CXXOBJ    = Ptest.o Comm.o matptest.o

EXE       = testf77.x testf90.x testc.x testcpp.x

all:  eospac6 ${DISTNAME}.x

eospac6:
	-$(MAKE) -C ../../src

$(DISTNAME).x: ${CXXOBJ}
	@-echo '-------------------------------------------------'
	@echo "Creating executable $@"
	$(CXX) -o $@ $(CXXFLAGS) ${CXXOBJ} $(LIBS)

testf77.x: testf77.o
	@-echo '-------------------------------------------------'
	@echo "Creating executable $@"
	$(F77) -o $@ $(F77FLAGS) $(LIBS) $<

testf90.x: testf90.o
	@-echo '-------------------------------------------------'
	@echo "Creating executable $@"
	$(F90) -o $@ $(F90FLAGS) $(LIBS) $<

testc.x: testc.o
	@-echo '-------------------------------------------------'
	@echo "Creating executable $@"
	$(CC) -o $@ $(CCFLAGS) $(LIBS) $<

testcpp.x: testcpp.o
	@-echo '-------------------------------------------------'
	@echo "Creating executable $@"
	$(CXX) -o $@ $(CXXFLAGS) ${CXXOBJ} $(LIBS) $<

.SUFFIXES :
.SUFFIXES : .o .h .c .cc .f .f90 .d .x

%.o:    %.cc
	$(CXX) -c $(CXXFLAGS) $(MPI_INCLUDE) $< -o $@

%.o:    %.c
	$(CC) -c $(CCFLAGS) $(MPI_INCLUDE) $< -o $@

%.o:    %.f
	$(F77) -c $(F77FLAGS) $(MPI_INCLUDE) $< -o $@

%.o:    %.f90
	$(F90) -c $(F90FLAGS) $(MPI_INCLUDE) $< -o $@

check: clean all
	@echo "*** Executing $@"
	-mpirun -np $(NPES) $(DISTNAME).x

helloworld: override CXXOBJ=
helloworld: $(EXE)
	@-echo '-------------------------------------------------'
	@-(for f in $(EXE); do \
           echo '*** Executing' $$f; \
           mpirun -np $(NPES) $$f; \
         done;)
	@-echo '-------------------------------------------------'
	@-echo '*** MODULES:'
	@-/usr/local/modules/$$MODULE_VERSION/bin/modulecmd tcsh list
	@-echo '-------------------------------------------------'
	@-echo '*** ENVIRONMENT:'
	@-env
	@-echo '-------------------------------------------------'

debug: all
	-mpirun -tv -np $(NPES) $(DISTNAME).x

clean:
	-rm -f *.x *.o
