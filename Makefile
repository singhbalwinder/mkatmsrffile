COMPILER = intel

#caseroot directory of a model build
CASEROOT = /compyfs/sing201/e3sm_scratch/SMS.ne30_ne30.FC5AV1C-L.compy_intel.atmsrffile

#common path to model built libraries
CMN_PTH = /compyfs/sing201/e3sm_scratch/SMS.ne30_ne30.FC5AV1C-L.compy_intel.atmsrffile/bld/intel/mvapich2/nodebug/nothreads/mct/
PTH_CSM = mct/noesmf/c1a1l1i1o1r1g1w1e1

#include string
INCSTR      = -I$(CMN_PTH)/include  -I$(CMN_PTH)/gptl -I$(CMN_PTH)/$(PTH_CSM)/include -I$(NETCDF_PATH)/include

#libraries path
LIBDIR_CMN  = $(CMN_PTH)/lib
LIBDIR_CSM  = $(CMN_PTH)/$(PTH_CSM)/lib

include $(CASEROOT)/Macros.make


LIBS = -L$(LIBDIR_CSM) -lcsm_share -L$(LIBDIR_CMN) -lpio -lmct -lmpeu -lgptl -L$(NETCDF_PATH)/lib -lnetcdf -lnetcdff #-L$(LIB_PNETCDF) -lpnetcdf


mkatmsrffile: mkatmsrffile.F90 
	$(MPIFC) $(FFLAGS) $(INCSTR)  $< -o $@  $(LIBS) $(LDFLAGS) 

clean:
	/bin/rm mkatmsrffile

