COMPILER = intel

#(DBG variable: 1 for ON and 0 for OFF)
DBG = 1

#from env_build.xml (NINST_VALUE)
NINST_VALUE = c1a1l1i1o1r1g1w1e1


#caseroot directory of a model build
CASEROOT = /compyfs/sing201/e3sm_scratch/SMS.ne30_ne30.FC5AV1C-L.compy_intel.atmsrffile

#Build directory
BLDDIR = /compyfs/sing201/e3sm_scratch/SMS.ne30_ne30.FC5AV1C-L.compy_intel.atmsrffile/bld

#common path to model built libraries
CMN_PTH = $(BLDDIR)/$(COMPILER)/mvapich2/nodebug/nothreads/mct

#path to CSM
PTH_CSM = mct/noesmf/$(NINST_VALUE)

#---------------------------
# USER INPUT ENDS HERE
#---------------------------


#include string
INCSTR      = -I$(CMN_PTH)/include  -I$(CMN_PTH)/gptl -I$(CMN_PTH)/$(PTH_CSM)/include -I$(NETCDF_ROOT)/include

#libraries path
LIBSTR_CMN  = -L$(CMN_PTH)/lib
LIBSTR_CSM  = -L$(CMN_PTH)/$(PTH_CSM)/lib

include $(CASEROOT)/Macros.make
FFLAGS = -O2
ifeq ($(DBG), 1)
   # FFLAGS is getting its value from Macros.make file above
	FFLAGS = -g -traceback  -O0 -fpe0 -check  all -check noarg_temp_created -ftrapuv -init=snan
endif


LIBS = -lpmi $(LIBSTR_CSM) -lcsm_share $(LIBSTR_CMN) -lpio -lmct -lmpeu -lgptl -L$(NETCDF_ROOT)/lib -lnetcdf -lnetcdff 


mkatmsrffile: mkatmsrffile.F90 
	$(MPIFC) $(FFLAGS) $(INCSTR)  $< -o $@  $(LIBS)

clean:
	/bin/rm mkatmsrffile

