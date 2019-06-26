#-------------------------------------
# Makefile for mkatmsrffile tool 
#-------------------------------------

#-------------------------------------
#Instructions:
# Load a compiler and the associated 
# netcdf library. Update variables "FC"
# and "NETCDF_PATH" to add the compiler
# and path to netcdf home directory
#-------------------------------------

FC := ifort
NETCDF_PATH := $(NETCDF_HOME)

EXE := exe_mkatmsrffile

#To turn on debug compile, set DBG to 1 otherwise 0
DBG = 1

#---------------------------
# USER INPUT ENDS HERE
#---------------------------

#include string
INC = -I$(NETCDF_ROOT)/include
LIB = -L$(NETCDF_ROOT)/lib -lnetcdf -lnetcdff

FFLAGS = -O2
ifeq ($(DBG), 1)
	FFLAGS = -g -traceback  -O0 -fpe0 -check  all -check noarg_temp_created -ftrapuv -init=snan
endif

$(EXE): mkatmsrffile.F90 
	$(FC) $(FFLAGS) $(INC)  $< -o $@  $(LIB)

clean:
	/bin/rm $(EXE)

