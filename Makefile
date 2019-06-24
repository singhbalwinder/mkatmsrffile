FC := ifort
PNETCDF_PATH := $(PNETCDF_HOME)
NETCDF_PATH := $(NETCDF_HOME)

EXE := exe_mkatmsrffile

#(DBG FLAS=GS: 1 for ON and 0 for OFF)
DBG = 1

#---------------------------
# USER INPUT ENDS HERE
#---------------------------


#include string
INC  = -I$(NETCDF_ROOT)/include
LIB = -L$(NETCDF_ROOT)/lib -lnetcdf -lnetcdff

FFLAGS += -O2
ifeq ($(DBG), 1)
	FFLAGS += -g -traceback  -O0 -fpe0 -check  all -check noarg_temp_created -ftrapuv -init=snan
endif


$(EXE): mkatmsrffile.F90 
	$(FC) $(FFLAGS) $(INC)  $< -o $@  $(LIB)

clean:
	/bin/rm $(EXE)

