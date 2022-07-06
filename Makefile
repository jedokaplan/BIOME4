# makefile for BIOME4, summer 2020

FC=mpiifort
FCFLAGS  = 

netcdf=/apps/netcdf/4.4.4-fortran

# should not need to modify anything below this line

#---------------------------------------------

NC_LIB=$(netcdf)/lib
NC_INC=$(netcdf)/include

CPPFLAGS = -I$(NC_INC)
LDFLAGS  = -L$(NC_LIB)
LIBS     = -lnetcdff

#---------------------------------------------

OBJS = f90getopt.o      \
       parametersmod.o  \
       netcdfmod.o      \
       coordsmod.o      \
       biome4.o         \
       biome4driver.o

#---------------------------------------------

.SUFFIXES: .o .f90 .F90 .f .mod

%.o : %.c
	$(CC) $(CFLAGS) -c -o $(*F).o $(CPPFLAGS) $<

%.o : %.f
	$(FC) $(FCFLAGS) -c -o $(*F).o $(CPPFLAGS) $<

%.o : %.f90
	$(FC) $(FCFLAGS) -c -o $(*F).o $(CPPFLAGS) $<

%.o : %.F90
	$(FC) $(FCFLAGS) -c -o $(*F).o $(CPPFLAGS) $<

all::	biome4

biome4: $(OBJS)
	$(FC) $(FCFLAGS) -o biome4 $(OBJS) $(LDFLAGS) $(LIBS)

clean::	
	-rm *.o *.mod
