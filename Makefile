# makefile for BIOME4, summer 2020

FC=gfortran
FCFLAGS  = 

# use the command "nf-config --all" to find the location of your netCDF installation
# and enter the path next to " --prefix    ->" on the line below

netcdf=/home/public/easybuild/software/netCDF-Fortran/4.5.4-gompi-2021b

# should not need to modify anything below this line

#---------------------------------------------

NC_LIB=$(netcdf)/lib
NC_INC=$(netcdf)/include

CPPFLAGS = -I$(NC_INC)
LDFLAGS  = -L$(NC_LIB)
LIBS     = -lnetcdff

#---------------------------------------------

OBJS = parametersmod.o  \
       netcdfmod.o      \
       coordsmod.o      \
       biome4.o         \
       biome4driver.o

#---------------------------------------------

.SUFFIXES: .o .f90 .f .mod

%.o : %.c
	$(CC) $(CFLAGS) -c -o $(*F).o $(CPPFLAGS) $<

%.o : %.f
	$(FC) $(FCFLAGS) -c -o $(*F).o $(CPPFLAGS) $<

%.o : %.f90
	$(FC) $(FCFLAGS) -c -o $(*F).o $(CPPFLAGS) $<

all::	biome4

biome4: $(OBJS)
	$(FC) $(FCFLAGS) -o biome4 $(OBJS) $(LDFLAGS) $(LIBS)

clean::	
	-rm *.o *.mod
