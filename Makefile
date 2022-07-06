# makefile for BIOME4, summer 2020

FC=gfortran
FCFLAGS  = 

# should not need to modify anything below this line

#---------------------------------------------

CPPFLAGS := $(shell nf-config --cflags)
LDFLAGS  := $(shell nf-config --flibs)

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
	$(FC) $(FCFLAGS) -o biome4 $(OBJS) $(LDFLAGS)

clean::	
	-rm *.o *.mod
