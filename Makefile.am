ACLOCAL_AMFLAGS = -I m4

bin_PROGRAMS = biome4

biome4_SOURCES =     \
  parametersmod.f90  \
  netcdfmod.f90      \
  coordsmod.f90      \
  biome4.f           \
  biome4driver.f90

clean-local:
	rm -r -f *.mod *.dSYM
