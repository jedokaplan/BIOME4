# BIOME4

## Introduction

This is the BIOME4 equilibrium global vegetation model, that was first used in experiments described in Kaplan et al. (2003). The computational core of the model was last updated in 1999, and at the time was called BIOME4 v4.2b2. 

This GitHub repository contains the original FORTRAN77 computational core, a modernized driver program writen in Modern Fortran, a makefile, and a joboptions namelist file. The model input and output are netCDF files. Job options that name the input files and the atmospheric CO₂ concentration for the run are contained in a Fortran namelist file. A sample namelist `globalrun.namelist` is included in this archive. 

BIOME4 requires the following variables to run (in the form of gridded fields):
- climatological monthly mean fields of temperature (°C)
- climatological monthly mean cloud cover (%)
- climatological mean monthly total precipitation (mm)
- soil water holding capacity in two or more layers (mm/mm)
- soil saturated conductivity in two or more layers (mm/h)

BIOME4 also requires a single global value for atmospheric CO₂ concentrations

The following input variables are optional:
- climatological absolute minimum temperature (°C) (will be estimated using a regression function based on mean temperature if not present)
- grid cell elevation above sea level (m) (will be set to sea level if not present)

The gridded input data can be at any resolution, but this version of the driver expects the input fields to be in unprojected (lon-lat) rasters.

This archive does not contain input files. The netCDF input file format will be described in this readme in a future update of this README.

## Usage

To run BIOME4 the following three arguments need to be specified on the command line following the executable _in exactly this order_:

1) the name of the job options namelist file
2) the geographic coordinates for the run in minlon/maxlon/minlat/maxlat format, e.g. -180/180/-90/90 for a global simulation.
3) the name of the output file, e.g., output.nc
   
Example: `./biome4 globalrun.namelist -180/180/-90/90 output.nc`

## Reference

When using BIOME4, please include the following citation:

Kaplan, J. O., Bigelow, N. H., Prentice, I. C., Harrison, S. P., Bartlein, P. J., Christensen, T. R., Cramer, W., Matveyeva, N. V., McGuire, A. D., Murray, D. F., Razzhivin, V. Y., Smith, B., Walker, D. A., Anderson, P. M., Andreev, A. A., Brubaker, L. B., Edwards, M. E., & Lozhkin, A. V. (2003). Climate change and Arctic ecosystems: 2. Modeling, paleodata-model comparisons, and future projections. _Journal of Geophysical Research-Atmospheres_, 108(D19). doi:10.1029/2002jd002559
