# BIOME4

This is the BIOME4 equilibrium global vegetation model, that was first used in experiments described in Kaplan et al. (2003). The computational core of the model was last updated in 1999, and at the time was called BIOME4 v4.2b2. 

This GitHub repository contains the original FORTRAN77 computational core, a modernized driver program writen in Modern Fortran, a makefile, and a joboptions namelist file. The model input and output are netCDF files. Job options that name the input files and the atmospheric CO2 concentration for the run are contained in a Fortran namelist file. A sample namelist `globalrun.namelist` is included in this archive. 

BIOME4 requires the following variables to run:
- climatological monthly mean fields of temperature (degC)
- climatological monthly mean cloud cover fraction
- climatological mean monthly total precipitation (mm)
- soil water holding capacity in two or more layers (mm/mm)
- soil saturated conductivity in two or more layers (mm/h)

The following input variable is optional and can be estimated using a regression function
- climatological absolute minimum temperature (degC)

The input data can be at any resolution, but this version of the driver expects the input fields to be in unprojected (lon-lat) rasters.

This archive does not contain input files. The netCDF input file format will be described in this readme in a future update of this README.

Reference

Kaplan, J. O., Bigelow, N. H., Prentice, I. C., Harrison, S. P., Bartlein, P. J., Christensen, T. R., Cramer, W., Matveyeva, N. V., McGuire, A. D., Murray, D. F., Razzhivin, V. Y., Smith, B., Walker, D. A., Anderson, P. M., Andreev, A. A., Brubaker, L. B., Edwards, M. E., & Lozhkin, A. V. (2003). Climate change and Arctic ecosystems: 2. Modeling, paleodata-model comparisons, and future projections. _Journal of Geophysical Research-atmospheres_, 108(D19). doi:10.1029/2002jd002559
