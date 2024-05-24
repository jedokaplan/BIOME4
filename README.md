# BIOME4

## Introduction

This is the BIOME4 equilibrium global vegetation model, that was first used in experiments described in Kaplan et al. (2003). The computational core of the model was last updated in 1999, and at the time was called BIOME4 v4.2b2. 

This GitHub repository contains the original FORTRAN77 computational core, a modernized driver program writen in Modern Fortran, a makefile, and a joboptions namelist file. The model input and output are netCDF files. Job options that name the input files and the atmospheric CO₂ concentration for the run are contained in a Fortran namelist file. A sample namelist `globalrun.namelist` is included in this archive. 

## Usage

To run BIOME4 the following three arguments need to be specified on the command line following the executable _in exactly this order_:

1) the name of the job options namelist file
2) the geographic coordinates for the run in minlon/maxlon/minlat/maxlat format, e.g. -180/180/-90/90 for a global simulation.
3) the name of the output file, e.g., output.nc
   
Example: `./biome4 globalrun.namelist -180/180/-90/90 output.nc`

## Input data
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

A sample input data file `BIOME4_inputdata.nc` in netCDF version 4 format is now provided as part of this repository. The sample input data file has the header shown below. The netCDF-4 special attributes `_ChunkSizes`, `_Shuffle`, `_DeflateLevel`, `_Endianness`, and `_NoFill` are not required for operation of the model but compression and specifying chunk sizes do help save space on disk.

```
netcdf BIOME4_inputdata {
dimensions:
	lon = 720 ;
	lat = 360 ;
	time = 12 ;
	soil_layer = 2 ;
variables:
	float lon(lon) ;
		lon:long_name = "longitude" ;
		lon:units = "degrees_east" ;
		lon:missing_value = -9999.f ;
		lon:_Storage = "chunked" ;
		lon:_ChunkSizes = 720 ;
		lon:_Shuffle = "true" ;
		lon:_DeflateLevel = 1 ;
		lon:_Endianness = "little" ;
		lon:_NoFill = "true" ;
	float lat(lat) ;
		lat:long_name = "latitude" ;
		lat:units = "degrees_north" ;
		lat:missing_value = -9999.f ;
		lat:_Storage = "chunked" ;
		lat:_ChunkSizes = 360 ;
		lat:_Shuffle = "true" ;
		lat:_DeflateLevel = 1 ;
		lat:_Endianness = "little" ;
		lat:_NoFill = "true" ;
	int time(time) ;
		time:long_name = "time" ;
		time:units = "month" ;
		time:missing_value = -9999 ;
		time:_Storage = "chunked" ;
		time:_ChunkSizes = 12 ;
		time:_Shuffle = "true" ;
		time:_DeflateLevel = 1 ;
		time:_Endianness = "little" ;
		time:_NoFill = "true" ;
	int soil_layer(soil_layer) ;
		soil_layer:long_name = "soil layer, 0-30cm, 30cm-bottom" ;
		soil_layer:units = "layer" ;
		soil_layer:missing_value = -9999 ;
		soil_layer:_Storage = "chunked" ;
		soil_layer:_ChunkSizes = 2 ;
		soil_layer:_Shuffle = "true" ;
		soil_layer:_DeflateLevel = 1 ;
		soil_layer:_Endianness = "little" ;
		soil_layer:_NoFill = "true" ;
	short temp(time, lat, lon) ;
		temp:long_name = "monthly mean temperature" ;
		temp:units = "degC" ;
		temp:missing_value = -9999s ;
		temp:_Storage = "chunked" ;
		temp:_ChunkSizes = 1, 360, 720 ;
		temp:_Shuffle = "true" ;
		temp:_DeflateLevel = 1 ;
		temp:_Endianness = "little" ;
		temp:_NoFill = "true" ;
	short prec(time, lat, lon) ;
		prec:long_name = "monthly total precipitation" ;
		prec:units = "mm" ;
		prec:missing_value = -9999s ;
		prec:_Storage = "chunked" ;
		prec:_ChunkSizes = 1, 360, 720 ;
		prec:_Shuffle = "true" ;
		prec:_DeflateLevel = 1 ;
		prec:_Endianness = "little" ;
		prec:_NoFill = "true" ;
	short sun(time, lat, lon) ;
		sun:long_name = "mean monthly percent of possible sunshine" ;
		sun:units = "percent" ;
		sun:missing_value = -9999s ;
		sun:_Storage = "chunked" ;
		sun:_ChunkSizes = 1, 360, 720 ;
		sun:_Shuffle = "true" ;
		sun:_DeflateLevel = 1 ;
		sun:_Endianness = "little" ;
		sun:_NoFill = "true" ;
	float whc(soil_layer, lat, lon) ;
		whc:long_name = "soil water holding capacity" ;
		whc:units = "mm/m" ;
		whc:missing_value = -9999.f ;
		whc:_Storage = "chunked" ;
		whc:_ChunkSizes = 1, 360, 720 ;
		whc:_Shuffle = "true" ;
		whc:_DeflateLevel = 1 ;
		whc:_Endianness = "little" ;
		whc:_NoFill = "true" ;
	float perc(soil_layer, lat, lon) ;
		perc:long_name = "soil water percolation index" ;
		perc:units = "mm/hr" ;
		perc:missing_value = -9999.f ;
		perc:_Storage = "chunked" ;
		perc:_ChunkSizes = 1, 360, 720 ;
		perc:_Shuffle = "true" ;
		perc:_DeflateLevel = 1 ;
		perc:_Endianness = "little" ;
		perc:_NoFill = "true" ;
	short tmin(lat, lon) ;
		tmin:long_name = "annual absolute mimimum temperature" ;
		tmin:units = "degC" ;
		tmin:missing_value = -9999s ;
		tmin:_Storage = "chunked" ;
		tmin:_ChunkSizes = 360, 720 ;
		tmin:_Shuffle = "true" ;
		tmin:_DeflateLevel = 1 ;
		tmin:_Endianness = "little" ;
		tmin:_NoFill = "true" ;

// global attributes:
		:title = "BIOME4 comprehensive driver dataset, 0.5 degree, v1.0" ;
		:_NCProperties = "version=2,netcdf=4.9.2,hdf5=1.14.0" ;
		:_SuperblockVersion = 2 ;
		:_IsNetcdf4 = 1 ;
		:_Format = "netCDF-4" ;
}
```
## Reference

When using BIOME4, please include the following citation:

Kaplan, J. O., Bigelow, N. H., Prentice, I. C., Harrison, S. P., Bartlein, P. J., Christensen, T. R., Cramer, W., Matveyeva, N. V., McGuire, A. D., Murray, D. F., Razzhivin, V. Y., Smith, B., Walker, D. A., Anderson, P. M., Andreev, A. A., Brubaker, L. B., Edwards, M. E., & Lozhkin, A. V. (2003). Climate change and Arctic ecosystems: 2. Modeling, paleodata-model comparisons, and future projections. _Journal of Geophysical Research-Atmospheres_, 108(D19). doi:10.1029/2002jd002559
