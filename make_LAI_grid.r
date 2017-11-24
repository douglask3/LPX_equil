##################################
## cfg							##
##################################
source("cfg.r")

## LPX output with height, gdd and fpc
fname_in = '/Users/dougl/Dropbox/LPX_storage_shed/Equilibrium Test 2 PI - 20170912 (CASP)/test_R20C_CRUnonclim_PI1-4099.nc'

## constructed biome output file (for nc and geotiff)
fname_out = 'outputs/lai_grid'

## varnames in fname_in
varnames = c(fpc = 'fpc_grid', lai = 'lai_ind')


##################################
## open							##
##################################
fpc    = stack(fname_in, varname = varnames['fpc'   ])
lai    = stack(fname_in, varname = varnames['lai'   ])

#################################
## Area weight LAI			   ##
#################################
lai_grid =  layer.apply(1:9, function(i) lai[[i]] * fpc[[i]])
lai_grid = sum(lai_grid)
lai_grid[lai_grid > 9E9] = NaN


##################################
## output    					##
##################################
writeRaster.gitInfo(lai_grid, paste(fname_out, '.nc', sep = ""),
				    varname = 'lai_grid', comment = "sum of lai_ind weighted by fpc_grid from LPX output files", overwrite = TRUE)

## geotiff file
writeRaster(lai_grid, paste(fname_out, gitVersionNumber(), '.tiff', sep = "-"),
			format = "GTiff", datatype='FLT4S', options = c("COMPRESS = NONE", "PROFILE = BASELINE"), 
			overwrite = TRUE)