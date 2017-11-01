fname_in = '/Users/dougl/Dropbox/LPX_storage_shed/Equilibrium Test 2 PI - 20170912 (CASP)/test_R20C_CRUnonclim_PI1-4099.nc'

fname_out = 'outputs/biomeOut'

varnames = c(height = 'height', fpc = 'fpc_grid', gdd = 'gdd_grid')

cols = c('#114400', '#005555', '#00EE11', '#000088',
		 '#AA5500', '#777922', '#66DD88', '#22EEFF',
		 '#FF9922', '#FEFF44', '#AA00FF', '#FFBAAA')

height = stack(fname_in, varname = varnames['height'])
fpc    = stack(fname_in, varname = varnames['fpc'   ])
gdd    = stack(fname_in, varname = varnames['gdd'   ])

biome_assignment <- function(fpc, height, gdd = NULL,
							 gdd_threshold = 350, veg_treshold = c(0.6, 0.3),
							 height_threshold = 10) {

	biome_key = cbind(c(1:12), 
	                  c('Tropical Forest', 'Warm Temperate Forest', 'Temperate Forest',
							'Boreal Forest',
						'Tropical Savannah', 'Sclerophyll Woodland', 'Temperate Parkland',
							'Boreal Parkland',
						'Dry Grass or Shrub', 'Hot Desert', 'Shrub Tundra', 'Tundra'))
		
	height = sum(height * fpc)
	vegCover = sum(fpc)
	
	warm     = gdd > gdd_threshold
	
	wood     = vegCover > veg_treshold[1]
	arid     = vegCover < veg_treshold[2]
	grass    = !wood & ! arid
	
	tropical = sum(fpc[[1:2]]) > 0
	warmTemp = fpc[[4]] > vegCover/2 & !tropical
	coldTemp = sum(fpc[[c(3,5)]]) > 0 & !tropical & !warmTemp
	boreal = !tropical & !warmTemp & !coldTemp
	
	tall     =  height > height_threshold
	
	biome = height
	biome[] = 0.0
	biome[        wood & tropical &  tall] = 1
	biome[ warm & wood & tropical & !tall] = 5
	biome[        wood & warmTemp &  tall] = 2
	biome[ warm & wood & warmTemp & !tall] = 6
	biome[        wood & coldTemp &  tall] = 3
	biome[ warm & wood & coldTemp & !tall] = 7
	biome[        wood & boreal   &  tall] = 4
	biome[        wood & boreal   & !tall] = 8
	biome[ warm & grass                  ] = 9
	biome[ warm & arid                   ] = 10
	biome[!warm                   & !tall] = 11
	biome[!warm & !wood                  ] = 12
	
	biome[height > 9E9| height == -999] = NaN
	
	return(list(biome, biome_key))
}

c(biome, key) := biome_assignment(fpc, height, gdd)
biome = convert_pacific_centric_2_regular(biome)
dev.off()

plot_raster_from_raster(biome, cols = cols, limits = 1.5:11.5, add_legend = FALSE, quick = TRUE)
legend(x = 'bottomleft', pt.bg = cols, pch = 22, pt.cex = 3, legend = key[, 2], ncol = 1)

comment = key[,1]
names(comment) = key[,2]
writeRaster.gitInfo(biome, paste(fname_out, '.nc', sep = ""),
				    varname = 'biome', comment = comment, overwrite = TRUE)
writeRaster(biome, paste(fname_out, gitVersionNumber(), '.tif', sep = "-"),
			format = "GTiff", options = c("COMPRESS = NONE", "PROFILE = BASELINE"), 
			overwrite = TRUE)