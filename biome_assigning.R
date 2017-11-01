fname = '/Users/dougl/Dropbox/LPX_storage_shed/Equilibrium Test 2 PI - 20170912 (CASP)/test_R20C_CRUnonclim_PI1-4099.nc'

varnames = c(height = 'height', fpc = 'fpc_grid', gdd = 'gdd_grid')

height = stack(fname, varname = varnames['height'])
fpc    = stack(fname, varname = varnames['fpc'   ])
gdd    = stack(fname, varname = varnames['gdd'   ])

biome_assignment <- function(fpc, height, gdd = NULL,
							 gdd_threshold = 350, veg_treshold = c(0.6, 0.3),
							 height_threshold = 10) {

	biome_key = cbind(c(1:12), 
	                  c('Tropical Forest', 'Warm Temperate Forest', 'Temperate Forest',
							'Boreal Forest',
						'Tropical Savannah', 'Sclerophyll Woodland', 'Temperate Parkland',
							'Boreal Parkland',
						'Dry Grass/Shrub', 'Hot Desert', 'Shrub Tundra', 'Tundra'))
		
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