plotMask <- function(r,...) {
    r[r>9E9] = NaN
    plot(r, ...)
}

biome_assignment <- function(fpc, height, gdd = NULL,
							 gdd_threshold = 350, veg_treshold = c(0.6, 0.3),
							 height_threshold = 12) {

	# matrix describing which number in outputted raster corrisponds to whih biome
	biome_key = cbind(c(1:15), 
	                  c('Tropical Humid Forest', 'Tropical Dry Forest', 'Warm Temperate Forest', 'Temperate Evergreen Forest', 'Temperate Deciduous Forest',
							'Boreal  Evergreen Forest', 'Boreal Deciduous Forest',
						'Tropical Savannah', 'Sclerophyll Woodland', 'Temperate Parkland',
							'Boreal Parkland',
						'Dry Grass or Shrub', 'Hot Desert', 'Shrub Tundra', 'Tundra'))
	
	## calculate cell average height and fraction of cells covered by vegetation
	height = sum(height * fpc)#[[1:7]])
	vegCover = sum(fpc)
	
	## bioclimatic thresholds
	warm     = gdd > gdd_threshold
	
    wood     = vegCover > veg_treshold[1]
	arid     = vegCover < veg_treshold[2]
	grass    = !wood & ! arid
	
	evergreen = sum(fpc[[c(2, 5, 6)]]) < sum(fpc[[1, 3, 4, 7]])
	tropical = sum(fpc[[1:2]]) > 0
	warmTemp = fpc[[4]] > vegCover/2 & !tropical
	coldTemp = sum(fpc[[c(3,5)]]) > 0 & !tropical & !warmTemp
	boreal = !tropical & !warmTemp & !coldTemp
	
	tall     =  height > height_threshold
	
	## setup output raster based on shape of input
	biome = height
	biome[] = 0.0
	
	## Assign biomes
	
		  #temp	  #life form  #phenology  #height
	biome[        wood       & tropical &  tall &  evergreen] = 1  #Thf
	biome[        wood       & tropical &  tall & !evergreen] = 2  #Tdf
	biome[ warm & wood       & tropical & !tall             ] = 8  #Ts
	biome[        wood       & warmTemp &  tall             ] = 3  #wtf
	biome[ warm & wood       & warmTemp & !tall             ] = 9  #sw
	biome[        wood       & coldTemp &  tall &  evergreen] = 4  #tef
	biome[        wood       & coldTemp &  tall & !evergreen] = 5  #tdf
	biome[ warm & wood       & coldTemp & !tall             ] = 10 #tp
	biome[        wood       & boreal   &  tall &  evergreen] = 6  #bef
	biome[        wood       & boreal   &  tall & !evergreen] = 7  #bdf
	biome[        wood       & boreal   & !tall             ] = 11 #bp
	biome[ warm & grass                                     ] = 12 #dg
	biome[ warm & arid                                      ] = 13 #hd
	biome[!warm                         & !tall             ] = 14 #st
	biome[!warm & !wood                                     ] = 15 #t
	
	## remove ocean cells
	biome[height > 9E9| height == -999] = NaN
	
	return(list(biome, biome_key))
}

biome_assignment_from_file <- function(filename_lpx, filename_tas) {
    
    fpc = brick(filename_lpx, varname = 'fpc_grid')
    height   = raster(filename_lpx, varname = 'height')
    gdd = raster(filename_tas) > 2
    gdd = gdd * 500
    
    c(biome, nn) := biome_assignment(fpc, height, gdd)
    return(biome)
}