plotMask <- function(r,...) {
    r[r>9E9] = NaN
    plot(r, ...)
}

variable_from_biome <- function(id, gdd_threshold = 350, veg_treshold = c(0.6, 0.3),
							 height_threshold = 12) {
    
    ## fpc
    if (id <= 11)
        fpc = c(0.6, 0.8, 1.0)
    else if (id == 12) 
        fpc = c(0.3, 0.45, 0.6)
    else if (id == 13) 
        fpc = c(0.0, 0.15, 0.3)
    else fpc = c(0.0, 0.3, 0.6)

    names(fpc) = paste0("fpc_grid-", c("Min", "Mid", "Max"))
    
    if (id <= 7)      
        height = c(10, 20, 9999)
    else if (id <=11)
        height = c(0,5, 10)
    else height = c(0, NaN, 9999)

    names(height) = paste0("height-", c("Min", "Mid", "Max"))

    if (id == 6 || id == 7 || id == 11 || id == 14 || id == 15) 
        gdd = c(0, 175, 350)
    else 
        gdd = c(350, 1000, 9999)

    names(gdd) = paste0("gdd-", c("Min", "Mid", "Max"))
    
    if (id == 1 || id == 3 || id == 4 || id == 6 || id == 9) 
        eg = c(0.5, 0.75, 1.0)
    else if (id == 2 || id == 5 || id == 7)
        eg = c(0, 0.25, 0.5)
    else
        eg = c(0, NaN, 1)
    
    names(eg) = paste0("evergreen-", c("Min", "Mid", "Max"))

    if (id <= 2 || id == 8) 
        tr = c(0.5, 0.75, 1.0)
    else if (id == 6 || id == 7 || id == 11 || id == 14 || id == 15)
        tr = c(0, 0, 0)
    else if (id == 12 || id == 13)
        tr = c(0, 0.5, 1)
    else 
        tr = c(0, 0.25, 0.5)
    
    names(tr) = paste0("tropical-", c("Min", "Mid", "Max"))

    if (id == 3 || id == 4 || id == 5 || id == 9 || id == 10)
        tm = c(0.5, 0.75, 1.0)
    else if (id == 12 || id == 13)
        tm = c(0, 0.5, 1.0)
    else
        tm = c(0, 0.25, 0.5)
    
    names(tm) = paste0("temperate-", c("Min", "Mid", "Max"))

    return(c(fpc, height, gdd, eg, tr, tm))
}


biome_assign_precalV <- function(fpc, eg, tr, tm, gdd, height) {
    out = fpc
    out[] = NaN
    
    fpc = cut_results(fpc, c(0.3, 0.6))
    gdd = cut_results(gdd, 350)
    height = cut_results(height, 10)
    
    #bl = cut_results(tr + tm, 0.5)
    #bl[gdd ==2] = 2
    tr = cut_results(tr, 0.5)
    eg = cut_results(eg, 0.5)
    tm = cut_results(tm, 0.5)
    #tr[tm == 2] = 1
    warmForest = fpc== 3 & gdd == 2 & height == 2
        tropForest = warmForest & tr == 2

            out[tropForest & eg == 2] = 1    
            out[tropForest & eg == 1] = 2

        tempForest = warmForest & tm == 2

            out[tempForest & eg == 2] = 3
            out[tempForest & eg == 1] = 5
    
    coldForest = fpc == 3 & gdd == 1 & height == 2
         
        tempForest = coldForest & tm == 2
        
            out[tempForest & eg == 2] = 4
            out[tempForest & eg == 1] = 5

        borelForest = coldForest & tm == 1

            out[borelForest & eg == 2] = 6
            out[borelForest & eg == 1] = 7
    
    savanna = fpc==3 & gdd == 2 & height == 1
        out[savanna] = 8
        out[savanna & tm == 2 &eg == 2] = 9            
        out[savanna & tm == 2 & eg == 1] = 10
        #out[savanna & tr == 2 & bl == 2] = 8
        out[savanna & tr == 2] = 8
        #browser()
        out[savanna & tr == 1 & tm == 1] = 11

    out[fpc == 3 & gdd == 1] = 14

    out[fpc == 2 & gdd == 2] = 12
    out[fpc == 2 & gdd == 1] = 15
    
    out[fpc == 1 & gdd == 2] = 13
    out[fpc == 1 & gdd == 1] = 15
    
    return(out)

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
