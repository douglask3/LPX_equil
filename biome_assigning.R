##################################
## cfg							##
##################################
source("cfg.r")

## LPX output with height, gdd and fpc
fname_in = '/Users/dougl/Dropbox/LPX_storage_shed/Equilibrium Test 2 PI - 20170912 (CASP)/test_R20C_CRUnonclim_PI1-4099.nc'

## constructed biome output file (for nc and geotiff)
fname_out = 'outputs/biomeOut'

## varnames in fname_in
varnames = c(height = 'height', fpc = 'fpc_grid', gdd = 'gdd_grid')

## biome colour for plotting - same order as biome key below.
cols = c(Thf = '#114400', Tdf = '#441100',
		 wtf = '#005555', tef = '#00EE33', tdf = '#66DD00',
		 bef = '#000088', bdf = '#330033',
		 Ts  = '#AA5500', sw  = '#777922', tp = '#66DD88', 
		 bp  = '#22EEFF', dg  = '#FF9922', hd = '#FEFF44', st = '#BB33FF', t = '#FFBAAA')

##################################
## open							##
##################################
height = stack(fname_in, varname = varnames['height'])
fpc    = stack(fname_in, varname = varnames['fpc'   ])
gdd    = stack(fname_in, varname = varnames['gdd'   ])

##################################
## biome function				##
##################################
biome_assignment <- function(fpc, height, gdd = NULL,
							 gdd_threshold = 350, veg_treshold = c(0.6, 0.3),
							 height_threshold = 10) {

	# matrix describing which number in outputted raster corrisponds to whih biome
	biome_key = cbind(c(1:15), 
	                  c('Tropical Humid Forest', 'Tropical Dry Forest', 'Warm Temperate Forest', 'Temperate Evergreen Forest', 'Temperate Deciduous Forest',
							'Boreal  Evergreen Forest', 'Boreal Deciduous Forest',
						'Tropical Savannah', 'Sclerophyll Woodland', 'Temperate Parkland',
							'Boreal Parkland',
						'Dry Grass or Shrub', 'Hot Desert', 'Shrub Tundra', 'Tundra'))
	
	## calculate cell average height and fraction of cells covered by vegetation
	height = sum(height * fpc)
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
	biome[        wood       & tropical &  tall &  evergreen] = 1
	biome[        wood       & tropical &  tall & !evergreen] = 2
	biome[ warm & wood       & tropical & !tall             ] = 8
	biome[        wood       & warmTemp &  tall             ] = 3
	biome[ warm & wood       & warmTemp & !tall             ] = 9
	biome[        wood       & coldTemp &  tall &  evergreen] = 4
	biome[        wood       & coldTemp &  tall & !evergreen] = 5
	biome[ warm & wood       & coldTemp & !tall             ] = 10
	biome[        wood       & boreal   &  tall &  evergreen] = 6
	biome[        wood       & boreal   &  tall & !evergreen] = 7
	biome[        wood       & boreal   & !tall             ] = 11
	biome[ warm & grass                                     ] = 12
	biome[ warm & arid                                      ] = 13
	biome[!warm                         & !tall             ] = 14
	biome[!warm & !wood                                     ] = 15
	
	## remove ocean cells
	biome[height > 9E9| height == -999] = NaN
	
	return(list(biome, biome_key))
}

c(biome, key) := biome_assignment(fpc, height, gdd)
biome = convert_pacific_centric_2_regular(biome)

##################################
## output        				##
##################################
## figure
plot_raster_from_raster(biome, cols = cols, limits = 1.5:14.5, add_legend = FALSE, quick = TRUE)
legend(x = 'bottomleft', pt.bg = cols, pch = 22, pt.cex = 3, legend = key[, 2], ncol = 1)

## netcdf file
comment = key[,1]
names(comment) = key[,2]
writeRaster.gitInfo(biome, paste(fname_out, '.nc', sep = ""),
				    varname = 'biome', comment = comment, overwrite = TRUE)

## geotiff file
writeRaster(biome, paste(fname_out, gitVersionNumber(), '.tiff', sep = "-"), overwrite = TRUE,
			format = "GTiff", datatype='INT1U', options = c("COMPRESS = NONE", "PROFILE = BASELINE"))
			


