##################################
## cfg							##
##################################
source("cfg.r")

## LPX output with height, gdd and fpc
fname_in = 'data/Figures_doug/Figure 2_6/4ave_foff.nc'

## constructed biome output file (for nc and geotiff)
fname_out = 'outputs/biomeOut-test'

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
			


