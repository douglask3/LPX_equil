##################################
## cfg							##
##################################
source("cfg.r")

## LPX output with height, gdd and fpc
fname_in = c(PI = '/Users/dougl/Dropbox/LPX_storage_shed/Equilibrium Test 2 PI - 20170912 (CASP)/test_R20C_CRUnonclim_PI1-4099.nc',
		     Paleo = '/Users/dougl/Dropbox/LPX_storage_shed/Equilibrium Test 2 PI - 20170912 (CASP)/test_R20C_CRUnonclim_PI1-1000.nc')


## varnames in fname_in
varnames = c(height = 'height'  ,
			 fpc    = 'fpc_grid', 
			 gdd    = 'gdd_grid', 
			 lai    = 'lai_ind' )

## biome colour for plotting - same order as biome key below.
cols   = list(height = c("white", "#333300"),
		      fpc    = c("white", "#99FF00", "#003300"), 
			  gdd    = c("white", "#AABB00", "#220000"),
		      lai    = c("white", "brown", "#002200"))
		   
dcols  = list(height = c("#330033", "white" , "#333300"),
			  fpc    = c("#210021", "#9900FF", "white", "#99FF00", "#003300"),
			  gdd    = c("#000022", "#88FF88", "white", "#DDDD00", "#220000"),
			  lai    = c("#002121", "cyan", "white", "brown", "#002200"))
		 
limits = list(height = c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20), 
			  fpc    = c(0.2, 0.4, 0.6, 0.8, 0.9, 0.95), 
			  gdd    = seq(0, 8000, 1000), 
			  lai    = c(0, 0.2, 0.5, 1, 2, 4, 6, 8, 10))
		
dlimits= list(height = c(-10, -8, -6, -4, -2, 2, 4, 6, 8, 10),
			  fpc    = c(-0.3, -0.2, -0.1, -0.05, 0.05, 0.1, 0.2, 0.3),
			  gdd    = c(-2000, -1000, -500, -200, -100, 100, 200, 500, 1000, 2000),
			  lai    = c(-1, -0.5, -0.2, -0.1, 0.1, 0.2, 0.5, 1))
			  
##################################
## open							##
##################################
open_data <- function(fname) {
	dat = lapply(varnames, function(i) stack(fname, varname = i))

	weightByFPC <- function(x)
		sum(layer.apply(1:9,  function(i) dat[[x]][[i]] * dat[['fpc']][[i]]))

	dat[["lai"   ]] = weightByFPC('lai'   )
	dat[["height"]] = weightByFPC('height')
	dat[['fpc'   ]] = sum(dat[['fpc']])
					   
	dat = lapply(dat, convert_pacific_centric_2_regular)
	return(dat)
}

dats = lapply(fname_in, open_data)

##################################
## plot							##
##################################
plotMap <- function(dat, limits, cols, vname) {
	dat[dat > 9E9] = NaN
	
	if (is.infinite(min.raster(dat, na.rm = TRUE))) dat[] = 0.0
	plot_raster_from_raster(dat, limits = limits, cols = cols, quick = TRUE, plot_loc = c(0.7, 1.35, -0.4,	-0.35),
						add_legend = TRUE)
	mtext(vname, side = 1, adj = 0.85, line = -5, font = 1.67)
}

plotAllMaps <- function(i, mn, lims = limits, col = cols) {
	dev.new()
	par(mfrow = c(2, 2), mar = rep(0,4), oma = c(0, 0, 2, 0))
	mapply(plotMap, i, lims, col, varnames)
	title(mn, outer = TRUE)
}

graphics.off()
mapply(plotAllMaps, dats, names(fname_in))

dats = mapply('-', dats[[2]], dats[[1]])
plotAllMaps(dats, 'difference', dlimits, dcols)
