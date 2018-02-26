##################################
## cfg							##
##################################
source("cfg.r")

## LPX output with height, gdd and fpc
fname_in = '/Users/dougl/Dropbox/LPX_storage_shed/Equilibrium Test 2 PI - 20170912 (CASP)/test_R20C_CRUnonclim_PI1-4099.nc'


## varnames in fname_in
varnames = c(height = 'height', fpc = 'fpc_grid', gdd = 'gdd_grid', lai = 'lai_ind')

## biome colour for plotting - same order as biome key below.
cols = list(height = c("white", "#333300"), fpc = c("white", "#99FF00", "#003300"), gdd = c("white", "#AABB00", "#220000"),
		   lai = c("white", "brown", "#002200"))
		   
dcols = list( gdd = c("#000022", "#88FF88", "white", "#DDDD00", "#220000"))
		 
limits = list(height = c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20), fpc = c(0.2, 0.4, 0.6, 0.8, 0.9, 0.95), gdd = seq(0, 8000, 1000), lai = c(0, 0.2, 0.5, 1, 2, 4, 6, 8, 10))
##################################
## open							##
##################################
dat = lapply(varnames, function(i) stack(fname_in, varname = i))

weightByFPC <- function(x)
	sum(layer.apply(1:9,  function(i) dat[[x]][[i]] * dat[['fpc']][[i]]))
## fpc tree, grass etc
dat[["lai"   ]] = weightByFPC('lai'   )
dat[["height"]] = weightByFPC('height')
dat[['fpc'   ]] = sum(dat[['fpc']])
				   
dat = lapply(dat, convert_pacific_centric_2_regular)

plotMap <- function(dat, limits, cols, vname) {
	dat[dat > 9E9] = NaN
	
	if (is.infinite(min.raster(dat, na.rm = TRUE))) dat[] = 0.0
	plot_raster_from_raster(dat, limits = limits, cols = cols, quick = TRUE, plot_loc = c(0.7, 1.35, -0.4,	-0.35),
						add_legend = TRUE)
	mtext(vname, side = 1, adj = 0.85, line = -5, font = 2)
}

par(mfrow = c(2, 2), mar = rep(0,4))
mapply(plotMap, dat, limits, cols, varnames)
