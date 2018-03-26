##################################
## cfg							##
##################################
source("cfg.r")

## LPX output with height, gdd and fpc
fname_in = c(PI = '/Users/dougl/Dropbox/LPX_storage_shed/Equilibrium Test 2 PI - 20170912 (CASP)/test_R20C_CRUnonclim_PI1-4099.nc',
		     Paleo = '/Users/dougl/Dropbox/LPX_storage_shed/Equilibrium Test 2 PI - 20170912 (CASP)/test_R20C_CRUnonclim_PI1-1000.nc')


## varnames in fname_in
varname_fpc    = 'fpc_grid'

layers = list(total = 1:9,
		      tree = 1:7, grass = 8:9, bare = 1:9,
			  BL = c(1:2, 4:5, 7), NL = c(3, 6), 
			  EG = c(1, 3:4, 6), DEC = c(2, 5, 7))
			  
PFT_names = c('Tbe', 'Tbd', 'tne', 'tbe', 'tbd', 'bne', 'bbd', 'c3g', 'c4g')

## Define cols and limits for plotting
cols   = c("white", "#99FF00", "#003300")
dcols  = c("#210021", "#9900FF", "white", "#99FF00", "#003300")
limits = seq(0.1, 0.9, 0.1)
dlimits= c(-0.3, -0.2, -0.1, -0.05, 0.05, 0.1, 0.2, 0.3)
			  
##################################
## open							##
##################################
open_data <- function(fname) {
	dat = stack(fname, varname = varname_fpc)
	dat = convert_pacific_centric_2_regular(dat)
	dat = c(dat, lapply(layers, function(i) sum(dat[[i]])))
	
	test = which(names(dat) == "bare")
	dat[[test]] = 1 - dat[[test]]
	
	return(dat)
}

dats = lapply(fname_in, open_data)

##################################
## plot							##
##################################
plot_SA_Map_standard <- function(dat, vname = '', limits, cols, add_legend = TRUE) {
	dat[dat > 9E9] = NaN
	
	if (is.infinite(min.raster(dat, na.rm = TRUE))) dat[] = 0.0
	plot_raster_from_raster(dat, limits = limits, cols = cols, quick = TRUE, plot_loc = c(0.7, 1.35, -0.4,	-0.35),
						add_legend = add_legend)
	mtext(vname, side = 1, adj = 0.85, line = -5, font = 1.67)
}



plotAllMaps <- function(dat, mn, lims = limits, col = cols) {
	plotGroups <- function(dati,  names, mfrow) {
		dev.new()
		par(mfrow = mfrow, mar = rep(0,4), oma = c(0, 0, 2, 5))
		add_legend = c(TRUE, rep(FALSE, length(dati) - 1))
		mapply(plot_SA_Map_standard, dati, names, add_legend,
			   MoreArgs = list(limits = lims, cols = col))
		title(mn, outer = TRUE)
	}
	
	byPFT = layers2list(dat[[1]])
	plotGroups(byPFT, PFT_names, c(3,3))
	
	dat = dat[-1]
	plotGroups(dat, names(dat), c(3,3))
}

graphics.off()
mapply(plotAllMaps, dats, names(fname_in))

dats = mapply('-', dats[[2]], dats[[1]])
plotAllMaps(dats, 'difference', dlimits, dcols)


