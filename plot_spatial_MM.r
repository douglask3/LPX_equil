##################################
## cfg							##
##################################
source("cfg.r")

## LPX output files containing fpc_grid
fname_in = c(PI = '/Users/dougl/Dropbox/LPX_storage_shed/Equilibrium Test 2 PI - 20170912 (CASP)/test_R20C_CRUnonclim_PI1-4099.nc',
		     Paleo = '/Users/dougl/Dropbox/LPX_storage_shed/Equilibrium Test 2 PI - 20170912 (CASP)/test_R20C_CRUnonclim_PI1-1000.nc')

varname    = 'fpc_grid'

## Define cols and limits for plotting
cols   = c("white", "#77FF77", "#00DD00", "#110000")
limits = c(0.001, 0.002, 0.005, 0.01, 0.02, 0.05, 0.1, 0.2)
			  
##################################
## open							##
##################################
open_data <- function(fname) {	
	dat = stack(fname, varname = varname)
	dat = convert_pacific_centric_2_regular(dat)
	dat[dat > 9E9] = NaN	
	return(dat)
}

## open each file
dats = lapply(fname_in, open_data)

##################################
## plot							##
##################################
plotGrad <- function(dat, mn) {
	## works out change in MM across neighboring cells
	grad = mmGrad.brick(dat)
	
	## plots result
	plot_SA_Map_standard(grad, mn, limits, cols)
	return(grad)
}

## Setup plotting window
par(mfrow = c(1, 2), mar = rep(0, 4))

##plot for each experiment
grads = mapply(plotGrad, dats, names(dats))

dev.new()
RainForest = c(Tbe = 0.95, Tbd = 0, tne = 0, tbe = 0, tbd = 0,  bne = 0, bbd = 0, c3g = 0.0, c4g = 0.05)

plot_diff_from_rainforest <- function(dat, mn) {
	diff = mapply(function(i, j) abs(i - j), RainForest, layers2list(dat))
	diff = sum(layer.apply(diff, function(i) i)) 
	plot_SA_Map_standard(diff, '', c(0.01, 0.1, 0.2, 0.5, 1.0, 1.5, 1.8, 1.9, 1.99), c("#001100", "#777700", "#FF99FF"))
	return(diff)
}

