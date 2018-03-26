##################################
## cfg							##
##################################
source("cfg.r")

## LPX output files containing fpc_grid
fname_in = c(PI = '/Users/dougl/Dropbox/LPX_storage_shed/Equilibrium Test 2 PI - 20170912 (CASP)/test_R20C_CRUnonclim_PI1-4099.nc',
		     Paleo = '/Users/dougl/Dropbox/LPX_storage_shed/Equilibrium Test 2 PI - 20170912 (CASP)/test_R20C_CRUnonclim_PI1-1000.nc')

varname    = 'fpc_grid'

## Define cols and limits for plotting
cols   = c("white", "green", "#220000")
limits = c(0.005, 0.01, 0.02, 0.04, 0.06, 0.08, 0.1)
			  
##################################
## open							##
##################################
open_data <- function(fname) {
	dat = stack(fname, varname = varname)
	dat = convert_pacific_centric_2_regular(dat)
	dat[dat > 9E9] = NaN	
	return(dat)
}
dats = lapply(fname_in, open_data)

plotGrad <- function(dat, mn) {
	grad = mmGrad.brick(dat)
	plot_SA_Map_standard(grad, mn, limits, cols)
	return(grad)
}

par(mfrow = c(1, 2), mar = rep(0, 4))
grads = mapply(plotGrad, dats, names(dats))
