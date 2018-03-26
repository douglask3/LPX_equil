##################################
## cfg							##
##################################
source("cfg.r")

## LPX output with height, gdd and fpc
fname_in = c(PI = '/Users/dougl/Dropbox/LPX_storage_shed/Equilibrium Test 2 PI - 20170912 (CASP)/test_R20C_CRUnonclim_PI1-4099.nc',
		     Paleo = '/Users/dougl/Dropbox/LPX_storage_shed/Equilibrium Test 2 PI - 20170912 (CASP)/test_R20C_CRUnonclim_PI1-1000.nc')


## varnames in fname_in
varname    = 'fpc_grid'

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
	## open raw data and convert to regular grid
	dat = stack(fname, varname = varname)
	dat = convert_pacific_centric_2_regular(dat)
	
	## calculate are of each veg catigory by summing layers defined above
	veg_cat = lapply(layers, function(i) sum(dat[[i]]))
	
	## join original data to start
	dat     = c(dat, veg_cat)
	
	## bare soil = 1 - corresponding cover.
	test = which(names(dat) == "bare")
	dat[[test]] = 1 - dat[[test]]
	
	return(dat)
}

## open each file in turn
dats = lapply(fname_in, open_data)

##################################
## plot							##
##################################
plotAllMaps <- function(dat, mn, lims = limits, col = cols) {
	plotGroups <- function(dati,  names, mfrow) {
		##set up plotting window
		dev.new()
		par(mfrow = mfrow, mar = rep(0,4), oma = c(0, 0, 2, 5))
		
		## adding legend to last plot only, so setting up boolean vector to cope with this
		add_legend = c(TRUE, rep(FALSE, length(dati) - 1))
		
		## plot each variable in turn
		mapply(plot_SA_Map_standard, dati, names, add_legend,
			   MoreArgs = list(limits = lims, cols = col))
		title(mn, outer = TRUE)
	}
	
	## plot each pfts FPC
	byPFT = layers2list(dat[[1]])
	plotGroups(byPFT, PFT_names, c(3,3))
	
	## plot each veg category
	dat = dat[-1]
	plotGroups(dat, names(dat), c(3,3))
}

## plot each file
graphics.off()
mapply(plotAllMaps, dats, names(fname_in))

## plot difference between files
dats = mapply('-', dats[[2]], dats[[1]])
plotAllMaps(dats, 'difference', dlimits, dcols)


