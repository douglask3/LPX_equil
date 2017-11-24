###############################
## cfg                       ##
###############################
source("cfg.r")
## Dir where you model spinup outputs
dir = '/Users/dougl/Dropbox/LPX_storage_shed/PI_SPINUP_20170823/'

## start of filename
fname = 'PISPINUPpt2-'

## variable to test
varname = 'fpc_grid'

## Some controls for optimization (see line with 'nls' in it as well
cntr = nls.control(warnOnly = TRUE, maxiter = 100)

state_levels = c(0.01, 0.1, 0.2, 0.4, 0.6, 0.8)

###############################
## open data                 ##
###############################

## list spinup outputs only
files = list.files(dir)
files = files[grep(fname, files)]
files = files[grep('.nc', files)]

## find file year
year  = sapply(files, function(i) strsplit(i, '-')[[1]][2])
year  = sapply(year, function(i) strsplit(i, '.nc')[[1]][1])
year  = as.numeric(year)
c(year, order)  := sort(year, index.return = TRUE)

## order files correctly and add full path
files = files[order]
files = paste(dir, files, sep = '/')

pdf('figs/equilibriumCheck.pdf', height = 12, width =5)

## setup plotting window
par(mfrow = c(9, 4), mar = rep(0, 4), oma = c(0,0,3,0))

###############################
## plot equilibrium status   ##
###############################
plotMap <- function(dat, limits, cols, topPlot) {
	if (is.infinite(min.raster(dat, na.rm = TRUE))) dat[] = 0.0
	plot_raster_from_raster(dat, limits = limits, cols = cols, quick = TRUE, plot_loc = c(0.7, 1.35, -0.4,	-0.35),
						add_legend = topPlot)
}

###############################
## check less than 2%	     ##
###############################
plot_within_2_percent <- function(endState, almostEndState, diffYear = 50, 
								  mask, topPlot) {
	## absolute difference between last and 2nd to last files, noramalised by end file state
	pc_diff = (50 / diffYear) * 100 * abs(endState - almostEndState) / endState

	## If NaN is produced, than either a ocean cell or last slice has zero, so in equlibrium
	endState[mask] = 0.0
	almostEndState[mask] = 0.0
	pc_diff[mask] = NaN
	
	## Plot value at end of run
	plotMap(endState, state_levels, c('white', 'green'), topPlot)
	if (topPlot) mtext('Current value', side = 3, cex = 0.83)
	
	## Plots any difference greater than 2%. White = okay, green = not in equlibrium.
	plotMap(pc_diff , c(0, 1, 2, 5, 10), c('blue', 'green', 'yellow', 'red'), topPlot)					
	if (topPlot) mtext('difference from\nprevious value (%)', side = 3, cex = 0.83)
}

####################################
## check within 2% of final trend ##
####################################
plot_2_percent_trend <- function(dat, endState, topPlot) {
	equil_fun <- function(mx, dx, x) 
		mx * (1 - exp( -dx * x))

	vdat = values(dat)

	mxCell <- function(y) {
		if (any(y > 9E9)) return(NaN)
		if (any(y > 9E9)) return(NaN)
		if ( max(diff(y)) == 0) return(max(y))
		
		nlsFun <- function(FUN) 
			nls(y ~ FUN(mx, dx, year),
				start = c(mx = max(y), dx = 1/max(year)),
				lower = c(mx = 0  , dx = -9E9), 
				upper = c(mx = 1, dx = 9E9),
				algorithm = "port", control = cntr)
		
		res = try(nlsFun(equil_fun),silent = TRUE)
				  
		if ( class(res) == "try-error") 
			res = try(nlsFun(equil_fun2),silent = TRUE)
		if ( class(res) == "try-error") 
			return(NaN)
		return( coefficients(res)['mx'])
	}
	
	## what is the run tresning towards?
	mxs = dat[[1]]
	values(mxs) = apply(vdat, 1, mxCell)
	
	## the equilibrum point is 2% less then this
	equilPoint = mxs * 0.98
	
	## Plot this equilibrium point
	plotMap(equilPoint, state_levels, c('white', 'green'), topPlot)
	if (topPlot) mtext('Predicted target', side = 3, cex = 0.83)

	## Plot the difference from this state. Blue = we're there; red = more to run
	diff = equilPoint - endState
	plotMap(diff, c(-0.05, -0.02, -0.01, -0.001, 0.001, 0.01, 0.02, 0.05), c('#000099', 'white', '#990000'), topPlot)
	if (topPlot) mtext('Difference from target', side = 3, cex = 0.83)
}

plot_pft <- function(pft, topPlot = FALSE) {
	## open files
	dat   = layer.apply(files, function(i) brick(i, varname = varname)[[pft]])
	dat   = convert_pacific_centric_2_regular(dat)

	## Pick last two time slices
	nouts          = nlayers(dat) ## how many output time slices are there#
	endState       = dat[[nouts]]
	almostEndState = dat[[nouts - 1]]
	diffYear       = year[nouts] - year[nouts - 1]
	mask = endState > 1E36

	plot_within_2_percent(endState, almostEndState, diffYear, mask, topPlot)
	plot_2_percent_trend(dat, endState, topPlot)
		
}

plot_pft(1, TRUE)
lapply(2:9, plot_pft)
dev.off()