###############################
## cfg                       ##
###############################
library(raster) ## package for reading in model output

## Dir where you model spinup outputs
dir = '/Users/dougl/Dropbox/LPX_storage_shed/PI_SPINUP_20170823/'

## start of filename
fname = 'PISPINUPpt2-'

## variable to test
varname = 'fpc_grid'

## pft to test
pft = 9

## Some controls for optimization (see line with 'nls' in it as well
cntr = nls.control(warnOnly = TRUE, maxiter = 100)

###############################
## open data                 ##
###############################

## list spinup outputs only
files = list.files(dir)
files = files[grep(fname, files)]
files = files[grep('.nc', files)]

## find file year
year = sapply(files, function(i) strsplit(i, '-')[[1]][2])
year = sapply(year, function(i) strsplit(i, '.nc')[[1]][1])
year = as.numeric(year)

year = sort(year, index.return = TRUE)

order = year[[2]]; year = year[[1]]

## order files correctly and add full path
files = files[order]
files = paste(dir, files, sep = '/')

## open files
dat = stack(files, varname = varname, bands = pft)

nouts = nlayers(dat) ## how many output time slices are there#


## setup plotting window
par(mfrow = c(2, 3))

###############################
## Check equlibrium          ##
###############################
## check less than 2%	     ##
###############################

## absolute difference between last and 2nd to last files, noramalised by end file state
diff = abs(dat[[nouts]] - dat[[nouts - 1]]) / dat[[nouts]]

## If NaN is produced, than either a ocean cell or last slice has zero, so in equlibrium
mask = dat[[nouts]] > 1E36
dat[[nouts]][mask] = NaN
diff[mask] = NaN

## Plots any difference greater than 2%. White = okay, green = not in equlibrium.
plot(dat[[nouts]])
plot(diff)
plot(diff > 0.02)


####################################
## check within 2% of final trend ##
####################################
equil_fun1 <- function(mx, dx, x) 
	mx * (1 - exp( -dx * x))
	
equil_fun2 <- function(mx, dx, x)
	mx * exp( -dx *x)

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
	
	res = try(nlsFun(equil_fun1),silent = TRUE)
			  
	if ( class(res) == "try-error") 
		res = try(nlsFun(equil_fun2),silent = TRUE)
	if ( class(res) == "try-error") 
		return(NaN)
	return( coefficients(res)['mx'])
}

vmxs = apply(vdat, 1, mxCell)
mxs = dat[[1]]
values(mxs) = vmxs

equilPoint = mxs * 0.98

dat = dat[[nlayers(dat)]]
dat[mask] = NaN
plot(dat)

diff = dat - equilPoint
diff[diff < 0.0] = 0.0
plot(diff)

unequal = dat < equilPoint
plot(unequal)
