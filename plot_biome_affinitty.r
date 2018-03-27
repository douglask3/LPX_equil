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

scaleByMax <- function(r, mx) {
	r[r > mx] = mx
	r = r / mx
	return(r)
}	
		
affinityCovertFuns = list(
	height = function(r, mx = 20)
		scaleByMax(r, mx)
	,
	gdd = function(r, mn = 350, mx = 7000) {
		r[r < mn] = mn
		
		r[r > mx] = mx
		r = r - mn
		r = r / (mx - mn)
		return(r)
	},
	lai = function(r, mx = 6) 
		scaleByMax(r, mx)
	
	)

##################################
## open							##
##################################
open_data <- function(fname) {
	open_mask <- function(i) {
		dat =  stack(fname, varname = i)
		dat[dat > 9E9] = NaN
		dat = convert_pacific_centric_2_regular(dat)
		return(dat)
	}
	dat = lapply(varnames, open_mask)
	
	dat[['height']] = affinityCovertFuns$height(dat[['height']])
	dat[['gdd'   ]] = affinityCovertFuns$gdd   (dat[['gdd'   ]])
	dat[['lai'   ]] = sum(dat[['lai']] * dat[['fpc']])
	dat[['lai'   ]] = affinityCovertFuns$lai   (dat[['lai'   ]])
	return(dat)
}

dats = lapply(fname_in, open_data)


##################################
## plot							##
##################################

Tree = c(1, 1, 1, 1, 1, 1, 1, 0, 0)
Evergreen = c(1, 0, 0, 0, 1, 0, 1, 0.5, 0.5)		  
		  

		
		
makeAffinity <- function(dat) {
	vegFrac = sum(dat[['fpc']])
	
	Dense = dat[['lai']]
	Sparse = 1 - Dense
	
	Tall = sum((dat[['height']] * dat[['fpc']])) / vegFrac
	Short = 1 - Tall

	Hot = dat[['gdd']]
	Cold = 1 - Hot

	EG = sum(Evergreen * dat[['fpc']]) / vegFrac
	DEC = 1 - EG
	
	Aff = addLayer(Dense, Sparse, Tall, Short, Hot, Cold, DEC, EG)
	names(Aff) = c("Dense", "Sparse", "Tall", "Short",
				   "Hot", "Cold", "Seasonal", "Evergreen")
	return(Aff)
}

Affs = lapply(dats, makeAffinity)

plot_affinity2biome <- function(biome, Aff) {
	nm       = biome[1]
	biome    = as.numeric(biome[-1])
	Affinity = sum(abs(Aff - biome)) / length(biome)
	
	if (nm == 'Thf') add_legend = TRUE
		else add_legend = FALSE
	
	plot_SA_Map_standard(Affinity, nm, 
						 seq(0.1, 0.9, 0.1), c("#002200", "#999900", "white"), 
						 add_legend = add_legend)	
	return(Affinity)
}
graphics.off()
par(mfrow = c(4, 4), mar = rep(0, 4))
Affinity = apply(biomeAffinityMatrix, 1, plot_affinity2biome, Affs[[1]])


