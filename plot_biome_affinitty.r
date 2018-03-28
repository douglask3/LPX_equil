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

scaleByMaxMin <- function(r, mn, mx) {
	r[r < mn] = mn
	r[r > mx] = mx
	r = r - mn
	r = r / (mx - mn)
	return(r)
}
		
affinityCovertFuns = list(
	height = function(r, mn = 5, 	mx = 25)
		scaleByMaxMin(r, mn, mx)
	,
	gdd = function(r, mn = 250, mx = 6570) 
		scaleByMaxMin(r, mn, mx)
	,
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
						 labelss = seq(0, 1, 0.1), add_legend = add_legend)	
	return(Affinity)
}

plot_Affinitys <- function(Aff) {
	dev.new()
	par(mfrow = c(4, 4), mar = rep(0, 4))
	Affinity = apply(biomeAffinityMatrix, 1, plot_affinity2biome, Aff)
	
	Affinity =  layer.apply(Affinity, function(i) i)
	biome = which.min(Affinity)
	
	cols = c(Thf = '#114400', Tdf = '#441100',
		 wtf = '#005555', tef = '#00EE33', tdf = '#66DD00',
		 bef = '#000088', bdf = '#330033',
		 Ts  = '#AA5500', sw  = '#777922', tp = '#66DD88', 
		 bp  = '#22EEFF', dg  = '#FF9922', hd = '#FEFF44', st = '#BB33FF', t = '#FFBAAA')
	
	plot_SA_Map_standard(biome, cols = cols, limits = 1.5:14.5, add_legend = FALSE)
	
	legend(x = -110, y = -10, pt.bg = cols, pch = 22, pt.cex = 3, legend = paste(names(cols), ' '), ncol = 2, cex = 0.67, bty = 'n')
}

graphics.off()
lapply(Affs, plot_Affinitys)


