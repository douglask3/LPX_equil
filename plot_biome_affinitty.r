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
			 
Tree      = c(1, 1, 1, 1, 1, 1, 1, 0, 0)
Evergreen = c(1, 0, 1, 1, 0, 1, 0, NaN, NaN)

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
	lai = function(lai, fpc = NULL, mx = 6)  {
		if (!is.null(fpc)) lai = sum(lai * fpc)
		r = scaleByMax(lai, mx)
		return(r)
	}
	,
	seasonal = function(mnths) {
		mnths = 12 - mnths
		mnths = mnths / 6
		return(mnths)
	}
	)

	Trees =  which(Tree == 1)
	Grass =  which(Tree != 1)
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
	
	dat[[ 'Tree.cover']] = affinityCovertFuns$lai   (dat[['lai'   ]][[Trees]], dat[['fpc']][[Trees]])
	dat[['Grass.cover']] = affinityCovertFuns$lai   (dat[['lai'   ]][[Grass]], dat[['fpc']][[Grass]])
	
	dat[['Grass.cover']] = (1 - dat[['Tree.cover']]) * dat[['Grass.cover']]
	dat[[ 'Bare.cover']] = 1 - dat[['Tree.cover']] - dat[['Grass.cover']]
	return(dat)
}

dats = lapply(fname_in, open_data)

biomeAffinityMatrix = read.csv('docs/Marchant_affinity.csv', stringsAsFactors= FALSE)
biomeAffinityMatrix['GDD'] = affinityCovertFuns[['gdd']](biomeAffinityMatrix['GDD'])
biomeAffinityMatrix['Height'] = affinityCovertFuns[['height']](biomeAffinityMatrix['Height'])
biomeAffinityMatrix['Green'] = affinityCovertFuns[['seasonal']](biomeAffinityMatrix['Green'])

biomeAffinityMatrix = data.frame(
	biomeAffinityMatrix['Biome'],
	"Hot" = biomeAffinityMatrix[['GDD']],
	Cold = 1 - biomeAffinityMatrix[['GDD']],
	biomeAffinityMatrix[c('Tree.Cover', 'Grass.Cover', 'Bare.Cover')],
	Tall = biomeAffinityMatrix[['Height']],
	Short = 1 - biomeAffinityMatrix[['Height']],
	Seasonal = biomeAffinityMatrix[['Green']],
	Evergreen = 1 - biomeAffinityMatrix[['Green']])


##################################
## plot							##
##################################

makeAffinity <- function(dat) {
	Hot = dat[['gdd']]
	Cold = 1 - Hot
	
	Tree.cover = dat[['Tree.cover']]
	Grass.cover = dat[['Grass.cover']]
	Bare.cover = dat[['Bare.cover']]
	
	Tall = sum((dat[['height']] * dat[['fpc']])[[Trees]]) / sum(dat[['fpc']][[Trees]])
	Short = 1 - Tall

	

	EG  = sum(Evergreen * dat[['fpc']] * dat[['lai']], na.rm = TRUE) / #
		sum(dat[['fpc']] * dat[['lai']], na.rm = TRUE) ## weight by LAI
	DEC = 1 - EG
	
	Aff = addLayer(Hot, Cold, Tree.cover, Grass.cover, Bare.cover, Tall, Short, DEC, EG)
	names(Aff) = c( "Hot", "Cold", "Tree.cover", "Grass.cover", "Bare.cover", "Tall", "Short",
				   "Seasonal", "Evergreen")
	return(Aff)
}

Affs = lapply(dats, makeAffinity)

plot_affinity2biome <- function(biome, Aff) {
	
	nm       = biome[1]
	biome    = as.numeric(biome[-1])
	Affinity = sum(abs(Aff - biome), na.rm = TRUE) / (2 * sum(biome, na.rm = TRUE))
	Affinity[is.na(Aff[[1]])] = NaN
	
	if (nm == 'Thf') add_legend = TRUE
		else add_legend = FALSE
	
	plot_SA_Map_standard(Affinity, nm, 
						 seq(0.1, 0.9, 0.1), c("#002200", "#999900", "white"), 
						 labelss = seq(0, 1, 0.1), add_legend = add_legend)	
	return(Affinity)
}

plot_factors <- function(Aff) {
	dev.new()
	par(mfrow = c(3, 3), mar = rep(0,4))
	
	nms = names(Aff)
	Aff = layers2list(Aff)
	
	mapply(plot_SA_Map_standard, Aff, nms,
		   MoreArgs = list(cols = c("white", "black"), 
						   limits = seq(0.1, 0.9, 0.1),labelss = seq(0, 1, 0.1)))
	
}

plot_Affinitys <- function(Aff) {
	dev.new()
	par(mfrow = c(4, 4), mar = rep(0, 4))
	Affinity = apply(biomeAffinityMatrix, 1, plot_affinity2biome, Aff)
	
	Affinity =  layer.apply(Affinity, function(i) i)
	biome = which.min(Affinity)
	
	cols = c(TRFO = "#003311", TSFO = "#009900", TDFO = "#775500",
		     WTRF = "#003333", WEFO = "#00AAAA", CTRF = "#0033DD", 
			 WAMF = "#330033", COMI = "#33FF33", CGSS = "#993300",
			 STEP = "#FFFF00", DESE = "#FFAAAA", CGSH = "#772255")
	
	
	plot_SA_Map_standard(biome, cols = cols, limits = 0.5 + (1:(length(cols)-1)), add_legend = FALSE)
	
	legend(x = -115, y = -10, pt.bg = cols, pch = 22, pt.cex = 3, legend = paste(names(cols), ' '), ncol = 2, cex = 0.67, bty = 'n')
}

graphics.off()
plotAlll <- function(...) {
	plot_factors(...)
	plot_Affinitys(...)
}
lapply(Affs, plotAlll)


