##################################
## cfg							##
##################################
source("cfg.r")
graphics.off()

## LPX output with height, gdd and fpc
fname_in = c(PI = 'data/PIRUN1FIREON_5380_138_CRUnonclim_PI1-5379.nc',
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
	gdd = function(r, mn = 250, mx = 6570) 
		scaleByMaxMin(r, mn, mx),
		
	height = function(r, mn = 5, 	mx = 25)
		scaleByMaxMin(r, mn, mx)
	,
	cover = function(lai, fpc = NULL, mx = 1)  {
		if (!is.null(fpc)) lai = sum(fpc)#sum(lai * fpc)
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
	
	dat[[ 'Tree.cover']] = affinityCovertFuns$cover   (dat[['lai'   ]][[Trees]], dat[['fpc']][[Trees]])
	dat[['Grass.cover']] = affinityCovertFuns$cover   (dat[['lai'   ]][[Grass]], dat[['fpc']][[Grass]])
	
	dat[['Grass.cover']] = (1 - dat[['Tree.cover']]) * dat[['Grass.cover']]
	dat[[ 'Bare.cover']] = 1 - dat[['Tree.cover']] - dat[['Grass.cover']]
	return(dat)
}

dats = lapply(fname_in, open_data)

biomeAffinityMatrix = biomeAffinityMatrix0 = 
					  read.csv('docs/Marchant_affinity.csv', stringsAsFactors= FALSE)
biomeAffinityMatrix['gdd'] = affinityCovertFuns[['gdd']](biomeAffinityMatrix['gdd'])
biomeAffinityMatrix['height'] = affinityCovertFuns[['height']](biomeAffinityMatrix['height'])
biomeAffinityMatrix['seasonal'] = affinityCovertFuns[['seasonal']](biomeAffinityMatrix['seasonal'])

biomeAffinityMatrix = data.frame(
	biomeAffinityMatrix['Biome'],
	"Hot" = biomeAffinityMatrix[['gdd']],
	Cold = 1 - biomeAffinityMatrix[['gdd']],
	biomeAffinityMatrix[c('Tree.Cover', 'Grass.Cover', 'Bare.Cover')],
	Tall = biomeAffinityMatrix[['height']],
	Short = 1 - biomeAffinityMatrix[['height']],
	Seasonal = biomeAffinityMatrix[['seasonal']],
	Evergreen = 1 - biomeAffinityMatrix[['seasonal']])

biomeCols = c(TRFO = "#003311", TSFO = "#009900", TDFO = "#775500",
		 WTRF = "#003333", WEFO = "#00AAAA", CTRF = "#0033DD", 
		 WAMF = "#330033", COMI = "#33FF33", CGSS = "#993300",
		 STEP = "#FFFF00", DESE = "#FFAAAA", CGSH = "#772255")

######################
## Plot Explanation ##
######################




plotAffinnityEqs <- function(biomePoints = FALSE, biomeLines = FALSE) {
	png(paste('figs/AffinityEquations', biomePoints, biomeLines, '.png', sep = ''),
		height = 7, width = 7, units = 'in', res = 300)
	par(mfcol = c(3, 2), mar = c(3, 2, 2, 2), oma = c(3, 1.4, 0, 1.4))
	plotAffinnityFuns <- function(x, FUN, labs, rowname, title) {
		y = affinityCovertFuns[[FUN]](x)
		plot(x, y, xlab = '', ylab = '', type = 'l', yaxt = 'n', lwd = 2)
		axis(2, at = c(0, 1), label = labs)
		axis(4)
		mtext(title, cex = 1.2)
		mtext(FUN, line = 2, side = 1)

		if (biomeLines) {
			lines(x, abs(biomeAffinityMatrix[1, rowname] - y), col = biomeCols[1], lty = 2, lwd = 2)
			lines(x, abs(biomeAffinityMatrix[3, rowname] - y), col = biomeCols[3], lty = 3, lwd = 2)
			pnts = c(1, 3)
		} else pnts = 1:nrow(biomeAffinityMatrix)
		if (biomePoints) {
			x = try(biomeAffinityMatrix0[pnts, FUN], silent = TRUE)
			if (class(x) == "try-error") x = biomeAffinityMatrix0[pnts, rowname]
			
			y = rep(0, length(x))
			for (i in unique(x)) {
				index = which(x == i)
				y[index] = seq(0, 1, length.out = 30)[1:length(index)]
			}
			points(x, y, col = biomeCols[pnts], pch = 19)
		}
	}

	plotAffinnityFuns(1:7500, "gdd", c('Cold', 'Hot'), "Hot", 'Hot/Cold')
	plotAffinnityFuns(seq(0, 30, 0.1), "height", c('Short', 'Tall'), "Tall", 'Short/Tall')
	plotAffinnityFuns(seq(6, 12, 0.1), "seasonal", c('Evergreen', 'Deciduous'), "Seasonal", 'Evergreen/Deciduous')
	axis(side = 1, at = seq(6, 12, length.out = 6), labels = seq(0, 1, 0.2), line = 3)
	mtext('Deciduous Cover', line = 5, side = 1)

	plotAffinnityFuns(seq(0, 1, 0.01), "cover", c('Sparse', 'Dense'),  "Tree.Cover",  'Tree cover')
	plotAffinnityFuns(seq(0, 1, 0.01), "cover", c('Sparse', 'Dense'), "Grass.Cover", 'Grass cover')
	plotAffinnityFuns(seq(0, 1, 0.01), "cover", c('Barren', 'Covered'),  "Bare.Cover",  'Bare cover')

	if (biomeLines)
		legend(x = 'topleft', c('Affinity', 'Tropical Rainforest', 'Savanna'),
			   col = c('black', biomeCols[c(1,3)]), lty = c(1, 2, 3))
			   
	if (biomePoints)
		legend(x = 'bottomright', biomeAffinityMatrix[,1], ncol = 2, col = biomeCols, pch = 19)
	
	mtext('Difference from biome', side = 4, outer = TRUE)
	mtext('Affinity', side = 2, outer = TRUE)
	dev.off.gitWatermark()
}

plotAffinnityEqs()
plotAffinnityEqs(TRUE)
plotAffinnityEqs(TRUE, TRUE	)

browser()
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

plot_affinity2biome <- function(biome, Aff,
							    AffCols = c("#002200", "#999900", "white"),
								AffLims = seq(0.1, 0.9, 0.1),
								AffLabs = seq(0, 1, 0.1), ...) {
	
	nm       = biome[1]
	biome    = as.numeric(biome[-1])
	
	totAff <- function(A0) {
		A = sum(abs(A0 - biome), na.rm = TRUE) / (2 * sum(biome, na.rm = TRUE))
		A[is.na(A0[[1]])] = NaN
		return(A)
	}
	
	if (is.list(Aff)) {
		Affinity = lapply(Aff, totAff)
		Affinity = Affinity[[2]] - Affinity[[1]]
	} else Affinity = totAff(Aff)
	
	if (nm == 'TRFO') add_legend = TRUE
		else add_legend = FALSE
	
	plot_SA_Map_standard(Affinity, nm, 
						 limits = AffLims, cols = AffCols, 
						 labelss = AffLabs, add_legend = add_legend)	
	return(Affinity)
}

plot_factors <- function(Aff, FactCols = c("white", "black"),
						      FactLims = seq(0.1, 0.9, 0.1),
							  FactLabs = seq(0, 1, 0.1), ...) {
	dev.new()
	par(mfrow = c(3, 3), mar = rep(0,4))
	
	if (is.list(Aff)) Aff = Aff[[2]] - Aff[[1]]
	nms = names(Aff)
	Aff = layers2list(Aff)
	
	mapply(plot_SA_Map_standard, Aff, nms,
		   MoreArgs = list(cols = FactCols,  
						   limits = FactLims,
						   labelss = FactLabs))
	
}

plot_Affinitys <- function(Aff, plotBiome = TRUE, ...) {
	dev.new()
	par(mfrow = c(4, 4), mar = rep(0, 4))
	
	Affinity = apply(biomeAffinityMatrix, 1, plot_affinity2biome, Aff, ...)
	
	Affinity =  layer.apply(Affinity, function(i) i)
	
	if (!plotBiome) return()
	
	biome = which.min(Affinity)
	
	
	plot_SA_Map_standard(biome, cols = biomeCols, limits = 0.5 + (1:(length(biomeCols)-1)), add_legend = FALSE)
	
	legend(x = -115, y = -10, pt.bg = biomeCols, pch = 22, pt.cex = 3, legend = paste(names(biomeCols), ' '), ncol = 2, cex = 0.67, bty = 'n')
}


plotAlll <- function(...) {
	plot_factors(...)
	plot_Affinitys(...)
}
lapply(Affs, plotAlll)

plotAlll(list(Affs[[1]], Affs[[2]]), plotBiome = FALSE,
		 FactCols = c("#000022", "#0000FF", "white", "#FF0000", "#220000"),
		 FactLims = c(-0.3, -0.2, -0.1, -0.01, 0.01, 0.1, 0.2, 0.3), 
		 FactLabs = NULL,
		 AffCols = c("#002200", "#999900", "white", "#990099", "#000022"),
		 AffLims = c(-0.3, -0.2, -0.1, -0.01, 0.01, 0.1, 0.2, 0.3),
		 AffLabs = NULL)
