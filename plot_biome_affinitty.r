##################################
## cfg							##
##################################
rm(biomeAffinityMatrix)
source("cfg.r")
graphics.off()

## LPX output with height, gdd and fpc
fname_in = c(PI = 'data/Figures_doug/Figure 2_6/4ave_pico2_foff.nc',
		     Paleo = 'data/Figures_doug/Figure 2_6/4ave_fon.nc')


## varnames in fname_in
varnames = c(height = 'height'  ,
			 fpc    = 'fpc_grid', 
			 gdd    = 'gdd_grid', 
			 lai    = 'lai_ind' )
			 
Tree      = c(1, 1, 1, 1, 1, 1, 1, 0, 0)
Evergreen = c(1, 0, 1, 1, 0, 1, 0, NaN, NaN)
biomeAffinityMatrix[8, c('Seasonal', 'Evergreen')] = c(0.5, 0.5)

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
	gdd = function(r, mn = 350, mx = 700) 
		scaleByMaxMin(r, mn, mx),
		
	height = function(r, mn = 0, 	mx = 20)
		scaleByMaxMin(r, mn, mx)
	,
	cover = function(lai, fpc = NULL, mx = 1.0)  {
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
	
    dat[['Dense' ]] = affinityCovertFuns$cover   (dat[['lai'   ]], dat[['fpc']][[Trees]])
    dat[['Sparse']] = 1 - dat[['Dense' ]]
	#dat[[ 'Tree.cover']] = affinityCovertFuns$cover   (dat[['lai'   ]][[Trees]], dat[['fpc']][[Trees]])
	#dat[['Grass.cover']] = affinityCovertFuns$cover   (dat[['lai'   ]][[Grass]], dat[['fpc']][[Grass]])
	
	#dat[['Grass.cover']] = (1 - dat[['Tree.cover']]) * dat[['Grass.cover']]
	#dat[[ 'Bare.cover']] = 1 - dat[['Tree.cover']] - dat[['Grass.cover']]
	return(dat)
}

dats = lapply(fname_in, open_data)

#biomeAffinityMatrix = biomeAffinityMatrix0 = 
#					  read.csv('docs/Marchant_affinity.csv', stringsAsFactors= FALSE)
#biomeAffinityMatrix['gdd'] = affinityCovertFuns[['gdd']](biomeAffinityMatrix['gdd'])
#biomeAffinityMatrix['height'] = affinityCovertFuns[['height']](biomeAffinityMatrix['height'])
#biomeAffinityMatrix['seasonal'] = affinityCovertFuns[['seasonal']](biomeAffinityMatrix['seasonal'])

#biomeAffinityMatrix = data.frame(
#	biomeAffinityMatrix['Biome'],
#	"Hot" = biomeAffinityMatrix[['gdd']],
#	Cold = 1 - biomeAffinityMatrix[['gdd']],
#	biomeAffinityMatrix[c('Tree.Cover', 'Grass.Cover', 'Bare.Cover')],
#	Tall = biomeAffinityMatrix[['height']],
#	Short = 1 - biomeAffinityMatrix[['height']],
#	Seasonal = biomeAffinityMatrix[['seasonal']],
#	Evergreen = 1 - biomeAffinityMatrix[['seasonal']])

biomeCols = cols#c(TRFO = "#003311", TSFO = "#009900", TDFO = "#775500",
#		 WTRF = "#003333", WEFO = "#00AAAA", CTRF = "#0033DD", 
#		 WAMF = "#330033", COMI = "#33FF33", CGSS = "#993300",
#		 STEP = "#FFFF00", DESE = "#FFAAAA", CGSH = "#772255")

#biomeAffinityMatrix = cbind(biomeAffinityMatrix, 
#        "Tree.Cover" = c(1,1,1,1,1,1,1,0.5, 0.5, 0.5, 0.5, 0.0, 0.0, 0.0, 0.0))
        
# biomeAffinityMatrix = cbind(biomeAffinityMatrix,        
#        "Grass.Cover" = biomeAffinityMatrix[, "Dense"] - biomeAffinityMatrix[, "Tree.Cover"],
#        "Bare.Cover" = biomeAffinityMatrix[, "Sparse"])
        
biomeAffinityMatrix[, "Hot"] = c(1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 0, 1, 1, 0, 0)
biomeAffinityMatrix[, "Cold"] = 1 - biomeAffinityMatrix[, "Hot"]#- c(1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 0, 1, 1, 0, 0)
######################
## Plot Explanation ##
#####################'
plotAffinnityEqs <- function(affintyLine = TRUE, biomePoints = FALSE, biomeLines = FALSE,
						     biomeN = c(1, 3)) {
	png(paste('figs/AffinityEquations', affintyLine,  biomePoints, biomeLines, '.png', sep = ''),
		height = 7, width = 7, units = 'in', res = 300)
	par(mfcol = c(3, 2), mar = c(3, 2, 2, 2), oma = c(3, 1.4, 0, 1.4))
	plotAffinnityFuns <- function(x, FUN, labs, rowname, title) {
		y = affinityCovertFuns[[FUN]](x)
		if (affintyLine) type = 'l' else type = 'n'
		plot(x, y, xlab = '', ylab = '', type = type, yaxt = 'n', lwd = 2, col = "grey")
		axis(2, at = c(0, 1), label = labs)
		axis(4)
		mtext(title, cex = 1.2)
		mtext(FUN, line = 2, side = 1)

		if (biomeLines) for (i in 1:length(biomeN)) {
			bn = biomeN[i]
			lines(x, abs(biomeAffinityMatrix[bn, rowname] - y), col = biomeCols[bn],
				  lty = 1 + i, lwd = 2)
		}
		
		if (biomePoints) {
			x = try(biomeAffinityMatrix0[biomeN, FUN], silent = TRUE)
			if (class(x) == "try-error") x = biomeAffinityMatrix0[biomeN, rowname]
			
			y = rep(0, length(x))
			for (i in unique(x)) {
				index = which(x == i)
				y[index] = seq(0, 1, length.out = 30)[1:length(index)]
			}
			points(x, y, col = biomeCols[biomeN], pch = 19)
		}
	}

	plotAffinnityFuns(1:7500, "gdd", c('Cold', 'Hot'), "Hot", 'Hot/Cold')
	plotAffinnityFuns(seq(0, 30, 0.1), "height", c('Short', 'Tall'), "Tall", 'Short/Tall')
	plotAffinnityFuns(seq(6, 12, 0.1), "seasonal", c('Evergreen', 'Deciduous'), "Seasonal", 'Evergreen/Deciduous')
	axis(side = 1, at = seq(6, 12, length.out = 6), labels = seq(0, 1, 0.2), line = 3)
	mtext('Deciduous Cover', line = 5, side = 1)
    
	plotAffinnityFuns(seq(0, 1, 0.01), "cover", c('Sparse', 'Dense'),  "Dense",  'Vegetation cover')
	plotAffinnityFuns(seq(0, 1, 0.01), "cover", c('Barren', 'Covered'),  "Sparse",  'Bare cover')

	if (biomeLines)
		legend(x = 'topleft', c('Affinity', 'Tropical Rainforest', 'Savanna'),
			   col = c('grey', biomeCols[c(1,3)]), lty = c(1, 2, 3))
			   
	if (biomePoints)
		legend(x = 'bottomright', biomeAffinityMatrix[,1], ncol = 2, col = biomeCols, pch = 19)
	
	mtext('Difference from biome', side = 4, outer = TRUE)
	mtext('Affinity', side = 2, outer = TRUE)
	dev.off.gitWatermark()
}

plotAffinnityEqs()
plotAffinnityEqs(FALSE, TRUE)
plotAffinnityEqs(TRUE, TRUE	)
plotAffinnityEqs(TRUE, TRUE, TRUE)

##################################
## plot							##
##################################

makeAffinity <- function(dat) {
	Hot = dat[['gdd']]
	Cold = 1 - Hot
	
	Dense  = dat[['Dense' ]]
	Sparse = dat[['Sparse']]
	
	Tall = sum((dat[['height']] * dat[['fpc']])[[Trees]]) / sum(dat[['fpc']][[Trees]])
	Short = 1 - Tall

	EG  = sum(Evergreen * dat[['fpc']] * dat[['lai']], na.rm = TRUE) / #
		sum(dat[['fpc']] * dat[['lai']], na.rm = TRUE) ## weight by LAI
	DEC = 1 - EG
	
	Aff = addLayer(Hot, Cold, Dense, Sparse, Tall, Short, DEC, EG)
	names(Aff) = c( "Hot", "Cold", "Dense", "Sparse", "Tall", "Short",
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

plot_Affinitys <- function(Aff, plotBiome = TRUE,                           
							    AffCols = c("#002200", "#999900", "white"),
								AffLims = seq(0.1, 0.9, 0.1), dev = TRUE,...) {
	if (dev) { 
        dev.new()
        par(mfrow = c(4, 4), mar = rep(0, 4))
    }
	biomeAffinityMatrix =  biomeAffinityMatrix[, c("Biome", names(Affs[[1]]))]
	Affinity = apply(biomeAffinityMatrix, 1, plot_affinity2biome, Aff, AffCols, AffLims,  ...)
	
	Affinity =  layer.apply(Affinity, function(i) i)
	
    if (is.list(Aff)) {
        plot.new()
        add_raster_legend2(cols = AffCols, limits = AffLims, srt=0,
                           extend_max = TRUE, extend_min = TRUE, transpose = FALSE, 
                           plot_loc = c(0.1, 0.9, 0.85, 0.9))
    }
	if (!plotBiome) return()
	
	biome = which.min(Affinity)
	
	
	plot_SA_Map_standard(biome, cols = biomeCols, limits = 0.5 + (1:(length(biomeCols)-1)), 
                         add_legend = FALSE, readyCut = TRUE)
	
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
		 AffLims = c(-0.5, -0.3, -0.2, -0.1, -0.01, 0.01, 0.1, 0.2, 0.3, 0.5),
		 AffLabs = NULL)

biomeAffinityMatrix = biomeAffinityMatrix[c(1,2, 8, 12, 13),]

png('figs/Affinity.png', height = 7, width = 7, res = 300, units = 'in')

    layout(rbind(1:3,c(4, 5, 0), 6))#, heights = c(1, 1, 0.3))
    par(mar = rep(0,4))
    plot_Affinitys(list(Affs[[1]], Affs[[2]]), plotBiome = FALSE,
             AffCols = c("#002200", "#999900", "white", "#990099", "#000022"),
             AffLims = c(-0.5, -0.3, -0.2, -0.1, -0.01, 0.01, 0.1, 0.2, 0.3, 0.5), dev = FALSE)
             
dev.off()