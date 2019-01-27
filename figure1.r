graphics.off()
library(raster)
library(ncdf4)
library(rasterExtras)
library(rasterPlot)
library(mapdata)
library(mapplots)
sourceAllLibs('../rasterextrafuns/rasterPlotFunctions/R/')
source("libs/biome_assign.r")

source("libs/load_biome_outs_and_info.r")
limits = seq(1.5, length.out = length(cols) - 1)




plot_biomes <- function(r, name) {    
    plot_map_standrd(r, cols, limits)
    mtext.units(name, adj = 0.9, line = -2)   
    return(unique(r))
}

plotFigure <- function(dat, name) {
    fname = paste0('figs/Figure1', name, '.pdf')
    pdf(fname, width = 5, height = 5)
    par(mfrow = c(2,2), mar = rep(0, 4))
    bv = mapply(plot_biomes, dat, names(files))
    bv = sort(unique(unlist(bv)))
    cols = c(cols[bv], 'ice' = "#CCCCCC")
    legend('left', col = cols, legend = names(cols), pch = 15, ncol = 2, pt.cex = 3)
    dev.off()
}

plotFigure(dat, '')
plotFigure(ddat, '-diff')