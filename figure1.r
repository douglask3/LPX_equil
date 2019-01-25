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


plot_map_standrd <- function(r, cols, limits, ...) {
    
    plot_map <- function(colsi, ...) 
        plot_raster_from_raster(r, cols = colsi, limits = limits, quick = TRUE, readyCut = TRUE, coast.lwd = NULL, add_legend = FALSE,...)
    
    openResampleMask <- function(file, ...) {
        mask = raster(file)
        mask = raster::resample(mask, r)
        mask = disaggregate(mask, 5)
    }
    
    plot_map(cols)
    
    mask = openResampleMask('data/seamask.nc')
    contour(mask, add = TRUE, drawlabels = FALSE, lwd = 0.1)

    plot_map(cols = make.transparent(cols,0.5), add = TRUE)

    mask = openResampleMask('data/icemask.nc')
    plot_raster_from_raster(mask, col = c('transparent', '#CCCCCC'), limits = c(0.5), coast.lwd = NULL, add = TRUE, quick = TRUE, add_legend = FALSE)
}

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