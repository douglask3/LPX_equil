graphics.off()
source("cfg.r")
library(raster)


site_file = 'data/data_model_comparison_DMM_with_master_corelist.csv'

limits = seq(1.5, length.out = length(cols) - 1)

site_dat = read.csv(site_file, stringsAsFactors=FALSE)
site_lat = as.numeric(site_dat[,"LATITUDE"])
site_lon = as.numeric(site_dat[,"LONGITUDE"] )
site_bnm = as.numeric(site_dat[,"POLLEN_TO_LPX_BIOME_NUMBER"])

plot_biomes <- function(r, name, tpoints = TRUE) {  
    plot_map_standrd(r, cols, limits)
    mtext.units(name, adj = 0.9, line = -2)   
    if (tpoints) {
        points(site_lon, site_lat, pch = 19, cex = 1.3)
        points(site_lon, site_lat, col = 'white', pch = 19)
        points(site_lon, site_lat, col = cols[site_bnm + 1], pch = 19, cex = 0.7)
    }
    return(unique(r))
}

plotFigure <- function(dat, name, tpoints = TRUE) {
    fname = paste0('figs/Figure1', name, '.pdf')
    print(fname)
    pdf(fname, height = 7, width = 5.35)#, res = 300, units = 'in')
        layout(rbind(1:2, 3:4, 5), heights = c(1,1, 0.3))
        par( mar = rep(0, 4), oma = c(0, 0, 0, 0.5))
    
        bv = mapply(plot_biomes, dat, names(files), c(T, rep(tpoints, 3)))
        bv = sort(unique(unlist(bv)))
        cols = c(cols[bv], 'ice' = "#CCCCCC")
        colsi = unique(cols) 
        index = sapply(colsi, function(i) which(cols == i)[1])
        cols = cols[index]
        legend('left', col = cols, legend = names(cols), pch = 15, ncol = 2, pt.cex = 3)
    dev.off()
    print("plotted")
}

PlotFigures <- function(dat, ddat, fname = '', tpoints = TRUE) {
    plotFigure(dat, fname, tpoints)
    plotFigure(ddat, paste0('-diff', fname), FALSE)
}

c(dat, ddat, files) := load_biomes(quick = FALSE)
PlotFigures(dat, ddat)

biome_dir = 'data/4_models_raw_output/'

forModel <- function(name) {
    
    biome_files = paste0(name, c("_pico2_FOFF", "_pico2_FON", "_foff", "_fon"), ".nc")
    names(biome_files) = c("a) control", "b) fire only", "c) low [~CO2~]\nonly", "d) fire &\nlow [~CO2~]")

    tas_file = 'data/Figures_doug/Figure 2_6/LGM_R20C2_detr_ensemble_hdx_tmp_ave_cropped.nc'

    c(dat, ddat) := load_biomes(biome_dir, biome_files, tas_file, quick = FALSE)
    PlotFigures(dat, ddat, name, FALSE)
}

lapply(c("HAD", "CNRM", "FGOALS", "MIROC"), forModel)