graphics.off()
source("cfg.r")


limits = seq(1.5, length.out = length(cols) - 1)

plot_biomes <- function(r, name) {    
    plot_map_standrd(r, cols, limits)
    mtext.units(name, adj = 0.9, line = -2)   
    return(unique(r))
}

plotFigure <- function(dat, name) {
    fname = paste0('figs/Figure1', name, '.png')
    png(fname, height = 7, width = 5, res = 300, units = 'in')
        layout(rbind(1:2, 3:4, 5), heights = c(1,1, 0.3))
        par( mar = rep(0, 4), oma = c(0, 0, 0, 0.5))
    
        bv = mapply(plot_biomes, dat, names(files))
        bv = sort(unique(unlist(bv)))
        cols = c(cols[bv], 'ice' = "#CCCCCC")
        legend('left', col = cols, legend = names(cols), pch = 15, ncol = 2, pt.cex = 3)
    dev.off()
}

plotFigure(dat, '')
plotFigure(ddat, '-diff')