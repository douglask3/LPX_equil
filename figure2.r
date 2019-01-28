graphics.off()
source("cfg.r")
source("libs/biome_assign.r")

source("libs/load_biome_outs_and_info.r")

precip = 'data/Figures_doug/Figure 2_6/LGM_R20C2_detr_ensemble_hdx_pre_ave_cropped.nc'
tas    = 'data/Figures_doug/Figure 2_6/LGM_R20C2_detr_ensemble_hdx_tmp_ave_cropped.nc'

precip = openMaskVals(precip) * 12  
tas    = openMaskVals(tas   )

plotBiomeScatter <- function(r, nm, ...) {
    print("yay")
    mask = !is.na(r + precip + tas)
    precip = precip[mask]
    tas    = tas   [mask]
    r      = r     [mask]
    plot(tas, precip, type = 'n', xlim = c(-5, 26), xaxt = 'n', yaxt = 'n', ...)

    col = cols[r]
    
    addPointsStandard(tas, precip, col)
    nm = paste(strsplit(nm, '\n')[[1]], collapse=' ')
    
    mtext.units(nm, side = 3, adj = 0.1, line = -1.5)
    return(unique(r))
}

plotFigure <- function(dat, name, ...) {
    fname = paste0('figs/figure2', name, '.png')
    png(fname, height = 7, width = 7, units = 'in', res = 300)
    layout(rbind(1:2, 3:4, 5), heights = c(1,1,0.4))
    par( mar = c(1, 1, 0, 0), oma = c(0,4, 0,0))

    v = list()
    nms = names(files)
    v[[1]] = plotBiomeScatter(dat[[1]], nms[[1]], ...)
    axis(2)
    v[[2]] = plotBiomeScatter(dat[[2]], nms[[2]], ...)
    v[[3]] = plotBiomeScatter(dat[[3]], nms[[3]], ...)
    axis(2); axis(1)
    v[[4]] = plotBiomeScatter(dat[[4]], nms[[4]], ...)
    axis(1)
    
    v = sort(unique(unlist(v)))
    cols = cols[v]
    if (cols[1] == "white") cols = cols[-1]
    
    plot.new()
    par(mar = c(0, 0, 5, 0))
    legend('bottom', col = cols, legend = names(cols), pch = 19, bty = 'n',
            horiz = TRUE, pt.cex = 3, x.intersp = 2, y.intersp = 2)

    mtext.units(side = 2, 'MAP mm/yr' , outer = TRUE, adj = 1-(1/2.4), line = 2)
    mtext.units(side = 1, 'MAT ~DEG~C', outer = TRUE, line = -7)
    dev.off()
}

#plotFigure( dat, '')
plotFigure(ddat, '-diff')
plotFigure( dat, '-log', log = 'y')
plotFigure(ddat, '-diff-log', log = 'y')