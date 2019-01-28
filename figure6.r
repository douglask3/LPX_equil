graphics.off()
library(ncdf4)
library(rasterExtras)
library(rasterPlot)
library(mapdata)
library(mapplots)
sourceAllLibs('../rasterextrafuns/rasterPlotFunctions/R/')
source("libs/biome_assign.r")
source("libs/openMaskVals.r")
source("libs/addPointsStandard.r")

precip = 'data/Figures_doug/Figure 2_6/LGM_R20C2_detr_ensemble_hdx_pre_ave_cropped.nc'

dir = "data/Figures_doug/Figure 2_6/"

cols   = c("white", "#FF9900", "#110000")
limits = c(0.001, 0.01, 0.02, 0.05, 0.1, 0.2)
cols   = make_col_vector(cols, ncols = length(limits) + 1)

files = c("a) control"            = "4ave_pico2_foff.nc", 
          "b) fire only"          = "4ave_pico2_fon.nc", 
          "c) low [~CO2~] only"   = "4ave_foff.nc" , 
          "b) fire & low [~CO2~]" = "4ave_fon.nc")

          
nms = names(files)
precip = raster(precip)
tree = lapply(paste0(dir, files), brick, varname = "fpc_grid")
fire = lapply(paste0(dir, files), brick, varname = "mfire_frac")

tree = lapply(tree, function(i) sum(i[[1:7]]))
fire = lapply(fire, function(i) sum(i))


plot_figure <- function(i, density = TRUE) {
    tree = tree[[i]]; fire = fire[[i]]; nm = nms[[i]]
    mask = !is.na(tree + fire + precip) & (precip + tree + fire) < 9E9
    
    tree = tree[mask]
    fire = fire[mask]
    precip = precip[mask] * 12
    
    fire = cut_results(fire, limits)
    
    if (length(unique(fire)) == 1) col = "black" else col = cols[fire]
    
    plot(range(precip), range(tree), type = 'n', xlab = '', ylab = '', xaxt = 'n', yaxt = 'n', xlim = c(0, 4000))
    mtext.units(side = 3, nm, adj = 0.05, line = -1.5)
    if (density)
        for (cex in seq(0, 1, 0.1)) points(precip, tree, col = make.transparent('black', 0.99), pch = 19, cex = cex)
    else addPointsStandard(precip, tree, col = col)
}

plot_window <- function(density) {
    fname = 'figs/figure6'
    if (density) fname = paste0(fname, '-density')
    fname = paste0(fname, '.png')
    
    png(fname, height = 7, width = 7, units = 'in', res = 300)
        layout(rbind(1:2, 3:4, 5), heights = c(1,1,0.4))
        par( mar = c(1, 1, 0, 0), oma = c(0,4, 0,0))
        plot_figure(1, density)
        axis(2)
        plot_figure(2, density)
        plot_figure(3, density)
        axis(1)
        axis(2)
        plot_figure(4, density)
        axis(1)

        if (!density) {
            plot.new()
            legend = paste(head(limits, -1), limits[-1], sep = ' - ' )
            legend = c(paste('<', limits[1]), legend, paste('>', tail(limits, 1)))
            legend('center', horiz = TRUE, pch = 19, col = 'black', pt.cex = 2.5, legend, bty = 'n')
            legend('center', horiz = TRUE, pch = 19, col = cols, pt.cex = 2, legend, bty = 'n')
        }
    dev.off()
}

plot_window(FALSE)
plot_window(TRUE)