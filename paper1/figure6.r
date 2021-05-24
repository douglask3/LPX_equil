graphics.off()
library(ncdf4)
library(rasterExtras)
library(rasterPlot)
library(mapdata)
library(mapplots)


precip = 'data/Figures_doug/Figure 2_6/LGM_R20C2_detr_ensemble_hdx_pre_ave_cropped.nc'

greens9 = c("#F7FFFB", "#DEF7EB", "#C6EFDB", "#9EE1CA", "#6BD6AE", "#42C692", "#21B571", "#089C51", "#086B30")

dir = "data/4_models_raw_output/"

cols   = c("white", "#FF9900", "#110000")
limits = c(0.001, 0.01, 0.02, 0.05, 0.1, 0.2)
cols   = make_col_vector(cols, ncols = length(limits) + 1)

files = c("a) control"            = "_pico2_FOFF.nc", 
          "b) fire only"          = "_pico2_FON.nc", 
          "c) low [~CO2~] only"   = "_foff.nc" , 
          "b) fire & low [~CO2~]" = "_fon.nc")

          
nms = names(files)
precip = raster(precip)

load_all_mod_dat <- function(fname, layers = NULL, ...) {
    files = list.files(dir, full.names = TRUE)
    files = files[grepl(fname,files)]
    if (length(files) > 4) browser()
    
    load_file <- function(file) {
        r = brick(file, ...)
        if (is.null(layers)) r = sum(r)
            else r = sum(r[[layers]])
        return(r)
    }
    r = layer.apply(files, load_file)
    
    r[r > 9E9] = NaN
    return(r)    
}

tree = lapply(files, load_all_mod_dat, varname = "fpc_grid", layers = 1:7)
fire = lapply(files, load_all_mod_dat, varname = "mfire_frac")
fire[[1]] = fire[[2]]; fire[[3]] = fire[[4]]

logit <- function(x) log((x)/(1-x))
plot_figure <- function(i, density = TRUE) {
    tree = tree[[i]]; fire = fire[[i]]; nm = nms[[i]]
    
    mask = all(!is.na(tree + fire + precip) & (precip + tree + fire) < 9E9)
    
    tree = tree[mask]
    
    tree = logit(squeeze(tree, 0.01)/0.95)
   
    fire = fire[mask]
    precip = precip[mask] * 12
    precip = matrix(rep(precip, 4), ncol = ncol(tree))
    fire = cut_results(fire, limits)
    
    if (length(unique(t(unique(fire)))) == 1) col = "black" else col = cols[fire]
    
    plot(range(precip), range(tree), type = 'n', xlab = '', ylab = '', xaxt = 'n', yaxt = 'n', xlim = c(0, 4000), ylim = c(-5, 5.3))
   
    if (density) {
        cols = unlist(lapply(1:8,function(i) rep(rev(greens9)[i], 9-i)))
        cols = densCols(precip, tree, colramp = colorRampPalette(rev(cols)), bandwidth = 0.02)
        points(tree~precip, pch = 20, cex = 2, col = cols)#make.transparent(cols, 0.9))
    } else addPointsStandard(precip, tree, col = col)
     mtext.units(side = 3, nm, adj = 0.05, line = -1.5)
}

squeeze <- function(x, f) x * (1-2*f) + f

plot_window <- function(density) {
    fname = 'figs/figure6'
    if (density) fname = paste0(fname, '-density')
    fname = paste0(fname, '.png')
    
    png(fname, height = 7, width = 7, units = 'in', res = 300)
        layout(rbind(1:2, 3:4, 5), heights = c(1,1,0.4))
        par( mar = c(1, 1, 0, 0), oma = c(0,4, 1,1))
        plot_figure(1, density)
        axis2 <- function(at = c(0.001, 0.01, 0.1, 0.5, 0.9, 0.95)) axis(2  , at = logit(squeeze(at, 0.01)/0.95), labels = at)
        axis2()
        plot_figure(2, density)
        plot_figure(3, density)
        axis(1)
        axis2()
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