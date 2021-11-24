graphics.off()
library(ncdf4)
library(rasterExtras)
library(rasterPlot)
library(mapdata)
library(mapplots)
source("cfg.r")

precip = 'data/pre/'

greens9 = c("#F7FFFB", "#DEF7EB", "#C6EFDB", "#9EE1CA", "#6BD6AE", "#42C692", "#21B571", "#089C51", "#086B30")
dcols = unlist(lapply(1:8,function(i) rep(rev(greens9)[i], (9-i)^2)))

dir = "data/4_models_raw_output/"

cols   = c("white", "#FF9900", "#110000")
limits = c(0.001, 0.01, 0.02, 0.05, 0.1, 0.2)
cols   = make_col_vector(cols, ncols = length(limits) + 1)
yticks = c(0.01, 0.1, 0.5, 0.9, 0.95)
xticks = seq(0, 5000, 1000)
files = c("a) control"            = "_pico2_FOFF.nc", 
          "b) fire only"          = "_pico2_FON.nc", 
          "c) low [~CO2~] only"   = "_foff.nc" , 
          "b) fire & low [~CO2~]" = "_fon.nc")

          
nms = names(files)

precip = yay = list.files(precip, full.names = TRUE)
precip = layer.apply(precip, raster)
#precip = layer.apply(precip, function(i) { i[is.na(precip[[2]])] = NaN; i}) 

precip[[1]] = precip[[1]] * 10
precip[[3]] = precip[[3]] * 30
precip = precip * 12

load_all_mod_dat <- function(fname, layers = NULL, ...) {
    files = list.files(dir, full.names = TRUE)
    files = files[grepl(fname,files)]
    #browser()
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
    
    mask = all(!is.na(tree + fire + precip) & (precip + tree + fire) < 9E9 & tree>0.0001)
    
    tree = tree[mask]
    
    tree = logit(squeeze(tree, 0.01)/0.95)
   
    fire = fire[mask]
    
    precip = precip[mask]
    #precip = matrix(rep(precip, 4), ncol = ncol(tree))
    fire = cut_results(fire, limits)
    
    if (length(unique(t(unique(fire)))) == 1) col = "black" else col = cols[fire]
    
    plot(range(precip), range(tree), type = 'n', xlab = '', ylab = '', 
         xaxt = 'n', yaxt = 'n', xlim = c(0, 4000), ylim = c(-5, 5.3))
    for (i in logit(squeeze(yticks, 0.01)/0.95)) lines(c(-9E9, 9E9), c(i, i), lty = 3, col = 'grey')
    for (i in xticks) lines(c(i, i), c(-9E9, 9E9), lty = 3, col = 'grey')
    if (density) {
        precip = as.vector(precip)#[,4])
        tree = as.vector(tree)#[,4])
        cols = densCols(precip, tree, colramp = colorRampPalette(rev(dcols)), bandwidth = 0.1)
        
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
        axis2 <- function(at = yticks) axis(2, at = logit(squeeze(at, 0.01)/0.95), labels = at*100)
        axis2()
        plot_figure(2, density)
        plot_figure(3, density)
        axis(1)
        axis2()
        plot_figure(4, density)
        axis(1)
        mtext(outer = TRUE, side = 2, 'Tree cover (%)', adj = 1-1/2.4, line = 1.5)
        mtext.units(outer = TRUE, side = 1, 'Mean annual precipitation (mm ~yr-1~)', line = -7.2)
        
        if (density) {
            plot.new()
            cols = rev(unique(dcols))
            ncols = sapply(cols, function(i) sum(dcols == i))
            add_raster_legend2(cols = c("white", greens9), limits = c(0, ncols), extend_max = TRUE, labelss = c(0, ncols),
                               transpose = FALSE, plot_loc = c(0.1, 0.9, 0.35, 0.45), srt = 0, ylabposScling=0.1)
            mtext(side = 1, 'counts per 100x100 bin', line = -0.5)
            
        } else {
            plot.new()
            legend = paste(head(limits, -1), limits[-1], sep = ' - ' )
            legend = c(paste('<', limits[1]), legend, paste('>', tail(limits, 1)))
            legend('center', horiz = TRUE, pch = 19, col = 'black', pt.cex = 2.5, legend, bty = 'n')
            legend('center', horiz = TRUE, pch = 19, col = cols, pt.cex = 2, legend, bty = 'n')
        }
    dev.off()
}

#plot_window(FALSE)
plot_window(TRUE)