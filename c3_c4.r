source("cfg.r")
graphics.off()

fpc_files = list.files('data/africa_files/raw_and_biome_plots/', 
                       full.names=TRUE, recursive = TRUE)
#fpc_files = fpc_files[grepl('_raw', fpc_files)]

clim_files = list.files('data/africa_files/climate_files/',
                        full.names = TRUE, recursive = TRUE)

clim_vars  = sapply(strsplit(clim_files, 'hdx_'), function(i) i[[2]])
clim_vars  = sapply(strsplit(clim_vars, "_africa.nc"), function(i) i[[1]])

openDat <- function(..., fun = brick)
    openMaskVals(..., fun = fun, extent = extent(c(10, 40, -35, -15)))

fpc = lapply(fpc_files, openDat, varname = 'fpc_grid')

mfire_frac = lapply(fpc_files, openDat, varname = 'mfire_frac')
mfire_frac = lapply(mfire_frac, function(i) sum(i) * 100)

c3 = lapply(fpc, function(i) sum(i[[1:8]]))
c3G = lapply(fpc, function(i) i[[8]])
c4G = lapply(fpc, function(i) i[[9]])
tree = lapply(fpc, function(i) 100*sum(i[[1:7]]))
grass = lapply(fpc, function(i) sum(i[[8:9]]))
c4G_pc = mapply('/', c4G, grass)

clim = lapply(clim_files, openDat, fun = raster)
names(clim) = clim_vars

LogitSquish <- function(r) { 
    v = 10000
    r = ((v-1) * r + 0.5)/v
    r = log(r/(1-r))
}

plotSpecialBox <- function(x, y, z, limits, cols, labels, breaks, binsize = 0.5, units = '%',
                           yaxis = TRUE) {
    
    if (length(y) == 8) y = selectVeg(y)
    if (length(z) == 8) z = selectVeg(z)
    breaks1 = seq(breaks[1]-binsize/2, breaks[2]-binsize/2, length.out = 100)
    breaks2 = seq(breaks[1]+binsize/2, breaks[2]+binsize/2, length.out = 100)
    
    mids   = breaks[-1] - diff(breaks)/2
    mids   = (breaks1 +  breaks2)/2
    mids   = c(mids, rev(mids))
    cols   =  make_col_vector(cols, ncols = length(limits))      
    
    
    y = lapply(y, LogitSquish)
    
    histZ <- function(i, j) {
        j = j
        FUN1 <- function(b1, b2) {
            mask = (i > b1) & (i < b2)
            if (sum.raster(mask, na.rm = TRUE) == 0)
                return(rep(0, length(limits)))
            
            FUN2 <- function(l1) {
                mask = mask & (j >= l1)# & (i < l2)
                return(sum.raster(mask, na.rm = TRUE)^0.5)
            }
            #vs = mapply(FUN2, head(limits, -1), limits[-1])
            vs = sapply(rev(limits), FUN2)
            return(vs)
        }
        vs = mapply(FUN1, breaks1, breaks2)
        
        return(vs)
    }
    
    ys = mapply(histZ, y, z, SIMPLIFY = FALSE)
    ys = lapply(ys, '/', 2.2*max(unlist(ys)))
    plot(c(0.5, 2.5), range(breaks) - c(diff(breaks)*0.2, 0), 
         type = 'n', axes = FALSE, xlab = '', ylab = '')
    axis(3, at = 1:length(y), labels = labels, pos = LogitSquish(1),)
    
    labels = c(0, 0.001, 0.01, 0.1, 0.5, 0.9, 0.99, 0.999, 1)
    at = LogitSquish(labels)
    if (yaxis) axis(2, at = at, labels = labels * 100)
    lapply(at, function(i) lines(c(-9E9, 9E9), c(i, i), col = 'grey', lty = 2))
    
    add_shape <- function(yi, x) {
        addPolygon <- function(d, col) {
            d = unlist(d)
            if (sum(d) == 0) return()
            xp = x+c(d, -rev(d))
            polygon(xp, mids, col = col, border = NA)
        }
        yl = apply(yi, 1, list)
        
        mapply(addPolygon, rev(yl), cols)
        
        mx = tail(yi, 1)
        lapply(list(x+mx, x-mx), lines, mids[1:(length(mids)/2)], lwd = 2)
    }
    mapply(add_shape, ys, 1:2)
    
    scatter_leg(limits, cols, units, 15)

    axis(3, c(-9E9, 9E9), pos = LogitSquish(1), labels = c('', ''))
    axis(2, LogitSquish(c(0, 1)), labels = c('', ''))
    axis(1, c(-9E9, 9E9), pos = LogitSquish(0), labels = c('', ''))
    axis(4, LogitSquish(c(0, 1)), labels = c('', '')  )    
}

scatter_leg <- function(limits, cols, units, pch = 15, x = 'bottom'){
    leglabs = paste0(head(limits,-1), ' - ', tail(limits, -1), units)
    leglabs = c(leglabs, paste(tail(limits, 1), '+', units))
    
    if (limits[1] != 0) {
        if (limits[1] > 0) leglabs = c(paste0(0, ' - ', limits[1], units), leglabs)
        else leglabs = c(paste('<', limits[1], units), leglabs)
    }
    if (length(cols) != length(leglabs)) browser()
    legend(x = x, ncol = length(leglabs)/2, leglabs, pch = pch, pt.cex = 2, col = cols, xpd = NA)
}


plot_3.2way <- function(x, y, z, zsc, cols, limits, title = '', z_names, xlab = '', ylab = '', units = '') {
    cols = make_col_vector(cols, ncols = length(limits)+1)
    cols0 = cols
    cols = make.transparent(cols, 0.9)

    mask = all(!is.na(x + y + z[[1]] + z[[2]]))
    
    x = x[mask]
    y = y[mask]
    z = lapply(z, function(i) cols[cut_results(zsc*i[mask], limits)])
    
    
    plotFun <- function(zi, txt,  ...) {
        plot(x, y, pch = 19, cex = 2, xlab = '', ylab = '', ...)
        grid()
        mtext.units(txt, side = 3, line = 0, adj = 0.1)
        for (nn in 1:4) for (cex in seq(1.6, 0.1, -0.1))
            points(x, y, col = zi, cex = cex, pch = 19)
    }
    z_names = paste(title, '-', z_names)
    plotFun(z[[1]], z_names[1])
    mtext.units(side = 2, ylab, line = 2.2)
    plotFun(z[[2]], z_names[2], yaxt = 'n')
    
    mar = par("mar")
    par(mar = rep(0, 4))
    
    plot.new()
    mtext.units(xlab, side = 3)
    scatter_leg(limits, cols0, units, 19, x = 'top')
    par(mar = mar)
}
png('figs/c3_c4_scatter2.png', height = 13.33, width = 6, units = 'in', res = 300)
layout(rbind(1:2, 3, 4:5, 6, 7:8, 9, 10:11, 12), height = c(1, 0.2, 1, 0.2, 1, 0.2, 1, 0.2))
par(oma = c(1, 3, 1, 0), mar = c(3.5, 0.5, 1, 0.5))


selectVars <- function(vname) 
    layer.apply(clim[names(clim) == vname], function(i) i)

selectVeg <- function(z) {
    out = lapply(1:(length(z)/4), function(i) z[(1 + (i-1)*4):(i*4)])
    lapply(out, function(i) layer.apply(i, function(j) j))
}

x = selectVars('pre')*12
y = selectVars('tmp')

PC = PolarConcentrationAndPhase(x)

z = selectVeg(c4G)
limits = c(0.0001, 0.001, 0.01, 0.1, 0.5, 0.9, 0.99, 0.999, 0.9999)*100
cols = c("white", "yellow", "brown", "#111100")
z_names = c('150ppm', '250ppm')
plot_3.2way(x, y, z, 100, cols, limits, 'C4 grass cover (% land)', z_names, '', '', '')

z = selectVeg(tree)
cols = c("white", "yellow", "green", "#003300")
plot_3.2way(x, y, z, 100, cols, limits, 'Tree cover (% land)', z_names, '', 'MAT ~DEG~C', '')


z = selectVeg(c4G_pc)
cols = c("#660000", "yellow", "white", "cyan", "#000066")
plot_3.2way(x, y, z, 100, cols, limits, 'C4 grass cover (% grass)', z_names, '', 'MAT ~DEG~C', '')

z = selectVeg(mfire_frac)
limits = c(0.001, 0.002, 0.005, 0.01, 0.002, 0.05, 0.1, 0.15, 0.2) * 100
cols = c("white", "yellow", "red", "black")
plot_3.2way(x, y, z, 1, cols, limits, 'Burnt area (% ~yr-1~)', z_names, 'Precp (mm ~yr-1~)', '', '%')
dev.off()

browser()
##############
## 1st plot ##
##############
png('figs/c3_c4_candlestick.png', height = 11, width = 11, units = 'in', res = 300)
par(mfrow = c(2,2), oma = c(3, 3, 1.5, 0.5), mar = c(1, 1, 0.5, 0.5))

breaks = LogitSquish(c(0, 1))
binsize = 0.5
limits = c(0, 0.001, 0.002, 0.005, 0.01, 0.002, 0.05, 0.1, 0.15, 0.2) * 100
cols = c("white", "yellow", "red", "black")
labels = c("150ppm", "250ppm")

plotSpecialBox(c(150, 250), c4G_pc , mfire_frac, limits, cols, labels, breaks, units = '')
mtext.units(side = 1, "Burnt area (% ~yr-1~)", line = -4)
mtext.units(side = 3, '% Grasses', line = -3, adj = 0.02)

plotSpecialBox(c(150, 250), c4G , mfire_frac, limits, cols, labels, breaks, units = '', yaxis = FALSE)
mtext.units(side = 1, "Burnt area (% ~yr-1~)", line = -4)
mtext.units(side = 3, '% Land', line = -3, adj = 0.02)

##############
## 2nd plot ##
##############
limits = c(0, 0.01, 0.03, 0.1, 0.3, 1, 3, 10, 30, 50)
cols = c("white", "yellow", "green", "black")
labels = c(" ", " ")

plotSpecialBox(c(150, 250), c4G_pc , tree, limits, cols, labels, breaks)
mtext.units(side = 1, "Tree Cover", line = -4)
mtext.units(side = 3, '% Grasses', line = -3, adj = 0.02)


##############
## 2nd plot ##
##############
limits = c(0, 10, 20, 40, 60, 80, 100, 120, 140, 160)*10
cols = c("white", "#44FF44", "blue", "black")
labels = c(" ", " ")

plotSpecialBox(c(150, 250), c4G_pc , c(clim[['pre']], clim[['pre']]), limits, cols, labels, 
               breaks, units = '', yaxis = FALSE)
mtext.units(side = 1, "Precip. (mm ~yr-1~)", line = -4)
mtext.units(side = 3, '% Grasses', line = -3, adj = 0.02)



mtext.units(side = 3, "~CO2~ concentration", outer = TRUE)
mtext(side = 2, "C4 grass cover (%)", outer = TRUE, line = 1.7)
dev.off()
