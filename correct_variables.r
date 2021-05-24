#source("cfg.r")
library(raster)
library(rasterExtras)
source("libs/biome_assign.r")


logit <- function(x, n = length(dats[[1]])) {
    x = (x * (n - 1) + 0.5)/n
    log(x/(1-x))
}

logistic <- function(x) 1/(1+exp(-x))


site_file = 'data/data_model_comparison_DMM_with_master_corelist.csv'
site_dat = read.csv(site_file, stringsAsFactors=FALSE)
site_dat = as.matrix(site_dat[c("LONGITUDE", "LATITUDE", "POLLEN_TO_LPX_BIOME_NUMBER")])
site_dat = site_dat[!apply(site_dat, 1, function(i) any(is.na(i))), ]


site_dat = cbind(site_dat, t(sapply(site_dat[,3], variable_from_biome)))

mod_dir = 'data/4_models_raw_output/'

pattern = '_fon'

mod_files = list.files(mod_dir, pattern = pattern, full.name = TRUE)

varnames = c("fpc_grid")

levelss = list(1:9)
aggFUNs = list(sum)

limitss = list(c(0, 0.1, 0.2, 0.4, 0.6, 0.8, 0.9))
colss = list(c('#ffffe5','#f7fcb9','#d9f0a3','#addd8e','#78c679','#41ab5d','#238443','#006837','#004529'))

#apply2Var <- function(varname, levels) {
#    browser()
#}

varname = varnames[1]
levels = levelss[[1]]
aggFUN = aggFUNs[[1]]
limits = limitss[[1]]
cols = colss[[1]]

openDat <- function(file) {
    dat = brick(file, varname = varname)
    if (!is.null(levels)) dat = dat[[levels]]   
    if (!is.null(aggFUN)) dat = aggFUN(dat)
    dat[dat > 9E9] = NaN
    return(dat)
}

dats = lapply(mod_files,  openDat)
dats = c(dats, mean(layer.apply(dats, function(i) i )))

par(mfcol = c(length(dats), 3), mar = rep(0, 4))

plotMap <- function(..., sites = FALSE) {
    plot_SA_Map_standard(..., limits = limits, cols = cols)
    if (sites) points(site_dat[,1], site_dat[, 2], pch = 19)
}
#lapply(dats, plotMap, sites = TRUE, maxLab = 1)

#calCorrection <- function(dat) 

dat = dats[[5]]
if (varname == "fpc_grid") dat[dat > 1] = 1
rxy = xyFromCell(dat, 1:length(dat))
getCell <- function(xy) {
    sqd = (rxy[,1]-xy[1])^2 + (rxy[,2] - xy[2])^2
    dif = sqrt(sqd)
    index = which.min(dif)
    c(rxy[index,], dat[index])
}

mod = apply(site_dat[,1:2], 1, getCell)

comb = cbind(mod[3,], site_dat[,grepl(varname, colnames(site_dat))])


calDif <- function(x) {
    if (any(is.na(x))) return(NaN)
    x = logit(x)
    if (x[1] < x[2]) out = (x[1] - x[2])
    else if (x[1] > x[3])  out = (x[1] - x[3])
    else out = 0
    return(out)
}
dif = apply(comb, 1, calDif)
xy = t(mod)[,1:2]
ra = rasterFromXYZ(cbind(xy, dif))
tps = Tps(xy, dif, lon.lat = TRUE)
p = raster(dat)
p = interpolate(p, tps)
p[is.na(dat)] = NaN

cdat = logistic(logit(dat)-p)
ddat = cdat - dat

