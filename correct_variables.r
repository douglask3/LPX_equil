#source("cfg.r")
library(raster)
library(rasterExtras)
source("libs/biome_assign.r")
graphics.off()
##################
## setup/params ##
##################

site_file = 'data/data_model_comparison_DMM_with_master_corelist.csv'

mod_dir = 'data/4_models_raw_output/'

pattern = '_fon'

mod_files = list.files(mod_dir, pattern = pattern, full.name = TRUE)

varnames = c("fpc_grid")

levelss = list(1:9)
aggFUNs = list(sum)

transs  = list(logit)
itranss = list(logistic)

limitss = list(c(0, 0.1, 0.2, 0.4, 0.6, 0.8, 0.9))
colss = list(c('#ffffe5','#f7fcb9','#d9f0a3','#addd8e','#78c679','#41ab5d','#238443','#006837','#004529'))

dlimitss1 = list(seq(-4, 4))
dlimitss2 = list(c(-0.5, -0.4, -0.3, -0.2, -0.1, 0.1, 0.2, 0.3, 0.4, 0.5))
dcolss = list(rev(c('#8c510a','#bf812d','#dfc27d','#f6e8c3','#f5f5f5','#c7eae5','#80cdc1','#35978f','#01665e')))

zlimits =  c(-4, -3, -2, -1, -0.1, 0.1, 1, 2, 3, 4)
zcols = rev(c('#7f3b08','#b35806','#e08214','#fdb863','#fee0b6','#f7f7f7','#d8daeb','#b2abd2','#8073ac','#542788','#2d004b'))

#################
## open things ##
#################

## site data
site_dat = read.csv(site_file, stringsAsFactors=FALSE)
site_dat = as.matrix(site_dat[c("LONGITUDE", "LATITUDE", "POLLEN_TO_LPX_BIOME_NUMBER")])
site_dat = site_dat[!apply(site_dat, 1, function(i) any(is.na(i))), ]
site_dat = cbind(site_dat, t(sapply(site_dat[,3], variable_from_biome)))

apply2Var <- function(varname, levels, aggFUN, limits, cols, dlimits1, dlimits2, dcols,
                      trans = function(x) x, itrans = trans) {
## Model data
    openDat <- function(file) {
        dat = brick(file, varname = varname)
        if (!is.null(levels)) dat = dat[[levels]]   
        if (!is.null(aggFUN)) dat = aggFUN(dat)
        dat[dat > 9E9] = NaN
        return(dat)
    }

    dats = lapply(mod_files,  openDat)
    dats = c(dats, mean(layer.apply(dats, function(i) i )))

#########################
## perform corrections ##
#########################
    calCorrection <- function(dat) {        
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
            x = trans(x)
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

        return(p)
    }
    cors = lapply(dats, calCorrection)
    cdat = mapply(function(i, j) itrans(trans(dat) - p), dats, cors)
    zscoring <- function(dat) {
        dat = trans(dat)
        return(dat - mean(dat[], na.rm = TRUE))/sd(dat[], na.rm = TRUE)
    }
    zscores = lapply(dats, zscoring)
    czscore = lapply(cdat, zscoring)

    png("figs/fpc_correction.png", width = 7.2, height = 10, res = 300, units = 'in')
    par(mfrow = c(6, length(dats)), mar = rep(0, 4), oma = c(0, 0, 2, 2))
#########################
## Plot variable       ##
#########################
    plotMap <- function(..., text2 = '', text3 = '') {
        plot_SA_Map_standard(..., add_legend = FALSE)
        #if (sites) points(site_dat[,1], site_dat[, 2], pch = 19)
        mtext(text3, side = 3)
    }
    plotLeg <- function(...) 
        add_raster_legend2(plot_loc = c(0.7,1.3,-0.5,-0.45), ...)
        
    modnames = sapply(sapply(mod_files, function(file) tail(strsplit(file, '/')[[1]], 1)),
                      function(nm) strsplit(nm, "_fon.nc")[[1]][1])
    modnames = c(modnames, 'ensemble')
    mapply(plotMap, dats, text3 = modnames, MoreArgs = list(limits = limits, cols = cols))
    mtext(side = 4, 'Model output')
    plotLeg(cols = cols, limits = limits, maxLab = 1)
    
    lapply(zscores, plotMap, limits = zlimits, cols = zcols)
    mtext(side = 4, 'z score')
    plotLeg(cols = zcols, limits = zlimits)

    lapply(cors, plotMap, limits = dlimits1, cols = dcols)
    mtext(side = 4, 'correction')
    plotLeg(cols = dcols, limits = dlimits1)

    lapply(cdat, plotMap, limits = limits, cols = cols)
    mtext(side = 4, 'corrected output') 
    plotLeg(cols = cols, limits = limits)
   
    lapply(mapply('-', cdat, dats), plotMap, limits = dlimits2, cols = dcols)
    mtext(side = 4, 'difference')
    plotLeg(cols = dcols, limits = dlimits2)

    lapply(czscore, plotMap, limits = zlimits, cols = zcols)
    mtext(side = 4, 'corrected z score')
    plotLeg(cols = zcols, limits = zlimits)
    dev.off()
    return(list(zscores, czscore))
}

##############################
## Connectivity score       ##
##############################
zscores = mapply(apply2Var,  varnames, levelss, aggFUNs,
                 limitss, colss, dlimitss1, dlimitss2, dcolss,
                  transs, itranss)


