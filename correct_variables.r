source('../gitProjectExtras/gitBasedProjects/R/sourceAllLibs.r')
sourceAllLibs('../gitProjectExtras/gitBasedProjects/R/', trace = FALSE)
sourceAllLibs('../rasterextrafuns/rasterPlotFunctions/R/', trace = FALSE)
sourceAllLibs('../rasterextrafuns/rasterExtras/R/', trace = FALSE)

library(raster)
#library(rasterExtras)
library(fields)
sourceAllLibs("libs/", trace = FALSE)
graphics.off()

plot2File = TRUE

##################
## setup/params ##
##################

site_file = 'data/data_model_comparison_DMM_with_master_corelist.csv'

mod_dir = 'data/4_models_raw_output/'
tas_dir =  'data/Figures_doug/Figure 2_6/'
pattern = '_fon'
pres = 100

varnames = list(tropical = "tropical", temperate = "temperate", evergreen = "evergreen",
                gdd = "gdd", height = c("height", "fpc_grid"), fpc = "fpc_grid")

logitX <- function(r, ..., X = 120) 
    logit(r/X, ...)
    
logitsticX <- function(r, ..., X = 120)
    X*logistic(r, ...)

transs  = list(logit, logit, logit, logN, logitX, logit)
itranss = list(logistic, logistic, logistic, exp, logitsticX, logistic)

levelss = list(c(1:2, 9), c(3:5, 8), c(1, 3, 4, 6), NaN, 1:9, 1:9)
aggFUNs = list(blankFun, blankFun, blankFun, blankFun, sumr, sumr)

limitss = list(c(0, 0.01, 0.05, 0.1, 0.2, 0.5, 0.8, 0.9, 0.95, 0.99),
               c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9),
               c(0, 0.01, 0.05, 0.1, 0.2, 0.5, 0.8, 0.9, 0.95, 0.99),
               c(0, 100, 200, 300, 350, 400, 450),
               c(0, 1, 2, 4, 6, 8, 10, 15),
               c(0, 0.1, 0.2, 0.4, 0.6, 0.8, 0.9))

contours = list(0.5, 0.5, 0.5, 350, c(5, 10), c(0.3, 0.6))

maxLab = list(1, 1, 1, NULL, NULL, NULL)

colss = list(c('#543005','#8c510a','#bf812d','#dfc27d','#f6e8c3',
               '#f5f5f5','#c7eae5','#80cdc1','#35978f','#01665e','#003c30'),
             c('#543005','#8c510a','#bf812d','#dfc27d','#f6e8c3',
               '#f5f5f5','#c7eae5','#80cdc1','#35978f','#01665e','#003c30'),  
             c('#543005','#8c510a','#bf812d','#dfc27d','#f6e8c3',
               '#f5f5f5','#c7eae5','#80cdc1','#35978f','#01665e','#003c30'),  
             rev(c('#d73027','#f46d43','#fdae61','#fee090','#ffffbf',
                   '#e0f3f8','#abd9e9','#74add1','#4575b4')),
             c('#fff7fb','#ece2f0','#d0d1e6','#a6bddb','#67a9cf',
               '#3690c0','#02818a','#016c59','#014636'),
             c('#ffffe5','#f7fcb9','#d9f0a3','#addd8e','#78c679',
               '#41ab5d','#238443','#006837','#004529'))

dlimitss1 = list(c(-6, -4, -2, -1, 1, 2, 4, 6),
                 seq(-4, 4),
                 c(-6, -4, -2, -1, 1, 2, 4, 6),
                 seq(-0.5, 0.5, 0.5),
                 c((-4):4),
                 seq(-4, 4))
dlimitss2 = list(c(-0.8, -0.6, -0.4, -0.2, -0.1 , 0.1, 0.2, 0.4, 0.6, 0.8),
                 c(-0.8, -0.6, -0.4, -0.2, -0.1 , 0.1, 0.2, 0.4, 0.6, 0.8),
                 c(-0.6, -0.4, -0.2, -0.1, -0.05, 0.05, 0.1, 0.2, 0.4, 0.6),
                 c(-140, -120, -100, -80, 60, -40, -20, 0, 20),
                 c(-10, -5, -2, -1, -0.5, -0.2, 0.2, 0.5, 1, 2, 5, 10),
                 c(-0.5, -0.4, -0.3, -0.2, -0.1, 0.1, 0.2, 0.3, 0.4, 0.5))
dcolss = list(c('#8e0152','#c51b7d','#de77ae','#f1b6da','#fde0ef',
                 '#f7f7f7','#e6f5d0','#b8e186','#7fbc41','#4d9221','#276419'),
              c('#8e0152','#c51b7d','#de77ae','#f1b6da','#fde0ef',
                 '#f7f7f7','#e6f5d0','#b8e186','#7fbc41','#4d9221','#276419'),
              c('#8e0152','#c51b7d','#de77ae','#f1b6da','#fde0ef',
                 '#f7f7f7','#e6f5d0','#b8e186','#7fbc41','#4d9221','#276419'),
              rev(c('#7f3b08','#b35806','#e08214','#fdb863','#fee0b6',
                    '#f7f7f7','#d8daeb','#b2abd2','#8073ac','#542788','#2d004b')),
              c('#40004b','#762a83','#9970ab','#c2a5cf',
                    '#e7d4e8','#f7f7f7','#d9f0d3','#a6dba0','#5aae61','#1b7837','#00441b'),
              rev(c('#8c510a','#bf812d','#dfc27d','#f6e8c3','#f5f5f5',
                    '#c7eae5','#80cdc1','#35978f','#01665e')))

zlimits =  c(-8, -4, -2, -1, 0, 1, 2, 4, 8)
zcols = rev(c('#7f3b08','#b35806','#e08214','#fdb863','#fee0b6','#f7f7f7','#d8daeb','#b2abd2','#8073ac','#542788','#2d004b'))

conn_cols = c('#ffffe5','#f7fcb9','#d9f0a3','#addd8e',
              '#78c679','#41ab5d','#238443','#006837','#004529')
conn_limits = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)  


biome_cols =  c(' ' = 'white'  , Thf = '#002200', Tdf = '#338800',
                wtf = '#005555', tef = '#00EE33', tdf = '#66DD00',
                bef = '#000088', bdf = '#330033',
                Ts  = '#999900', sw  = '#BB9999', tp = '#8844FF', 
                bp  = '#880088', dg  = '#FFAA00', hd = '#FFFF77', st = '#00AAEE', t = '#99CCFF')

#################
## open things ##
#################

mod_files = list.files(mod_dir, pattern = pattern, full.name = TRUE)
tas_files = list.files(tas_dir, full.names = TRUE) 
tas_file = tail(tas_files, 1)

## site data
site_dat = read.csv(site_file, stringsAsFactors=FALSE)
site_dat = as.matrix(site_dat[c("LONGITUDE", "LATITUDE", "POLLEN_TO_LPX_BIOME_NUMBER")])
site_dat = site_dat[!apply(site_dat, 1, function(i) any(is.na(i))), ]
site_dat = cbind(site_dat, t(sapply(site_dat[,3], variable_from_biome)))


apply2Var <- function(varname, name, levels, aggFUN, limits, contour, cols, dlimits1, dlimits2, dcols,
                      trans = function(x) x, itrans = trans, maxLab) {
## Model data
    openDat <- function(file, cfile, varname) {
        
        if (length(varname) == 2) {
            dat1 = brick(file, varname = varname[1])  
            dat2 = brick(file, varname = varname[2])
            if (!is.null(levels)) {
                dat1 = dat1[[levels]] 
                dat2 = dat2[[levels]]
            }
            #browser()
            datt = sum(dat2)
            dat = dat1*dat2#/datt
            dat[datt == 0] = 0
                                 
        } else if (varname == 'gdd') {
            dat = 500*(raster(cfile) > 2)
        } else if (varname == "evergreen" || varname == "tropical" || varname == "temperate") {
            if (varname == "evergreen") pfts = 1:7 else pfts = 1:9
            dat = brick(file, varname = "fpc_grid") [[pfts]]  
            datt = sum(dat) 
            dat = sum(dat[[levels]])/datt
            dat[datt>9E9] = NaN
            dat[datt==0] = 0
            
        } else {
            dat = brick(file, varname = varname)
            if (!is.null(levels)) dat = dat[[levels]] 
        }
        if (!is.null(aggFUN)) dat = aggFUN(dat)
        dat[dat > 9E9] = NaN
        return(dat)
    }
                          
    dats = mapply(openDat, mod_files, tas_file, MoreArgs = list(varname))
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
            x0 = x
            if (any(is.na(x))) return(NaN)
            print(x)
            print(length(dats[[1]]))
            x = trans(x, length(dats[[1]]))
            print(x)
            #print(trans)
            if (x[1] < x[2]) out = (x[1] - x[2])
            else if (x[1] > x[4])  out = (x[1] - x[4])
            else out = 0
            return(out)
        }
        dif = apply(comb, 1, calDif)
        
        xy = t(mod)[,1:2]
        ra = rasterFromXYZ(cbind(xy, dif))
        
        tps = Tps(xy, dif, lon.lat = TRUE) 
        
        #p = raster(dat)
        p = interpolate(dat, tps)
        p[is.na(dat)] = NaN
        
        return(p)
    }
    cors = lapply(dats, calCorrection)
    cdat = mapply(function(i, j) itrans(trans(i, n = length(dats[[1]])) - j), dats, cors)
    
    zscoring <- function(dat) {
        dat = trans(dat, n = length(dats[[1]]))
        return(dat - mean(dat[], na.rm = TRUE))/sd(dat[], na.rm = TRUE)
    }
    zscores = lapply(dats, zscoring)
    czscore = lapply(cdat, zscoring)
    
    if (plot2File) 
        png(paste0("figs/", name, "_correction.png"), width = 7.2, height = 10, 
            res = pres, units = 'in')
    else options(repr.plot.width=7.2, repr.plot.height=10)
    par(mfrow = c(6, length(dats)), mar = rep(0, 4), oma = c(0, 0, 2, 2))
        
#########################
## Plot variable       ##
#########################
    plotMap <- function(x, ..., text2 = '', text3 = '', addContour = FALSE) {
        plot_map_standrd(x, ..., readyCut = FALSE, add_legend = FALSE)
        if (addContour) contour(x, levels = contour, add = TRUE)
        #if (sites) points(site_dat[,1], site_dat[, 2], pch = 19)
        mtext(text3, side = 3)
    }
    plotLeg <- function(...) 
        add_raster_legend2(plot_loc = c(0.7,1.3,-0.5,-0.45), ...)
        
    modnames = sapply(sapply(mod_files, function(file) tail(strsplit(file, '/')[[1]], 1)),
                      function(nm) strsplit(nm, "_fon.nc")[[1]][1])
    modnames = c(modnames, 'ensemble')
    mapply(plotMap, dats, text3 = modnames,
           MoreArgs = list(limits = limits, cols = cols, addContour = TRUE))
    mtext(side = 4, 'Model output')
    plotLeg(cols = cols, limits = limits, maxLab = maxLab)
    
    lapply(zscores, plotMap, limits = zlimits, cols = zcols)
    mtext(side = 4, 'z score')
    plotLeg(cols = zcols, limits = zlimits)

    lapply(cors, plotMap, limits = dlimits1, cols = dcols)
    mtext(side = 4, 'bias')
    plotLeg(cols = dcols, limits = dlimits1)
                             
    lapply(cdat, plotMap, limits = limits, cols = cols, addContour = TRUE)
    mtext(side = 4, 'corrected output') 
    plotLeg(cols = cols, limits = limits)
   
    lapply(mapply('-', cdat, dats), plotMap, limits = dlimits2, cols = dcols)
    mtext(side = 4, 'difference')
    plotLeg(cols = dcols, limits = dlimits2)

    lapply(czscore, plotMap, limits = zlimits, cols = zcols)
    mtext(side = 4, 'corrected z score')
    plotLeg(cols = zcols, limits = zlimits)
    if (plot2File) dev.off()
    
    return(list(zscores, czscore, dats, cdat))
}   

##############################
## Connectivity score       ##
##############################
if (!exists('zscores')) {
    zscores = mapply(apply2Var,  varnames, names(varnames), levelss, aggFUNs,
                 limitss, contours, colss, dlimitss1, dlimitss2, dcolss,
                 transs, itranss, maxLab)
}

plot_biomes <- function(r, name, tpoints = TRUE) {  
    
    plot_map_standrd(r, biome_cols, seq(1.5, length.out = length(biome_cols) - 1))
    mtext.units(name, adj = 0.9, line = -2)   
    grid()
    if (tpoints) {
        points(site_dat[,1], site_dat[,2], pch = 19, cex = 1.3)
        points(site_dat[,1], site_dat[,2], col = 'white', pch = 19)
        points(site_dat[,1], site_dat[,2], col = biome_cols[site_dat[,3]+1],
               pch = 19, cex = 0.7)
    }
    return(unique(r))
}
modnames = c(sapply(sapply(mod_files, function(file) tail(strsplit(file, '/')[[1]], 1)),
                      function(nm) strsplit(nm, "_fon.nc")[[1]][1]), "Ensemble") 
if (T) {
png("figs/bias_corrected_biome.png", height = 11, width = 11,  units = 'in', res = pres)

lmat = t(matrix(1:10, nrow= 2))
layout(rbind(cbind(lmat, lmat + 10, lmat +20), 31), widths = c(1, 1.5, 1, 1.5, 1, 1.5))
par(mar = rep(0, 4), oma = c(0, 4, 2, 1))
plotBiomes <- function(modid, corid, tall.cex = 0.2, summPlot = FALSE, letter = c('', '')) {
    if (corid > 2) {
        rs = lapply(zscores[4,], function(i) i[[modid]])
        mask = !is.na(rs[['fpc']])
        transDat <- function(var, tFUN, ...) 
            tFUN(rs[[var]][mask], n = 100, ...)
        dat = mapply(transDat, names(varnames), transs)
        dat = apply(apply(dat, 2, scale), 2, function(i) (i - mean(i))/sd(i))
        if (corid == 3) nms = c("fpc", "height") else nms = names(varnames)
        
        #fit = cascadeKM(scale(dat[,5:6], center = TRUE, scale = TRUE), 1, 20, iter = 100)
        #nc = which.min(fit$results[2,])
        biome = rs[[1]]
        
        clusters = kmeans(dat[,c( 5:6)], 4, 200)#, algorithm = "Lloyd")
        
        order = sort.int(clusters$centers[, 'height'] + clusters$centers[, 'fpc'], 
                         decreasing = TRUE, index.return = TRUE)[[2]]
        
        biome[mask] = apply(sapply(order, function(i) clusters[[1]] == i), 1, which)
        
        plot_map_standrd(biome, c('#7570b3', '#1b9e77','#d95f02', 'grey'), 0.5 + (1:3))
        if (modid == 1 || summPlot == TRUE)   mtext.units("clustered", adj = 0.9, line = -2)
        mtext(side = 1, letter[1], line = -2.5, adj = 0.67)
        grid()
#indexFUN <- function(i) {
        #    sort.int(biomes$centers[,i], index.return = TRUE, 
        #                                 decreasing=TRUE)[[2]]
        #    browser()
        #}
        #biomes = biomes[indexFUN('fpc')]
        #browser()
        #cdat = cbind(dat, )
        #cdat = as.data.frame(cdat)
        #cdat[,7] = as.factor(cdat[,7])
        #names(cdat) = c(names(varnames), 'cluster')
        
        #fit = rpart(cluster~., data=as.data.frame(cdat[,5:7]))
        #predictions = predict(fit, cdat[,5:6], type="class")
        #browser()
        dat = rs
        
    } else {
        dat = lapply(zscores[2 + corid,], function(i) i[[modid]])
        biome = biome_assign_precalV(dat[["fpc"]], dat[["evergreen"]], dat[["tropical"]],
                            dat[["temperate"]], dat[["gdd"]], dat[["height"]])
        if (modid == 1 || summPlot == TRUE) 
            modName = c('uncorrected', 'corrected')[corid] else modName = ''
        if (corid==1) tpoints = TRUE else tpoints = FALSE
        plot_biomes(biome+1, modName, tpoints)    
        test = test0 = (biome ==1) | (biome == 8 & dat[["height"]]>5)  
        test = round(raster::aggregate(test, 2))>0
        mtext(side = 1, letter[1], line = -2.5, adj = 0.67)
        xyt = xyFromCell(test, which(test[]))
        points(xyt[,1], xyt[,2], cex = tall.cex, col = biome_cols[2], pch = 19)
    }
    
    if (!summPlot && corid == 1) mtext(side = 2, modnames[modid], line = 2, ad = 0.8)    
    if (corid==1) axis(2)# else axis(4)
    #if (modid==1) axis(3)
    if (modid==length(zscores[[1,1]])) axis(1)
    
    if (summPlot) par(mar = c(1, 0.0, 2, 0.5)) else par(mar = c(1, 3, 2, 0.5))

    x = logit(dat[['fpc']][], n = 100); y = log(dat[['height']][] + 0.001)
    
    plot(x, y, type = 'n', axes = FALSE, xlab = '', ylab = '',
        xlim = logit(c(0,1), n= 100), ylim = log(c(0, 60)+0.001))
    mtext(side = 3, letter[2], line = -1.5, adj = 0.1)
    addPoints <- function(fpcRange, heightRange = NULL, col) {
        
        if (length(fpcRange) == 2) {
            fpcRange = logit(fpcRange, n = 100); heightRange = log(heightRange + 0.001)
       
            selectRange <- function(i, r) (i > r[1]) & (i < r[2])
            test = selectRange(x, fpcRange) &  selectRange(y,  heightRange)
        } else {
            test = clusters[[1]] == fpcRange
            x = x[mask[]]; y = y[mask[]]
        }
        x0 = x; y0 = y
        x = x[test]; y = y[test]
        
        cols = make_col_vector(c("white", col), ncols = 10)[-1]
        cols = cols[unlist(mapply(rep, 3:9, 9 + (3:9)^2))]
        cols = densCols(x,y, colramp = colorRampPalette(cols), bandwidth = 0.1)
        points(y~x, pch = 20, col = cols, cex = 0.5)
        if (length(fpcRange) == 2) {
            slines <- function(...) lines(..., lty = 2, col = make.transparent('black', 0.67))
            sapply(fpcRange, function(i) slines(c(i, i), log(c(0, 100) + 0.001)))
            sapply(heightRange, function(i) slines(logit(c(0.6, 1), n = 100),c(i, i)))
        }
    }
    if (corid == 3) {
        mapply(addPoints, order, col = c('#7570b3', '#1b9e77','#d95f02', 'grey'))
    } else {
        addPoints(c(0.6, 1), c(10, 100), '#3A3851')
        addPoints(c(0.6, 1), c(0, 10), '#0A4e3B')
        addPoints(c(0.3, 0.6), c(0, 100), '#752F01')
        addPoints(c(0.0, 0.3), c(0, 100), 'grey')
    }
    labs = c(0, 0.01, 0.05, 0.1, 0.2, 0.5, 0.8, 0.9, 0.95, 0.99, 1)
    axis(1, at = logit(labs, n = 100), labels = labs)
    
    if (summPlot) {
        if (corid == 2)  mtext(side = 1, line = 2, 'Fractional cover')
        if (corid == 1)  mtext(side = 2, line = 2, 'Height (m)')  
    } else {
        if ((modid) == length(zscores[[1,1]])) 
            mtext(side = 1, line = 2, 'Fractional cover') 
        if (modid == 3)  mtext(side = 2, line = 2, 'Height (m)') 
    }
    labs = c(0, 0.1, 1, 2, 5, 10, 20, 50)
    if (!summPlot || (summPlot && corid == 1)) axis(2, at = log(labs + 0.001), labels = labs)
    if (summPlot && corid == 3) axis(4, at = log(labs + 0.001), labels = labs)
    par(mar = rep(0, 4))
    #browser()
    #hist(dat[['height']][dat[['height']]>1], 100, axes = F, 
    #     xlab = '', ylab = '', main = '', xlim = c(0, 60)) 
    #axis(1)
    #
    return(biome)
}
biomes = lapply(1:3, function(cid) lapply(1:length(zscores[[1,1]]), plotBiomes, cid))
plot.new()

legend('center', col = biome_cols, legend = names(biome_cols), pch = 15, ncol = 6, pt.cex = 3,
       bty = 'n')
dev.off()
}


writeBiome <- function(biome,model, name1) {
    file = paste0("../outputs/", model, "/",name1)
    makeDir(file)
    file = paste0(file, "/biome.nc")
    print(file)
    writeRaster(biome, file, overwrite = TRUE)
}

writeExperiment <- function(biomesi, ...)
    mapply(writeBiome, biomesi, modnames, MoreArgs = list(...))
   
biomes = mapply(writeExperiment, biomes, c('uncorrected', 'corrected', 'clusted'))


outputDat <- function(modid, corid, returnVar = "height") {
    dat = lapply(zscores[2 + corid,], function(i) i[[modid]])
    
    dir = paste0("../outputs/", modnames[modid])
    makeDir(dir)
    dir = paste0(dir, '/', c('uncorrected', 'corrected')[corid])
    makeDir(dir)             
    
    outputVar <- function(varname) {
        file = paste0(dir, '/', varname, ".nc")
        writeRaster(dat[[varname]], file = file, overwrite = TRUE)
    }
    lapply(c("fpc", "evergreen", "tropical", "temperate", "gdd", "height"), outputVar)
    return(dat[[returnVar]])   
    
}
heights = lapply(1:2, function(cid) lapply(1:length(zscores[[1,1]]), outputDat, cid))
fpcs = lapply(1:2, function(cid) lapply(1:length(zscores[[1,1]]), outputDat, cid, "fpc"))

save(biomes, heights, fpcs, file = 'outputs/biomes.Rd')
print("yay")
browser()

png("figs/bias_corrected_biome-ens.png", height = 7, width = 8,  units = 'in', res = pres)

layout(cbind(c(1, 1, 1, 2), c(3, 3, 3, 4), c(3, 3, 5, 4), c(6, 6, 6, 7), c(8, 6, 9, 7)), 
    widths = c(1, 0.6, 0.4, 0.6, 0.4), height = c(0.35, 0.15, 0.35, 0.5))
par(mar = rep(0, 4), oma = c(3, 3.5, 0.5, 2.5))
lapply(1:2, function(cid) plotBiomes(length(zscores[[1,1]]), cid, tall.cex = 0.5, 
        summPlot = TRUE, letter = list(c('a', 'd'), c('b', 'e'))[[cid]]))
plot.new()

legend('center', col = biome_cols, legend = names(biome_cols), pch = 15, ncol = 2, pt.cex = 3,
       bty = 'n')
plotBiomes(length(zscores[[1,1]]), 3, summPlot = TRUE, letter = letters[c(3, 6)])

plot.new()
# legend('center', col = c('#7570b3', '#1b9e77'), legend = c('Dense Forest', 'Sparse Forest'), pch = 15, pt.cex = 3,  bty = 'n')
plot.new()
 legend('center', col = c('#7570b3', '#1b9e77', '#d95f02', 'grey'), 
        legend = c('Dense Forest', 'Sparse Forest', 'Savanna/grass', 'Desert'), pch = 15, pt.cex = 3,  bty = 'n')
dev.off()
browser()

start = c(-70, 0)

j0 = colFromX(zscores[[1,1]][[1]], start[1])
i0 = rowFromY(zscores[[1,1]][[1]], start[2])

iijj = lapply(c(-1, 0, 1), function(jj) sapply(c(-1, 0, 1), function(ii,jj) c(ii,jj), jj)) 
iijj = do.call(cbind, iijj)

findM <- function(i, j, mask, conn, rin, w, test0 = FALSE)  {
        
        newRanges <- function(ij) {
            newRange <- function(coni, ri) {
                c(min(coni[[1]][ij[1],ij[2]], ri[i, j]),
                max(coni[[2]][ij[1],ij[2]], ri[i, j])) 
            }
            list(mapply(newRange, conn[[3]], rin))
        }
        whichMinD  <- function(v) {
            v = lapply(v, function(i) i[[1]])
            test  =sapply(v, function(i) !any(is.na(i)))            
            v = v[test]              

            if (length(v) == 0) {
                blankOut = rep(NaN, length(conn[[3]]) )
                return(list(NaN, rbind(blankOut, blankOut)))
            }
            
            mean.wtd <- function(vi) sum(w*(vi[2,]-vi[1,]))
            d = sapply(v, mean.wtd) 
            
            if (test0) browser()
            #if (test0 && !is.na(conn[[3]][i,j]) && conn[[3]][i,j]<=min(d)) return(NULL)    
            index = which.min(d)            
            list(d[index], v[[index]])
        }
        iijj = iijj + c(i,j)
        test = iijj[1,] <nrow(mask) & iijj[2,] <ncol(mask)
        iijj = iijj[,test]        
        iijj = t(iijj[,!(apply(iijj, 2, function(i) mask[i[1], i[2]]))])
        
        out =  whichMinD(apply(iijj, 1, newRanges))
        if (is.null(out)) return(conn) 
        conn[[1]][i,j] = out[[1]]     
        for  (lr in 1:length(conn[[3]]))
            for (mm in 1:2) conn[[3]][[lr]][[mm]][i,j] = out[[2]][mm,lr]  
        conn
} 
initaliseRW <- function(rin, w, i0, j0, temp_name) {
    print(temp_name)
    temp_file = paste0("temp/initaliseRW-", i0, '-', j0, '-', temp_name)
    temp_comp = paste0(temp_file, "complete.Rd")
    if (file.exists(temp_comp)) load(temp_comp)  else {
    
    conn0 = as.matrix(rin[[1]])
    conn0[,] = NaN
    connp = list(conn0, conn0)
    
    conn = rep(list(connp), length(rin))
    for (i in 1:length(rin)) for (j in 1:2) conn[[i]][[j]][i0,j0] =  rin[[i]][i0,j0]   
    conn = list(conn0, conn0, conn) 
    conn[[2]][,] = 0.0 
    mask = any(layer.apply(rin, is.na))
   
    nr = nrow(rin[[1]]); nc = ncol(rin[[1]])
    maxDist =  max(i0, j0, nr-i0, nc-j0)
    
    nxtp = 10
    stpp = 10
    temp_stepMake <- function(nxt) paste0(temp_file, '-', nxt, '.Rd')
    temp_step = temp_stepMake(nxtp)
     
    for (d in 1:maxDist) {
        if (d > nxtp) {
            cat("save point to:", temp_step, "\n")
            if (!file.exists(temp_step)) save(conn, file = temp_step)   
            nxtp = nxtp + stpp  
            temp_step = temp_stepMake(nxtp)         
        }
        
        if (file.exists(temp_step)) {
            load(temp_step)
            next
        } 
        print(d/maxDist)
        
        indexicate <- function(k) 
            rbind(c(i0-d, j0-k), c(i0+d, j0-k), c(i0-d, j0+k), c(i0+d, j0+k),
                  c(i0-k, j0-d), c(i0+k, j0-d), c(i0-k, j0+d), c(i0+k, j0+d))
                                                                           
        index = lapply(0:d,  indexicate)
        index = unique(do.call(rbind, index))
        index = index[apply(index>0, 1, all) & index[,1] < nr & index[,2] < nc,]
    
        order = sample(1:nrow(index), nrow(index), replace = FALSE)
        for (s in order) {
            ss = s             
            if (mask[index[s, 1], index[s, 2]]) next
            conn = findM(index[s, 1], index[s, 2], mask, conn, rin, w)#, conn[[1]], max)  
            conn[[2]][index[s, 1], index[s, 2]] = conn[[2]][index[s, 1], index[s, 2]]+1    
        }        
    }
    save(conn, file = temp_comp)
    }    
    return(conn) 
}

translanteRW2raster <- function(conn, rin) {
    rout0 = rin[[1]]
    routp = addLayer(rout0, rout0)
    routp = rep(c(routp), length(conn[[3]]))
    #names(routp) = names(rin)
    for (v in 1:length(routp)) for (mm in 1:2) routp[[v]][[mm]][] = conn[[3]][[v]][[mm]]
    rout = list(rout0, rout0, routp)
    for (v in 1:2) rout[[v]][] = conn[[v]]  
    return(rout)
}
#


gt <- function(a, b) a > b    
lt <- function(a, b) a < b  
findMaps <- function(is1, is2, js1, js2, conn, rin, w) {
    
    compCompare <- function(cn, ri) {
        ri =  as.matrix(ri)[is1, js1]       
        compMM <- function(ci, FUN1) {
            co = ci
            test = which(FUN1(ci[is2, js2], ci[is1, js1]))
            co[is1, js1][test] = ci[is2, js2][test]
            test = which(FUN1(co[is1, js1],ri))
            co[is1, js1][test] = ri[test]
            return(co)
        }
        cn = list(compMM(cn[[1]], gt), compMM(cn[[2]], lt)) 
    }
    connU = mapply(compCompare, conn[[3]], rin, SIMPLIFY = FALSE)
    d = mapply(function(ci, w) w*abs(ci[[2]]-ci[[1]]), connU, w, SIMPLIFY = FALSE)
    
    if (length(d) > 1)         
        for (di in d[-1]) d[[1]] = d[[1]] + di
    
    d = d[[1]]
    test = which(d < conn[[1]])
    
    conn[[1]][test] = d[test]
    conn[[2]][test] = conn[[2]][test] +1
    for (lr in 1:length(conn[[3]])) for (mm in 1:2)
        conn[[3]][[lr]][[mm]][test] = connU[[lr]][[mm]][test]
    return(conn)
 }
shinnyRA <- function(rin, zscore, w) {

    if (is.raster(rin)) browser()  else conn = rin
    nr = nrow(conn[[1]]); nc = ncol(conn[[1]])
    cis = list(2:nr,1:(nr-1))
    cjs = list(2:nc, 1:(nc-1))
    for (shim in 1:1000) {
        print(shim)
        it1 = sample(1:2, 1); jt1 = sample(1:2, 1) 
        it2 = sample(1:2, 1); jt2 = sample(1:2, 1)
        
        conn = findMaps(cis[[it1]], cis[[it2]], cjs[[jt1]], cjs[[jt2]], conn, zscore, w) 
    }
    return(conn)
}


ruinModCor <- function(modID = 5,   correctID = 2) {
    print("new RW simulator!")
    print(modID)
    print(correctID)
    zscore = zscore0 =  lapply(zscores[correctID,], function(i) i[[modID]])
    allZs = do.call(cbind, lapply(zscore, function(i) i[]))
    mask = !apply(allZs, 1, function(i) any(is.na(i)))
    allZsm = allZs[!apply(allZs, 1, function(i) any(is.na(i))),]
    pca = prcomp(allZsm)
    
    ws =  pca$sdev^2/sum(pca$sdev^2)
    allZs[mask,] = predict(pca)
    
    for (i in 1:length(zscore)) zscore[[i]][] = allZs[,i]   
    
    conn = initaliseRW(zscore, ws, i0, j0,
                       paste("zscore_pca_from_correctedTr", length(zscore), modID, correctID, sep = '-'))
     
    conn = shinnyRA(conn, zscore, ws)
    #conn_scores = translanteRW2raster(conn, zscore) 
    
    translanteRW2raster(conn, zscore)[[1]] 
}
#connS = lapply(1:2, function(cID) lapply(1:5, ruinModCor, cID))

conn_cols = c('#ffe5ff','#ffbcf7','#fee391','#fec44f','#fe9929','#ec7014','#cc4c02','#993404','#662506')
conn_cols = rev(c('#ffffd9','#edf8b1','#c7e9b4','#7fcdbb','#41b6c4','#1d91c0','#225ea8','#0c2c84'))
conn_limits = c(1, 2, 5, 10, 15, 20, 30)
plotConn <- function(modid, conid, cols = conn_cols, limits = conn_limits) {
    if (conid == 3) con = dif[[modid]] else con = connS[[conid]][[modid]]
    plot_map_standrd(con, cols, limits, FALSE) 
    if (conid<3)
        contour(biomes[[conid]][[modid]]==1, add = T, levels = c(0.5), lwd = 1.5,
                drawlabels = FALSE)
    points(start[1], start[2], pch = 4, lwd = 2.5, cex = 2)
    grid()
    if (conid == 1) {
        mtext(line = -2, adj = 0.8, side = 2, modnames[modid])
        axis(2)
    }
    if (conid == 3) axis(4)
    if (modid == 1) {
        mtext(line = -2, adj = 0.8, side = 3,
              c('uncorrected', 'corrected', 'difference')[conid])
        axis(3)
    }
    if (modid == 5) axis(1)
}
png("figs/connectivity_score.png", res = pres, units = 'in', height = 12.8, width = 7)
nmods = length(zscores[[1,1]])
layout(rbind(cbind(1:nmods, nmods + 1:nmods, 2*nmods + 1:nmods), 3*nmods + c(1, 1, 2)),
       heights = c(rep(1, nmods), 0.3)) 
par(mar = rep(0, 4), oma = rep(2, 4))

lapply(1:2, function(cons) lapply(1:5,  plotConn, cons))

dif =  mapply('-', connS[[2]], connS[[1]])
dconn_cols = rev(c('#8c510a','#bf812d','#dfc27d','#f6e8c3','#f5f5f5','#c7eae5','#80cdc1','#35978f','#01665e'))
dconn_limits = c(-10, -5, -2, -1, -0.5, 0.5, 1, 2, 5, 10)
lapply(1:5, plotConn, 3, dconn_cols, dconn_limits)

par(mar = c(0, 0, 2, 0))
add_raster_legend2(dat = connS[[1]][[1]], cols = conn_cols, limits = conn_limits,
                   transpose = F, plot_loc = c(0.1, 0.9, 0.4, 0.6), srt = 0, 
                   extend_max = TRUE, add = FALSE)
add_raster_legend2(dat = dif[[1]], cols = dconn_cols, limits = dconn_limits, 
                   transpose = F, plot_loc = c(0.1, 0.9, 0.4, 0.6), srt = 0, 
                   extend_max = TRUE, extend_min = TRUE, add = FALSE, oneSideLabels = FALSE)
dev.off()  
browser() 

targetRW <- function(rin, zscore, ninter) {
    conn = layer.apply(rin, function(i) as.matrix(i))
    nr = nrow(rin); nc = ncol(rin)
    maxDist = max(c(sqrt(i0^2 + j0^2), sqrt((i0-nr)^2 + j0^2),
                  sqrt((i0-nr)^2 + (j0-nc)^2), sqrt(i0^2 + (j0 - nc)^2)))
    for (i in 1:ninter) {
        d = runif(1, 0, maxDist)
        print(i)
        FUN <- function(j) {
            i = round(sqrt(d^2 - j^2))
            rbind(c(i0-i, j0-j), c(i0-i, j0+j), c(i0+i, j0-j), c(i0+i, j0+j),
                  c(i0-j, j0-i), c(i0-j, j0+i), c(i0+j, j0-i), c(i0+j, j0+i))
            
        }
        index = lapply(0:d, FUN)
        index = unique(do.call(rbind, index))
        if (length(dim(index)) == 1) {
            i = index[1]; j = index[2]
        }   else {
            index = index[apply(index>0, 1, all) & index[,1] < nr & index[,2] < nc,]
            dists = apply(index, 1, function(i) conn[[3]][i[1], i[2]]     )
            if (all(is.na(dists))) next
            c(i, j) := index[which.min(dists),]
        }
        for (step in 1:10000) {
            print(step)
            txy = sample(c(0, 1), 1)
            if (txy == 0) i = i + sample(c(-1, 1), 1)
               else j = j + sample(c(-1, 1), 1)
            
            if (i > nr | j > nc | is.na(zscore[i,j])) break  
            conn = findM(i, j, mask, conn, zscore, TRUE)
            
            conn[[4]][i,j] = conn[[4]][i,j]+1
        }    
         print(step)
    }
    rout = rin
    for (i in 1:4) rout[[i]][] = conn[[i]]    
    browser()
}
#conn1x = targetRW(conn1, zscore, 1000)
browser()
par(mfrow = c(2, 2), mar = rep(0, 4))
plot_map_standrd(zscore1, zcols, zlimits, FALSE)
add_raster_legend2(zcols, zlimits, dat = zscore,
                   plot_loc = c(0.5, 0.97, 0.93, 0.95),
                   add = TRUE, transpose = F, extend_max = T, extend_min = T, srt = 0)  
plot_map_standrd(zscore2, zcols, zlimits, FALSE)
plot_map_standrd(conn1[[3]], conn_cols, conn_limits, FALSE)
points(start[1], start[2], pch = 4, cex = 3, lwd = 4) 
add_raster_legend2(conn_cols, conn_limits, dat = conn0[[3]],
                   plot_loc = c(0.5, 0.97, 0.93, 0.95),
                   add = TRUE, transpose = F, extend_max = T, srt = 0)    
plot_map_standrd(conn2[[3]], conn_cols, conn_limits, FALSE) 
points(start[1], start[2], pch = 4, cex = 3, lwd = 4)  


#for (chain in 1:1000) {
#    print(chain)
#    i = i0; j = j0
#    for (step in 1:50000) {
#        txy = sample(c(0, 1), 1)
#        if (txy == 0) i = i + sample(c(-1, 1), 1)
#        else j = j + sample(c(-1, 1), 1)
#        
#        if (is.na(zscore[i,j])) break
#        conn[[4]][i,j] = conn[[4]][i,j]+1
#    }
#}
