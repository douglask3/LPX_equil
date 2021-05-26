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
tas_dir =  'data/Figures_doug/Figure 2_6/'
pattern = '_fon'

mod_files = list.files(mod_dir, pattern = pattern, full.name = TRUE)
tas_files = list.files(tas_dir, full.names = TRUE) 
varnames = list(gdd = "gdd", height = c("height", "fpc_grid"), fpc = "fpc_grid")

blankFun <- function(i) i
levelss = list(NaN, 1:7, 1:9)
aggFUNs = list(blankFun, mean, sum)

logN <- function(x, n= length(dats[[1]])) log(x+1/n)
transs  = list(logN, logN, logit)
itranss = list(exp, exp, logistic)

limitss = list(c(0, 100, 200, 300, 350, 400, 450),
               c(0, 0.1, 0.2, 0.5, 1, 2, 4, 6, 8),
               c(0, 0.1, 0.2, 0.4, 0.6, 0.8, 0.9))
colss = list(rev(c('#d73027','#f46d43','#fdae61','#fee090','#ffffbf',
                   '#e0f3f8','#abd9e9','#74add1','#4575b4')),
             c('#fff7fb','#ece2f0','#d0d1e6','#a6bddb','#67a9cf',
               '#3690c0','#02818a','#016c59','#014636'),
             c('#ffffe5','#f7fcb9','#d9f0a3','#addd8e','#78c679',
               '#41ab5d','#238443','#006837','#004529'))

dlimitss1 = list(seq(-0.5, 0.5, 0.5),
                 c(-1.2, -1, -0.8, -0.6, -0.4, -0.2, 0, 0.2),
                 seq(-4, 4))
dlimitss2 = list(c(-140, -120, -100, -80, 60, -40, -20, 0, 20),
                 c(-6, -4, -2, -1, 1, 2, 4, 6),
                 c(-0.5, -0.4, -0.3, -0.2, -0.1, 0.1, 0.2, 0.3, 0.4, 0.5))
dcolss = list(
(c('#40004b','#762a83','#9970ab','#c2a5cf',
                    '#e7d4e8','#f7f7f7','#d9f0d3','#a6dba0','#5aae61','#1b7837','#00441b')),
              rev(c('#8c510a','#bf812d','#dfc27d','#f6e8c3','#f5f5f5',
                    '#c7eae5','#80cdc1','#35978f','#01665e')))

zlimits =  c(-4, -3, -2, -1, -0.1, 0.1, 1, 2, 3, 4)
zcols = rev(c('#7f3b08','#b35806','#e08214','#fdb863','#fee0b6','#f7f7f7','#d8daeb','#b2abd2','#8073ac','#542788','#2d004b'))


conn_cols = c('#ffffe5','#f7fcb9','#d9f0a3','#addd8e',
              '#78c679','#41ab5d','#238443','#006837','#004529')
conn_limits = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)  

#################
## open things ##
#################

## site data
site_dat = read.csv(site_file, stringsAsFactors=FALSE)
site_dat = as.matrix(site_dat[c("LONGITUDE", "LATITUDE", "POLLEN_TO_LPX_BIOME_NUMBER")])
site_dat = site_dat[!apply(site_dat, 1, function(i) any(is.na(i))), ]
site_dat = cbind(site_dat, t(sapply(site_dat[,3], variable_from_biome)))

apply2Var <- function(varname, name, levels, aggFUN, limits, cols, dlimits1, dlimits2, dcols,
                      trans = function(x) x, itrans = trans) {
## Model data
    openDat <- function(file, cfile, varname) {
        
        if (length(varname) == 2) {
            dat1 = brick(file, varname = varname[1])  
            dat2 = brick(file, varname = varname[2])
            if (!is.null(levels)) {
                dat1 = dat1[[levels]] 
                dat2 = dat2[[levels]]
            }
            dat = dat1*dat2/sum(dat2)                     
        } else if (varname == 'gdd') {
            dat = 500*(raster(cfile) > 2)
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
            x = trans(x)
            
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
    cdat = mapply(function(i, j) itrans(trans(i) - j), dats, cors)
    
    zscoring <- function(dat) {
        dat = trans(dat)
        return(dat - mean(dat[], na.rm = TRUE))/sd(dat[], na.rm = TRUE)
    }
    zscores = lapply(dats, zscoring)
    czscore = lapply(cdat, zscoring)
    
    png(paste0("figs/", name, "_correction.png"), width = 7.2, height = 10, 
        res = 300, units = 'in')
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
zscores = mapply(apply2Var,  varnames, names(varnames), levelss, aggFUNs,
                 limitss, colss, dlimitss1, dlimitss2, dcolss,
                  transs, itranss)

zscore1 = zscores[[1]][[5]]
zscore2 = zscores[[2]][[5]] 
zscore1 = crop(zscore1, extent(c(-83, -33, -30, 15)))      
zscore2 = crop(zscore2, extent(c(-83, -33, -30, 15)))      
start = c(-70.25, 8.25)

j0 = colFromX(zscore, start[1])
i0 = rowFromY(zscore, start[2])

iijj = lapply(c(-1, 0, 1), function(jj) sapply(c(-1, 0, 1), function(ii,jj) c(ii,jj), jj)) 
iijj = do.call(cbind, iijj)

findM <- function(i, j, conn, rin, test0 = FALSE)  {
        newRange <- function(ij) {
            c(min(conn[[1]][ij[1],ij[2]], rin[i, j]),
              max(conn[[2]][ij[1],ij[2]], rin[i, j]))
        }
        whichMinD  <- function(v) {
            if (all(is.na(v))) return(c(NaN, NaN, NaN))
            d = v[2,] - v[1,]
            if (test0 && !is.na(conn[[3]][i,j]) && conn[[3]][i,j]<=min(d)) return(NULL)    
            index = which.min(d)
            c(v[,index], d[index])
        }
        iijj = iijj + c(i,j)
        test = iijj[1,] <nrow(conn[[3]]) & iijj[2,] <ncol(conn[[3]])
        iijj = iijj[,test]
        iijj = t(iijj[,!is.na(apply(iijj, 2, function(i) conn[[1]][i[1], i[2]]))])
       
        out =  whichMinD(apply(iijj, 1, newRange))
        if (is.null(out)) return(conn)      
        for  (lr in 1:3) conn[[lr]][i,j] = out[lr]  
        conn
} 
initaliseRW <- function(rin) {
    conn = as.matrix(rin)
    conn[,] = NaN
    conn = list(conn, conn, conn, conn)
    conn[[1]][i0,j0] =  rin[i0,j0]
    conn[[2]][i0,j0] =  rin[i0,j0] 
    conn[[4]][,] = 0.0 

    nr = nrow(rin); nc = ncol(rin)
    maxDist =  max(i0, j0, nr-i0, nc-j0)
    
    for (d in 1:maxDist) {
        print(d/maxDist)
        indexicate <- function(k) 
            rbind(c(i0-d, j0-k), c(i0+d, j0-k), c(i0-d, j0+k), c(i0+d, j0+k),
                  c(i0-k, j0-d), c(i0+k, j0-d), c(i0-k, j0+d), c(i0+k, j0+d))
    
        index = lapply(0:d,  indexicate)
        index = unique(do.call(rbind, index))
        index = index[apply(index>0, 1, all) & index[,1] < nr & index[,2] < nc,]
    
        order = sample(1:nrow(index), nrow(index), replace = FALSE)
        for (s in order) {
            if (is.na(rin[index[s, 1], index[s, 2]])) next
            conn = findM(index[s, 1], index[s, 2], conn, rin)#, conn[[1]], max)   
            #conn[[2]] = findM(index[s, 1], index[s, 2])#, conn[[2]], min)  
            conn[[4]][index[s, 1], index[s, 2]] = conn[[4]][index[s, 1], index[s, 2]]+1    
        }
        #if (d == 22) browser()
    }
    rout = addLayer(rin, rin, rin, rin)
    for (i in 1:4) rout[[i]][] = conn[[i]]
    return(rout)   
}
#conn1 = initaliseRW(zscore1)   
#conn2 = initaliseRW(zscore2)

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
            conn = findM(i, j, conn, zscore, TRUE)
            
            conn[[4]][i,j] = conn[[4]][i,j]+1
        }    
         print(step)
    }
    rout = rin
    for (i in 1:4) rout[[i]][] = conn[[i]]    
    browser()
}
conn1x = targetRW(conn1, zscore, 1000)
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
