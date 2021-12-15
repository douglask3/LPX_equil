source('../../gitProjectExtras/gitBasedProjects/R/sourceAllLibs.r')
sourceAllLibs('../../gitProjectExtras/gitBasedProjects/R/', trace = FALSE)
sourceAllLibs('../../rasterextrafuns/rasterPlotFunctions/R/', trace = FALSE)
sourceAllLibs('../../rasterextrafuns/rasterExtras/R/', trace = FALSE)

library(raster)
#library(rasterExtras)
library(fields)
sourceAllLibs("../libs/", trace = FALSE)
graphics.off()

matrix2list <- function(x, dim = 2) lapply(apply(x, dim, list), unlist) 

vars = c("fpc", "height", "gdd")

xlims = list(c(0, 1), c(0, 120), c(0, 600))

transs  = list(logit, function(x, ...) logit(x/120, ...), log)
itranss = list(logistic, function(x, ...) 120 * logistic(x, ...), exp)

ptranss = list(blankFun, function(x) 1-exp(-x*0.1), blankFun)
pitranss = list(blankFun, function(x) -10*log(1-x), blankFun)
ptranss = list(blankFun, function(x) x/120, function(x) 1-exp(-x*0.1))
pitranss = list(blankFun, function(x) x*120, function(x) -10*log(1-x))

ForestCentres = list(CNRM   = list(c(-70, -0), c(-53, -08), c(-60, -10), c(-67, -14)),
                     ENS    = list(c(-72, -1), c(-54, -09), c(-61, -09), c(-66, -14)),
                     FGOALs = list(c(-70, -0), c(-55, -08), c(-60, -10), c(-67, -14)),
                     HADGEM = list(c(-70, -0), c(-52, -06), c(-67, -10)),
                     MIROC  = list(c(-75, -2), c(-51, -05), c(-61, -08), c(-66, -13)))
                
dir = '../outputs/'




findBioclimPnts <- function(dat, ForestCentre) {
    findPnt <- function(pnt)  
        dat[cellFromXY(dat, pnt)]
    
    sapply(ForestCentre, findPnt)    
}




testWhereInSphere <- function(bcranges, dats, transs) {
    shift = sapply(bcranges, mean)
    scale = sapply(bcranges, diff)/2
    
    siftScar <- function(sh, sc, dat, trans)
        ((trans(dat)) - sh)/sc
    sdats = mapply(siftScar, shift, scale, dats, transs)
    
    if (!is.raster(dats[[1]])) return(sdats)
    out = sqrt(sum(layer.apply(sdats, function(i) i^2)))<1
    out[out == 0] = NaN
    
    return(out)                              
}                     



testNiche <- function(bcranges, ForestCentre, ...) {
    inSphere = testWhereInSphere(bcranges, ...)
    
    if (is.na(mean(inSphere[], na.rm = TRUE))) {
        print("broke")
        return(c(0))
    }
    pol = rasterToPolygons(inSphere, digits = 2, dissolve = TRUE)

     
    ps = lapply(pol@polygons , slot , "Polygons")[[1]]
    coords = lapply(ps, function(x) slot(x, "coords"))

    findPntInPol <- function(pnt){
        for (i in 1:length(coords)) {
            test = point.in.polygon(pnt[1], pnt[2], coords[[i]][,1], coords[[i]][,2])
            if (test) break
        }
        if (test) return(i) else return(NaN)
    }
    
    Cpols = sapply(ForestCentre, findPntInPol)
    if (all(is.na(Cpols))) out = 1 else if (any(is.na(Cpols)))  browser() else if (var(Cpols) > 0) out = 3 else out = 4
    return(c(out))
}  

#bcranges
#testNiche(bcranges, ForestCentre, dats, transs)

randomBCrange <- function(i, continent_range, inRange = TRUE,  id = NULL) {    
    if (is.null(id))
        mu = sample(continent_range[,i], 1)
    else
        mu = continent_range[id, i]
    
    sd = runif(1, 0, 1)
    bcrange = mu + c(-1, 1) * logit(sd)
    bcrange = sort(bcrange)
    
    bcrange
}
#id = sample(1:nrow(continent_range), 1)
selectPossibleNiche <- function(tcen) {
    dis = 2 
    id = sample(1:nrow(tcen), 1)
    p = sds = 0
    while( max(dis) > 1) {
        p = p + 1
        bcranges = lapply(1:3, randomBCrange, continent_range = tcen, id = id)
        dis = testWhereInSphere(bcranges, dats = matrix2list(tcen), list(blankFun))
        dis = apply(dis, 1, function(i) sqrt(sum(i^2)))                    
    }
    print(p)
    print(dis)
    
    return(bcranges)
}
#bcranges = selectPossibleNiche(tcen)
#bcranges



grab_cache = TRUE
nboots = 100
testRandomNiche <- function(ntest, model, experiment, ForestCentre, tcen) {
    tfile = paste0("../temp/randomNicheTest--gdd-", model, '-', experiment, '-', ntest, ".Rd")
    print(tfile)
    
    #print(file.exists(tfile))
    if (file.exists(tfile) && grab_cache) 
        load(tfile)
    else {        
        bcranges = selectPossibleNiche(tcen)#lapply(1:3, randomBCrange, id = id)
        
        test = testNiche(bcranges, ForestCentre, dats, transs)
        save(bcranges, test, file = tfile)
    }
    print(test)
    return(list(bcranges, test))
}
runNicheBoots <- function(...)
    out = lapply(1:nboots, testRandomNiche, ...)


run4model <- function(model, ForestCentre) {
    experiments = list.files(paste0(dir, model, '/'))
    run4Exp <- function(experiment) {
        print(model)
        print(experiment)
        dats = suppressWarnings(lapply(vars, function(i) raster(paste0(dir, '/', model, '/', experiment, '/', i, '.nc'))))
        dats <<- dats
        
        centres = lapply(dats, findBioclimPnts, ForestCentre)
        
        tcen = mapply(function(F,i) F(i), transs, centres)
        
        out = runNicheBoots(model, experiment, ForestCentre = ForestCentre, tcen = tcen)
        
    }
    
    outs = lapply(experiments, run4Exp)
}
models = list.files(dir)
model = models[1]
ForestCentre = ForestCentres[[1]]


experiments = list.files(paste0(dir, model, '/'))
experiment = experiments[1]
outss = mapply(run4model, models, ForestCentres)
