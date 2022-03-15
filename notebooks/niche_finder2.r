source('../../gitProjectExtras/gitBasedProjects/R/sourceAllLibs.r')
sourceAllLibs('../../gitProjectExtras/gitBasedProjects/R/', trace = FALSE)
sourceAllLibs('../../rasterextrafuns/rasterPlotFunctions/R/', trace = FALSE)
sourceAllLibs('../../rasterextrafuns/rasterExtras/R/', trace = FALSE)

library(raster)
#library(rasterExtras)
library(fields)
sourceAllLibs("../libs/", trace = FALSE)
graphics.off()

vars = c("fpc", "height", "gdd")
trans = list(function(i) logit(i, exp(100)), 
             function(i) logit(i/120, exp(100)), 
             function(i) log(i/8000))
centreRange = list(c(0.5, 1), c(10, 120), c(500, 9E9))
updown = c(T, T, T)

dir = '../outputs/'
                
speciesDist = "../outputs/bcObs-den/all.Rd"

models = list.files(dir)
models = models[!grepl("bcObs-den", models)]
experiments = list.files(paste0(dir, models[1], '/'))

nboots = 100

Fmapply <- function(FUNS, rs, SIMPLIFY = FALSE, ...)
        mapply(function(i, j) i(j, ...), FUNS, rs, SIMPLIFY = SIMPLIFY)


dRaster <- function(r) {
    v = as.matrix(r)
    v[2:nrow(v),] = v[2:nrow(v),] - v[1:(nrow(v)-1),]
    v[,2:ncol(v)] = v[,2:ncol(v)] - v[,1:(ncol(v)-1)]
    r[] = v     
    r
}

dMaxima <- function(r) {
    max9 = raster::focal(x = r, fun = function(i) max(i, na.rm = TRUE), 
                         w = matrix(1, ncol = 3, nrow = 3))
    r[r == max9]
}

#####
centreRangeT = Fmapply(trans, centreRange)
centreRangeT = lapply(centreRangeT, logistic)
forModelExperiemnt <- function(model, experiment) {
    dats = suppressWarnings(lapply(vars, function(i) 
                raster(paste0(dir, '/', model, '/', experiment, '/', i, '.nc'))))
    varMask <- function(r, i) 
        r > i[1] & r < i[2]
    
    mask = mapply(varMask, dats, centreRange)
    mask = sum(do.call(addLayer, mask))!=length(dats)
    
    datsT = Fmapply(trans, dats, TRUE)
    
    defineRange <- function() {
        forVar <- function(i, dat) {

            selecCombo <- function(a, b) { 
                x = runif(nboots * 100, a, b)
                ps = logit(x, exp(100))^2
                logit(sample(x, nboots, TRUE, ps), exp(100))
            }
            
            dat[mask] = NaN
            dat[dat < i[1]] = NaN; dat[dat > i[2]] = NaN
            ddat = dMaxima(dat)
            
            x0 = selecCombo(min(ddat), max(ddat))
            browser()
            r = abs(selecCombo(0, 1))
            
            return(cbind(x0, r))       

            x0 = logit(runif(nboots, i[1], i[2]), exp(100))
            r = abs(logit(runif(nboots, 0, 1), exp(100)))
            return(cbind(x0, r))
        }
        rngs = mapply(forVar, centreRangeT, dats, SIMPLIFY = FALSE)
        rngs = lapply(1:nrow(rngs[[1]]), function(i) lapply(rngs, function(r) r[i,]))
        inSphere <- function(rng) {
            normaliseVar <- function(x, x0r, cr) {
                out = (x - x0r[1])/x0r[2]
                if (is.na(cr)) return(out)
                if (cr) out[out>0] = 0
                else out[out<0] = 0
                return(out)
            }
            nDat = mapply(normaliseVar, dats, rng, updown)
            Sphere = sqrt(sum(layer.apply(nDat, function(i) i)^2))<1
            Sphere[Sphere == 0] = NaN
            pol = rasterToPolygons(Sphere, digits = 2, dissolve = TRUE)
            browser()
        }
        lapply(rngs, inSphere)
        browser()
    }
    defineRange()
    browser()
}

lapply(experiments, function(exp) lapply(models, forModelExperiemnt, exp))
    

