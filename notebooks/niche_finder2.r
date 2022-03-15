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



#####
centreRangeT = Fmapply(trans, centreRange)
centreRangeT = lapply(centreRangeT, logistic)
forModelExperiemnt <- function(model, experiment) {
    dats = suppressWarnings(lapply(vars, function(i) raster(paste0(dir, '/', model, '/', experiment, '/', i, '.nc'))))

    dats = Fmapply(trans, dats, TRUE)
    #dats = lapply(dats, logistic)

    defineRange <- function() {
        forVar <- function(i, dat) {

            #x0s = rs = c()
            selecCombo <- function(x) {
                
                ps = logit(x, exp(100))^2
                logit(sample(x, nboots, TRUE, ps), exp(100))

                #x = sort(x)
                #xs = c()
                #for (i in 1:nboots) {
                #    xi = sample(x, 1,  prob = ps)
                #    ps = sqrt((xi - x)^2) * ps
                #    ps = ps/mean(ps)
                #    xs = c(xs, xi)
                #}
            }
            browser()
            x = logistic(dat[!is.na(dat)])
            x = x[x>i[1] & x <i[2]]
            
            x0 = selecCombo(x)
            r = abs(selecCombo(runif(nboots * 100, 0, 1)))
            
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
    

