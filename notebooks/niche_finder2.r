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

obs = list(c("../../savanna_fire_feedback_test/data/driving_Data/TreeCover.nc",
             "../../savanna_fire_feedback_test/data/driving_Data/nonTreeCover.nc"),
             "../data/height_Simard.nc",
             "../../savanna_fire_feedback_test/data/driving_Data_global_new/GDD0.nc")

trans = list(function(i) logit(i), 
             function(i) logit(i/120), 
             function(i) log(i/8000))
centreRange = list(c(0.5, 1), c(10, 120), c(500, 9E9))
updown = c(T, T, T)

dir = '../outputs/'
                
speciesDist = "../outputs/bcObs-den/all.Rd"

models = list.files(dir)
models = models[!grepl("bcObs-den", models)]
experiments = list.files(paste0(dir, models[1], '/'))

nboots = 1000

Fmapply <- function(FUNS, rs, SIMPLIFY = FALSE, ...)
        mapply(function(i, j) i(j, ...), FUNS, rs, SIMPLIFY = SIMPLIFY)

ndimMat = 20
extent = c(-120, -30, -30, 25)

Mat0 = array(0, dim = rep(ndimMat, 3))
sideBs = logit(seq(0, 1, length.out = ndimMat))

load(speciesDist)

obs = lapply(obs, function(i) layer.apply(i, brick))
v1 = obs[[1]][[1]]; v2 = obs[[1]][[2]]
obs[[1]] = v1 + v2 + ((1-v1-v2)/(1-v1))*v1*((1/0.8) -1)
obs[[1]][obs[[1]]>1] = 1

obs = lapply( obs, sum)
obs = Fmapply(trans, obs, TRUE)
obs[[1]][obs[[1]] < -50] = NaN

sideBs = cbind(rep(sideBs*2 + 6, each = ndimMat^2), rep(sideBs*3, each = ndimMat), sideBs)
#####
centreRangeT = Fmapply(trans, centreRange)

forModelExperiemnt <- function(model, experiment, tID = '') {
    loadDat <- function(i)
        raster(paste0(dir, '/', model, '/', experiment, '/', i, '.nc'))
    dats = suppressWarnings(lapply(vars, loadDat))
    dats[[2]][dats[[2]] > 40] = 40
    dats = Fmapply(trans, dats, TRUE)
    dats = lapply(dats, crop, extent)
    obs = lapply(obs, raster::resample, dats[[1]])
    
    varMask <- function(r, i) 
        r > i[1] & r < i[2]
    
    mask = mapply(varMask, dats, centreRangeT)
    mask = sum(do.call(addLayer, mask))==length(dats)
    
    defineRange <- function() {
        forVar <- function(i, dat) {
            selecCombo <- function(x)#, prob = rep(1, x)) {
                #browser()
                logit(sample(x, nboots, TRUE))#, prob)) #ps = logit(x)^2
            #}
            mask = mask & !is.na(dats[[1]])
            x = dat[mask]            
            x = x[x>i[1] & x <i[2]]
            
            x0 = selecCombo(logistic(x))
            r = abs(selecCombo(runif(nboots * 100, 0, 1)))
            return(cbind(x0, r))
        }

        rngs = mapply(forVar, centreRangeT, dats, SIMPLIFY = FALSE)
        rngs = lapply(1:nrow(rngs[[1]]), function(i) lapply(rngs, function(r) r[i,]))

        inSphere <- function(rng, n, dats, dID) {  
            tfile = paste('../temp/nich_finder2', tID, dID, model, experiment, n, ndimMat, 
                           '.Rd', sep = '-')   
            print(tfile)   
            if (file.exists(tfile)) { 
                load(tfile); 
                return(list(rng, Barea, nfrags, fragIndex, polSizes, matSphere))
            }           
            normaliseVar <- function(x, x0r, cr) {
                out = (x - x0r[1])/x0r[2]
                if (is.na(cr)) return(out)
                if (cr) out[out>0] = 0
                else out[out<0] = 0
                return(out)
            }
            
            nDat = mapply(normaliseVar, dats, rng, updown)
            Sphere = Sphere_out = sqrt(sum(layer.apply(nDat, function(i) i)^2))<1
            
            Sphere[Sphere == 0] = NaN
    
            tfile_nc = paste('../temp/nich_finder2', tID, model, experiment, n, ndimMat, 
                             'Sphere.nc', sep = '-') 

            Sphere = writeRaster(Sphere, file = tfile_nc, overwrite = TRUE)
            Barea = sum(area(Sphere, na.rm = TRUE)[], na.rm = TRUE)
            if (Barea == 0) {
                nfrags = 0
                fragIndex = NaN
                polSizes = c()
                matSphere = array(0, dim = rep(ndimMat, 3))
            } else {
                pol = rasterToPolygons(Sphere, digits = 2, dissolve = TRUE)
                
                ps = lapply(pol@polygons , slot , "Polygons")[[1]]
                nfrags = length(ps)
                polArea = sapply(ps, function(x) slot(x, "area"))
                polSizes = sort(polArea, decreasing=TRUE)/max(polArea)
                fragIndex = sum(polSizes)
            
                mdis = mapply(normaliseVar, matrix2list(sideBs), rng, updown)
                matSphere = apply(mdis, 1, function(i) sqrt(sum(i^2))) < 1
                matSphere = array(matSphere, dim = rep(ndimMat, 3))
            }
            save(rng, Barea, nfrags, fragIndex, matSphere, polSizes, file = tfile)
            return(list(rng, Barea, nfrags, fragIndex, polSizes, matSphere))
        }
        inSpheres <- function(...) 
            return(list(Mod = inSphere(..., dats = obs, dID = 'mod'),
                        LGM = inSphere(..., dats = dats, dID = 'LGM')))
        
        out = mapply(inSpheres, rngs, 1:length(rngs))
        
        #browser()
    }
    defineRange()
    #browser()
}

lapply(experiments, function(exp) lapply(models, forModelExperiemnt, exp, 'Forest'))
    

