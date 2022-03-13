source('../../gitProjectExtras/gitBasedProjects/R/sourceAllLibs.r')
sourceAllLibs('../../gitProjectExtras/gitBasedProjects/R/', trace = FALSE)
sourceAllLibs('../../rasterextrafuns/rasterPlotFunctions/R/', trace = FALSE)
sourceAllLibs('../../rasterextrafuns/rasterExtras/R/', trace = FALSE)

library(raster)
#library(rasterExtras)
library(fields)
sourceAllLibs("../libs/", trace = FALSE)
graphics.off()


##########################################


vars = c("fpc", "height", "gdd")

xlims = list(c(0, 1), c(0, 120), c(0, 10000))
logit5 <- function(x) logit(x, exp(5))
logit20 <- function(x) logit(x, exp(20))
transs  = list(logit5, function(x, ...) logit20(x/120, ...), log)
itranss = list(logistic, function(x, ...) 120 * logistic(x, ...), exp)

logisticit <- function(x, a) logistic(a*logit(x))

logisticitN <- function(x, ...)
    (logisticit(x, ...) - logisticit(0, ...))/(logisticit(1, ...) - logisticit(0, ...))

ptranss = list(function(x) logisticitN(x, 0.1) , function(x) (x/120)^0.2, function(x) 1-exp(-x*0.0001))
pitranss = list(function(x) logisticitN(x, 10), function(x) (x^5)*120, function(x) -10000*log(1-x))


ForestCentres = list(CNRM   = list(c(-70, -0), c(-53, -08), c(-60, -10), c(-67, -14)),
                     ENS    = list(c(-72, -1), c(-54, -09), c(-61, -09), c(-66, -14)),
                     FGOALs = list(c(-70, -0), c(-55, -08), c(-60, -10), c(-67, -14)),
                     HADGEM = list(c(-70, -0), c(-52, -06), c(-67, -10)),
                     MIROC  = list(c(-75, -2), c(-51, -05), c(-61, -08), c(-66, -13)))
    
                
SavannaCentres = list(CNRM   = list(c(-55, 3), c(-58, -23)),
                      FGOALS = list(c(-60, 5), c(-57, -18)),
                      HADGEM = list(c(-55, 4), c(-45, -12)),
                      MIROC  = list(c(-55, 3), c(-42, -7)),
                      ENS    = list(c(-55, 3), c(-55, -18)))   
                
dir = '../outputs/'
                
speciesDist = "../outputs/bcObs-den/all.Rd"



############################################

findBioclimPnts <- function(dat, CentreS) {
    findPnt <- function(pnt)  
        dat[cellFromXY(dat, pnt)]
    
    sapply(CentreS, findPnt)    
}

################################################

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

######################################################

testNiche <- function(bcranges, CentreS, ...) {
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
    
    Cpols = sapply(CentreS, findPntInPol)
    
    if (all(is.na(Cpols))) out = 0 else if (any(is.na(Cpols)))  browser() 
        else out = length(unique(Cpols))
    return(c(out, inSphere))
} 

#################################################################

randomBCrange <- function(i, continent_range, inRange = TRUE,  id = NULL) {    
    if (is.null(id))
        mu = sample(continent_range[,i], 1)
    else
        mu = continent_range[id, i]
    
    mn = logistic(max(abs(continent_range[,i]-mu)))
    
    
    sd = runif(1, mn, 1)
    
    bcrange = mu + c(-1, 1) * logit(sd, exp(36))
    bcrange = sort(bcrange)
    
    bcrange
}


selectPossibleNiche <- function(tcen) {
    dis = 2 
    id = sample(1:nrow(tcen), 1)
    p = sds = 0
   
    while( max(dis) > 1) {
        p = p + 1
        bcranges = lapply(1:3, randomBCrange, continent_range = tcen, id = id)
        dis = testWhereInSphere(bcranges, dats = matrix2list(tcen), list(blankFun))
        dis = apply(dis, 1, function(i) sqrt(sum(i^2)))   
        #print(dis)
        if (p == 1000) browser()                 
    }
    print(p)
    print(dis)
    
    return(bcranges)
}

#############################################################

load(speciesDist)
lims = lims[-1,] - diff(lims)/2
tlims = cbind(rep(lims[,1], each = nrow(lims)^2), 
              rep(lims[,2], each = nrow(lims)), 
              lims[,3])

pMat = array(0, dim = dim(specDmat[[1]])) 

pMatFind <- function(info) {
    dis = testWhereInSphere(info, matrix2list(tlims), transs)
    pMat[] = apply(dis, 1, function(i) sqrt(sum(i^2)))<1
    return(pMat)
}

###################################################

grab_cache = T
nboots = 2000
nniches = 2000
nniche = 1000


testRandomNiche <- function(ntest, model, experiment, CentreS, tcen, extraName = '') {
    tfile = paste0("../temp/randomNicheTest--gdd-2-inSphere-", model, '-', experiment, '-', 
                   ntest, extraName, ".Rd")
    nfile = paste0("../temp/randomNicheTest--gdd-2-inSphere-", model, '-', experiment, '-', 
                   ntest, extraName, ".nc")
    print(tfile)
    if (file.exists(tfile) && grab_cache) {
        load(tfile)
        
    } else { 
        bcranges = selectPossibleNiche(tcen)#lapply(1:3, randomBCrange, id = id)
       
        c(test, inSphere) := testNiche(bcranges, CentreS, dats, transs)
        pMatout = pMatFind(bcranges)
        inSphere = writeRaster(inSphere, file = nfile, overwrite = TRUE)
        save(bcranges, test, pMatout, inSphere, file = tfile)
    }
    
    return(list(bcranges, test, pMatout, inSphere))
}       


runNicheBoots <- function(model, experiment, CentreS, ..., extraName = ''){
    out = c()
    test = 0
    i = 0
    nRs = rep(0, 5)

    tfile = paste(c('../temp/runNicheBoots', model, experiment, extraName, unlist(CentreS), 
                  nniche, nniches, nboots, '.Rd'), collapse = '_')
    
    if (file.exists(tfile) && FALSE) { load(tfile); return(out)}
    while ((sum(nRs) < nniches || any(nRs[nRs>0] < nniche)) && i < nboots) {
        i = i + 1
        outi = testRandomNiche(i, model, experiment, CentreS, extraName = extraName, ...)
        
        out = c(out, list(outi))
        nRs[outi[[2]]+1] = nRs[outi[[2]]+1] + 1
        print(nRs)
        
        test = sum(lapply(out, function(i) i[[2]])==3)
    }
    print(i)
    print(test)
    out = lapply(1:i, testRandomNiche,  model, experiment, CentreS, extraName = extraName)
    
    save(out, file = tfile)
    return(out)
}

###############################

run4model <- function(model, CentreS, ...) {
    cat(paste("########", model, '########', '\n'))
    experiments = list.files(paste0(dir, model, '/'))
    run4Exp <- function(experiment) {
        print(model)
        print(experiment)
        dats = suppressWarnings(lapply(vars, function(i) raster(paste0(dir, '/', model, '/', experiment, '/', i, '.nc'))))
        dats[[1]][dats[[1]] < 0.01] = 0.01
        dats[[2]][dats[[2]] < 0.01] = 0.01
        dats <<- dats
        
        centres = lapply(dats, findBioclimPnts, CentreS)
        centres[[2]][centres[[2]] <0.01] = 0.01       
        centres[[1]][centres[[1]] <0.01] = 0.01                    
        tcen = mapply(function(F,i) F(i), transs, centres)
        
        print(tcen)
        print("====")
        out = runNicheBoots(model, experiment, CentreS = CentreS, tcen = tcen, ...)
        return(list(out, centres, dats))
    }
    lapply(experiments, run4Exp)
}


##########################

models = list.files(dir)
models = models[!grepl("bcObs-den", models)][1:2]


experiments = list.files(paste0(dir, models[1], '/'))
experiment = experiments[1]

#outsF = mapply(run4model, models, ForestCentres[1:2], SIMPLIFY = FALSE)

###############



#outsS = mapply(run4model, models, SavannaCentres[1:2], SIMPLIFY = FALSE, MoreArgs = list(extraName = 'savanna_new2'))


##################

addAxis <- function(i, side) {
    at = seq(0, 1, length.out = 11)
    
    labels = pitranss[[i]](at)
    if (labels[1] == 0 && tail(labels, 1) == 1) {
        test = labels>0.5
        labels[test] = 1-labels[test]
        labels = signif(labels, 1)
        labels[test] = 1 - labels[test]
    } else labels = signif(labels, 1)
    at = ptranss[[i]](labels)
    labels[labels < 9E-9] = 0
    test = labels>0.9 & labels < 1
    labels[!test] = signif(labels[!test], 1)
    
    axis(side, at = at, labels = labels)
}

######################'#


##########################


x = seq(-1, 1, 0.001)
y = sqrt(1-x^2)
x = c(x, rev(x)); y = c(y, -rev(y))


plotNichProb <- function(i, j, out, biomeCols) {
    
    centres = out[[2]]
    out = out[[1]]

    centre4plot <- function(k) 
        ptranss[[k]](centres[[k]])#itranss[[k]](transs[[k]](centres[[k]])))

    xc = centre4plot(i); yc = centre4plot(j)
    
    plot(xc, yc, xlim = c(0,1), ylim = c(0,1), type = 'n', axes = FALSE, xlab = '', ylab = '')
    addAxis(i, 1)
    addAxis(j, 2)
    mtext(vars[i], side = 1, line = 2)
    mtext(vars[j], side = 2, line = 2)
    #polygon(c(0, 1, 1, 0), c(0, 0, 1, 1), col = '#110033')
    polygon(c(0, 1, 1, 0), c(0, 0, 1, 1), col = 'white')

    xgp = seq(0, 1, 0.01)
    xg = xgp#logit(xgp)
    yg = rep(xg, each = length(xg))
    xg = rep(xg, length(xg))
    x0 = x
     
    addPoly <- function(fail, col, nc) {
        info = fail[[1]]      
        shiftScale <- function(x, i){
            x0 = x
            x = itranss[[1]](transs[[1]](x))
            x = transs[[i]](pitranss[[i]](x))
            x = 2*(x - mean(info[[i]]))/diff(info[[i]])
        }
        
        xt = shiftScale(xg, i); yt = shiftScale(yg, j)
        return(matrix(sqrt(xt^2 + yt^2), ncol = sqrt(length(xg)))<1)
    }
    
    addPolys <- function(id, col) {
        con = out[sapply(out, function(i) i[[2]] == id)] 
        
        if (length(con) == 0) return()               
        img = lapply(con, addPoly, col, length(con))
        imgi = img[[1]]
        for (i in img[-1]) imgi = imgi + i
        imgi = imgi / max(imgi)
        if (col == "#000000") contour(imgi>0, drawlabels = FALSE, add = TRUE)
        else {
            col = paste0(col, as.hexmode(1:80))
            image(dnorm(logit(imgi)), col = col, add = TRUE)
            print(id)
        } #contour(outi, add = TRUE, col = col, levels = c(0.1, 0.5, 0.9), lty = c(3, 2, 1))
    }
    
    points(xc, yc, pch = 4, col = 'red')
    poly2add = which(sapply(1:5, function(id) sum(sapply(out, function(i) i[[2]] == id) ))>2)
    #browser()
    cols = c("#000000", biomeCols)[poly2add]
    
#make_col_vector(c("red", "blue"), ncols = length(poly2add))
    for (nn in 1:3) {
        mapply(addPolys, rev(poly2add), rev(cols))
        mapply(addPolys, poly2add, cols)
    }
    #addPolys(4, '#000000') 
    #addPolys(3, '#FF0000') 
    #addPolys(2, '#0000FF')
    #addPolys(1, '#00DDDD')  
    
    points(xc, yc, pch = 4, col = 'red')
}
                         
plotNiches <- function(out, ..., biomeCols, model, experiment, fnameID = '')   {   
    #options(repr.plot.width=16, repr.plot.height=4)
    png(paste0("../figs/nicheSpace", fnameID, model, experiment, ".png", sep = '-'), 
        res = 300, units = 'in', height = 8, width = 10)
    
    layout(rbind(c(1, 0, 0, 0), 2:5, 6:9, 10), height = c(0.67, 1, 1.5, 0.3))
    par(mar = c(0.25, 2.5, 0.25, 0.5))

    plot(c(0, 1), c(0, 1), type = 'n', axes = FALSE, xlab = '', ylab = '')
    xleg = yleg = seq(0, 1, 0.001)
    addCirc <- function(col, mn = 0) {
        xleg = yleg = seq(0, 1, 0.01)
        z = crossprod(t(dnorm(xleg, 0.5, 1)) , dnorm(yleg, 0.5, 1))
        z = z - min(z)
        z = z/(max(z))
        z[z>1] = 1
        col = paste0(col, as.hexmode(round(seq(1, 255, length.out = 80))))
        if (col == "#000000")  contour(round(z, 2) == 0.5, drawlabels = FALSE, add = TRUE)
        else image(dnorm(logit(z), mn, 0.2), add = TRUE, col = col)
    }
    addCirc("#000000", -0.67)
    addCirc(biomeCols[1], 0.67)
    addCirc(biomeCols[2], 1.33)
    addCirc(biomeCols[3], 2)

    text('not tested', x = 0.03, y = 0.03, srt = -45, xpd = TRUE)
    text('Genralist', x = 0.16, y = 0.16, srt = -45, xpd = TRUE)
    text('Specialist', x = 0.5, y = 0.5, srt = -45, xpd = TRUE)
    
    par(mar = c(0.5, 2.5, 00, 0.5))

    nn = plotNichProb(1, 2, out, biomeCols = biomeCols, ...)
    nn = plotNichProb(2, 3, out, biomeCols = biomeCols, ...)
    nn = plotNichProb(3, 1, out, biomeCols = biomeCols, ...)
    mtext(side = 4, paste0(model, '-', experiment), line = 1, xpd = TRUE)
    plot.new()
    
    for (ni in 1:4)  {          
        pMat = out[[3]][[1]]
        pMat[!is.na(pMat)] = 0
    
        niches = out[[1]][sapply(out[[1]], function(i) i[2] == ni)]
        if (length(niches) == 0) plot.new()
        else {
    
            for (niche in niches)
                pMat = pMat + !is.na(niche[[4]])
            pMat = pMat / length(niches)  
            mar = par("mar"); par(mar = rep(0,4))    
            mpcols = c('#110033', 'white')   
            mplimits = c(0.01, 0.1, 0.2, 0.5, 0.8, 0.9, 0.99)      
            plot_raster_from_raster(pMat, x_range = c(-90, -30), y_range = c(-55, 15), 
                    cols = mpcols, limits = mplimits, quick = TRUE,
                       add_legend = FALSE, e = NULL)
            par(mar = mar)
        }
    }
    add_raster_legend2(mpcols, mplimits, dat= pMat, add = FALSE, transpose = FALSE, maxLab = 1, srt = 0, plot_loc = c(0.1, 0.9, 0.3, 0.7), ylabposScling=0.00)
    dev.off()
    
}
library(maps)
plotModel <- function(outs, model, ...)
    mapply(plotNiches, outs, experiment = experiments, MoreArgs = list(model = model, ...))
    
biomeCols = c("#d95f02", "#7570b3", "#1b9e77")                    
#nn = mapply(plotModel, outsF, models, fnameID = 'forest', 
#            MoreArgs = list(biomeCols = rev(biomeCols)))
#outss[[1]][[1]][[1]]   
                   
#####################

#nn = mapply(plotModel, outsS, models, fnameID = 'savanna', 
#            MoreArgs = list(biomeCols = rev(biomeCols)))



####################

findRefugiaNiche <- function(ID, out) {
    out = out[[1]]
    nichMat = out[[1]][[3]]
    nichMat[] = 0
    for (mt in out)   {  
        if (ID == 0) {
            if (mt[[2]] != 1) nichMat = nichMat + 1 - mt[[3]]
        } else { 
            if (mt[[2]] == ID)  nichMat = nichMat + mt[[3]]            
        }
    }
    
    #nichMat[allDmat == 0] = NaN
    if (ID == 0) nichMat = nichMat/sum(nichMat, na.rm = TRUE)
    else nichMat = nichMat/sum(sapply(out, function(i) i[[2]] == ID))#browser()
    
    return(nichMat)
}


##################
findRefugiaNiches <- function(outs) 
    lapply(outs, lapply, function(out) lapply(1:4, findRefugiaNiche, out))


nichesF = findRefugiaNiches(outsF)
nichesS = findRefugiaNiches(outsS) 

########################

findOutsideRefugia <- function(specDmat, nichMat) {
    inn = sum((specDmat *nichMat), na.rm = TRUE)#/sum(nichMat, na.rm = TRUE)
    nichMat = 1-nichMat
    outn = sum((specDmat *nichMat), na.rm = TRUE)/min(sum(nichMat, na.rm = TRUE), sum(specDmat, na.rm = TRUE))    #out = inn/(inn+outn)
    #if (is.na(out)) browser()
    
    return(c(inn, outn))
}
nicheAssign <- function(...) sapply(specDmatNorm, findOutsideRefugia, ...)

nicheOLf = lapply(nichesF, lapply, lapply, nicheAssign )
nicheOLs = lapply(nichesS, lapply, lapply, nicheAssign )
browser()
nicheOLf[[1]][[1]]


################

sapply(nicheOLf, function(i) round(i[[1]]*100,2))


##########################

sapply(nicheOLs, function(i) round(i[[1]]*100,2))
