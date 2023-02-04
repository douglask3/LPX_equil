############
## set-up ##
############
source('../gitProjectExtras/gitBasedProjects/R/sourceAllLibs.r')
sourceAllLibs('../gitProjectExtras/gitBasedProjects/R/', trace = FALSE)
sourceAllLibs('../rasterextrafuns/rasterPlotFunctions/R/', trace = FALSE)
sourceAllLibs('../rasterextrafuns/rasterExtras/R/', trace = FALSE)

library(raster)
#library(rasterExtras)
library(fields)
sourceAllLibs("libs/", trace = FALSE)
graphics.off()

file = "outputs/biomes.Rd"

load(file)

biome = biomes[,2]

#heights = lapply(mainOuts, lapply, function(i) i[[1]])
#fpc = lapply(mainOuts, lapply, function(i) i[[2]])
height = heights[[2]]
fpc = fpcs#[[2]]
testPol <- function(r) {
    r[is.na(r)] = 0
    pol = rasterToPolygons(r, digits = 2, dissolve = TRUE)

    ps = lapply(pol@polygons , slot , "Polygons")[[1]]
    nfrags = length(ps)
    polArea = sapply(ps, function(x) slot(x, "area"))
    polSizes = sort(polArea, decreasing=TRUE)[-1]
    polSizes = polSizes/max(polSizes)
    fragIndex = sum(polSizes)
    
    return(c(sum((r * raster::area(r))[], na.rm = TRUE)/1E6, 
           sum(polSizes > 0.1), nfrags, fragIndex))
}

testBiome <- function(biome, height = NULL) {
    tfile = strsplit(strsplit(filename(biome), 'outputs/')[[1]][2], '/')[[1]][1:2]
    
    tfile = paste0(c('temp/fragmentation_biomes', tfile, '.Rd'), collapse = '-')
    print(tfile)
    if (file.exists(tfile)) {load(tfile); return(out)}# && !grepl('clusted', tfile)
    if (is.null(height)) {
        out = t(sapply(1:4, function(i) testPol(biome == i)))
        colnames(out) = c('Area', 'Major frags', 'n. frags', 'frag index')
        rownames(out) = letters[1:4]
    } else {
        biome[biome == 1 & height < 10.5] = 8
        forest = testPol(biome == 1)
        savanna = testPol(biome == 8 | biome == 9) 

        forestWood = testPol(biome == 1 | (biome == 8 & height > 5))
        savannaShort = testPol((biome == 8 | biome == 9) & height < 5) 
        out = rbind(c(forest, savanna), c(forestWood, savannaShort))
        
        colnames(out) = paste(rep(c('forest', 'savanna'), each = 4), 
                                  c('Area', 'Major frags', 'n. frags', 'frag index'))
        rownames(out) = c('Tall savanna', 'Short forest')
    }
    save(out, file = tfile)
    out
}

out = mapply(testBiome, biome, height, SIMPLIFY = FALSE)
out = do.call(rbind, out)
write.csv(out, file = 'outputs//biome_summ.csv')

biome = biomes[,1]
height = heights[[1]]

out = mapply(testBiome, biome, height, SIMPLIFY = FALSE)
out = do.call(rbind, out)
write.csv(out, file = 'outputs//biome_summ-uncorr.csv')

out = mapply(testBiome, biomes[,3], SIMPLIFY = FALSE)

out = do.call(rbind, out)
write.csv(out, file = 'outputs//biome_summ-clust.csv')



models = sapply(biome, function(i)
                            strsplit(strsplit(filename(i), 'outputs/')[[1]][2], '/')[[1]][1])
expNs = c("pre-correction", "post-correction")

cols_hgt = c('#fff7fb','#ece2f0','#d0d1e6','#a6bddb','#67a9cf',
               '#3690c0','#02818a','#016c59','#014636')
cols_fpc = c('#ffffe5','#f7fcb9','#d9f0a3','#addd8e','#78c679',
               '#41ab5d','#238443','#006837','#004529')

lims_hgt = c(0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20)
lims_fpc = c(0, 0.1, 0.2, 0.4, 0.6, 0.8, 0.9)

plotVar <- function(var, name, cols, lims, extend_max = TRUE, maxLab = '', lettrs, units = '', scale = 1, title = '') {
    file = paste0("figs/correctedUncorrected-", name, ".png")

     plotExp <- function(expn, rs, lastOnly = FALSE, letter = '') {
         plotModel <- function(name, r) {
            plot_SA_Map_standard(r*scale, '', lims*scale, cols, add_legend=FALSE, quick = !lastOnly)
            
            if (expNs[1] == expn) mtext(side = 2, name, line = -1, adj = 0.35)
            if ((models[1] == name) || (letter == 'a' || letter == 'b'))
                    mtext(side = 3, expn)
            mtext(side = 1, letter, line = -2.5, adj = 0.67)
        }
        
        if (lastOnly) plotModel('', tail(rs, 1)[[1]])
        else mapply(plotModel, models, rs)
        if (letter == 'a' || letter == 'c') mtext(side = 2, at = 0.5, line = 0.0, xpd = NA, title)
    }
    legendFUN <- function() {
        legendColBar(c(0.3, 0.7), c(0.1, 0.9), cols, lims*scale, add = FALSE, 
                 extend_max = extend_max, extend_min = FALSE, maxLab = maxLab, 
                 transpose = T, switch = F, oneSideLabels = F, units = units)
    }

    png(file, height = 12, width = 3.5, res = 300, units = 'in')
    layout(rbind(matrix(1:10, ncol = 2), 11), height = c(rep(1, 5), 0.3))
    par(mar = rep(0, 4), oma = rep(1, 4))
   
    mapply(plotExp, expNs, var)
    legendFUN()
    dev.off()
    
    mapply(plotExp, expNs, var, lastOnly = TRUE, letter = lettrs)
     # par("usr")[1]
    mar = par("mar")
    par(mar = c(0, 0, 0, 0))
    legendFUN()
    par(mar = mar)
    
}
file = "figs/correctedUncorrected-slimmed-both.png"
png(file, height = 1.3*12*2.3/5.2, width = 3.5, res = 300, units = 'in')

layout(rbind(1:2, 3, 4:5, 6), height = c(1, 0.33, 1, 0.33))
par(mar = rep(0, 4), oma =c(0, 1.5, 2, 0))
plotVar(heights, "height", cols_hgt, lims_hgt, TRUE, lettrs = c('a', 'b'), units = 'm', title = 'Height')
plotVar(fpc, "fpc", cols_fpc, lims_fpc, FALSE, lettrs = c('c', 'd'), units = '%', maxLab = 100, scale = 100, title = 'Fractional Cover')
dev.off()
    
  

