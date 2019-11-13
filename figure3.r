graphics.off()
rm(biomeAffinityMatrix) 
source("cfg.r")
dir = 'data/dmm_csv/'

files = list.files(dir, full.names = TRUE)

cols = c('#770077', '#6666FF', '#FFBBBB', 'white')

modNames = c('ensemble', 'CNRM-CM33', 'FGOALS-1.0g', 'HadCM3M2', 'MIROC3.2')
expNames = c('fire, [~CO2~]', '[~CO2~]', 'fire', 'control')

biome_diff <- function(i,j) 
    2*sum(abs(biomeAffinityMatrix[i,-1] - biomeAffinityMatrix[j,-1]))/ncol(biomeAffinityMatrix[,-1])
    
nbiomes = nrow(biomeAffinityMatrix)
mat = sapply(1:nbiomes, function(i) sapply(1:nbiomes, biome_diff, i))
colnames(mat) = rownames(mat) = biomeAffinityMatrix[,1]

findScores <- function(file) {
    dat = read.csv(file, stringsAsFactors = FALSE)
    if (ncol(dat) == 6) dat = dat[,-1]
    score <- function(j) mapply(function(i,j) mat[i,j], dat[,1], dat[,j])
    scores = sapply(2:ncol(dat), score)
    
    return(scores)
}

scores = lapply(files, findScores)

scores4t = c()
for (i in scores) scores4t = cbind(scores4t, i)


#ts = apply(scores4t, 2, function(i) apply(scores4t, 2, function(j, i) browser(), i))
#plot(c(-2, 2), c(0, 8), type = 'n')
addDensityLine <- function(i, j) {
    if (sum(i-j) == 0) return()
    lines(density(i-j, adjust = 2))
    i-j
}

#diffs = apply(scores4t, 2, function(i) apply(scores4t, 2, addDensityLine,  i))
ts = apply(scores4t, 2, function(i) apply(scores4t, 2, function(j, i) t.test(i, j, paired = TRUE)[[3]], i))
ws = apply(scores4t, 2, function(i) apply(scores4t, 2, function(j, i)  wilcox.test(i, j, paired = TRUE)[[3]], i))


plot(ts, ws, pch = 19, log = 'xy', xlab = '', ylab = '', cex = 0.67)
mtext(side = 1, line = 2.5, 't.test p-value')
mtext(side = 2, line = 2.5, 'Wilcoxon p-value')

points(ts[1,], ws[1,], col = "red", pch = 19, cex = 1.2)
points(ts[1,], ws[1,], cex = 1.2)
lines(c(0.1, 0.1), c(9E-9, 1))
lines(c(9E-9, 1), c(0.1, 0.1))
browser()

colnames(ts) = rownames(ts) = paste(rep(modNames, each = 4), expNames)
write.csv(ts, file = 'outputs/experiment_ttest.csv')

#ts = ts > 0.1
#cols = c("#110033", "white")
#limits = c(0.001, 0.01, 0.05, 0.1, 0.5)
#cols = make_col_vector(cols, ncols = length(limits)+1)
#ts[is.na(ts)] = 1
#ts_col =  cut_results(ts, limits)#
#
#image(ts_col, col = cols[ts_col])

#ts = lapply(1:5, function(i) ts[,((i-1)*4+1):(i*4)] > 0.05)


png('figs/figure3.png', width = 7.10866, height = 6.1, res = 300, units = 'in')
    layout(rbind(1, 2), heights = c(1.1, 0.1))
    par(mar = c(2.4, 0, 0, 0), oma = c(0.5, 3, 0.5, 1))
    plot(c(0, 24),c(0, max(unlist(scores)) + 0.05), type = 'n', xlab = '', ylab = '', xaxt = 'n')
    mtext(side = 2, line = 2  , "Ecological distance between model and data (MM score)")
    mtext(side = 1, line = 1, 'Climate models')
    
    add_boxes <- function(score, at, name) {
        boxplot(score, xaxt = 'n', at = at + 1:4, names = rep('', 4), add = TRUE, col = cols)
        x = at + 1:4
        y = apply(score, 2, mean)
        mapply(function(i, j) lines(i + c(-0.37, 0.37), rep(j, 2), col = "grey", lwd = 2), x, y)
        mapply(function(i, j) lines(i + c(-0.37, 0.37), rep(j, 2), lty = 3, lwd = 2), x, y)
        #points(x = at + 1:4, , pch = 4, lwd = 3, cex = 2.25)
        mtext(name, at = at + 1, adj = 0, side = 1)
    }

    mapply(add_boxes, scores, seq(0, length.out = length(scores), by = 5), modNames)
    
    x = which(ts[1,] > 0.1) 
    x = x + floor(((x)-1)/4)
    lapply(x, function(i) lines(i + c(-0.4, 0.4), c(-0.02, -0.02), lwd = 3)) 
    
        par(mar = c(0, 0, 0, 0))
    plot(c(0, 1), c(0, 1), type = 'n', axes = FALSE)
   
    #legend('center', , horiz = TRUE, pch = 19, col = cols)
    mapply(function(i, j) mtext.units(line = -3, i, at = j), expNames,0.125 + c(0, 0.25, 0.5, 0.75))
    points(0.125 + c(0, 0.25, 0.5, 0.75), rep(0.5, 4), pch = 15, cex = 2.5, col = 'black')
    points(0.125 + c(0, 0.25, 0.5, 0.75), rep(0.5, 4), pch = 15, cex = 2, col = cols)
dev.off()