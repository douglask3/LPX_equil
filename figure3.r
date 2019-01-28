graphics.off() 
source("cfg.r")
dir = 'data/dmm_csv/'

files = list.files(dir, full.names = TRUE)

cols = c('#AA66AA', '#AAAAFF', '#FFAAAA', 'white')


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

pdf('figs/figure3.pdf')
    layout(rbind(1, 2), heights = c(1, 0.1))
    par(mar = c(1, 0, 0, 0), oma = c(0, 3, 1, 1))
    plot(c(0, 24),c(0, max(unlist(scores)) + 0.05), type = 'n', xlab = '', ylab = '', xaxt = 'n')

    add_boxes <- function(score, at, name) {
        boxplot(score, xaxt = 'n', at = at + 1:4, names = rep('', 4), add = TRUE, col = cols)
        points(x = at + 1:4, apply(score, 2, mean), pch = 4, lwd = 3, cex = 2.25)
        mtext(name, at = at + 1, adj = 0, side = 1)
    }

    mapply(add_boxes, scores, seq(0, length.out = length(scores), by = 5), c('ensemble', 'CNRM-CM33', 'FGOALS-1.0g', 'HadCM3M2', 'MIROC3.2'))

    plot(c(0, 1), c(0, 1), type = 'n', axes = FALSE)
    #legend('center', , horiz = TRUE, pch = 19, col = cols)
    mapply(function(i, j) mtext.units(line = -3, i, at = j),
           c('Fire, [~CO2~]', '[~CO2~]', 'fire', 'control'),0.125 + c(0, 0.25, 0.5, 0.75))
    points(0.125 + c(0, 0.25, 0.5, 0.75), rep(0.5, 4), pch = 15, cex = 2.5, col = 'black')
    points(0.125 + c(0, 0.25, 0.5, 0.75), rep(0.5, 4), pch = 15, cex = 2, col = cols)
dev.off()