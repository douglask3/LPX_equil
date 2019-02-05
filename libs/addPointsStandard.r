addPointsStandard <- function(x, y, col, ...) {
    points(x, y, pch = 19, cex = 2.5)
    points(x, y, pch = 19, col = col,cex = 2)
    col = make.transparent(col, 0.9)
    for (i in 1:5) {
        index = sample(1:length(x), length(x), replace = FALSE)
        points(x[index], y[index], pch = 19, col = col[index], cex = 2*(5-i)/5)
    }
}