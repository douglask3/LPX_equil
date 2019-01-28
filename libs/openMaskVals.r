openMaskVals <- function(file) {
    r = raster(file)
    r[r > 9E9] = NaN
    return(r)
}