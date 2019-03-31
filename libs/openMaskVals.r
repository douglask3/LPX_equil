openMaskVals <- function(file, ..., FUN = raster) {
    r = FUN(file, ...)
    r[r > 9E9] = NaN
    return(r)
}