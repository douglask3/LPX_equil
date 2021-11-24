openMaskVals <- function(file, ..., extent = NULL, fun = NULL, FUN = raster) {
    if (is.null(fun)) fun = FUN
    r = fun(file, ...)
    if (!is.null(extent)) r = raster::crop(r, y = extent)
    r[r > 9E9] = NaN
    return(r)
}