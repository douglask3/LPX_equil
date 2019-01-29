plot_map_standrd <- function(r, cols, limits, readyCut = TRUE, ...) {
    r = crop(r,  c(-108, -33, -60, 25))
    plot_map <- function(colsi, ...) 
        plot_raster_from_raster(r, cols = colsi, limits = limits, quick = TRUE, readyCut = readyCut, coast.lwd = NULL, add_legend = FALSE,...)
    
    openResampleMask <- function(file, ...) {
        mask = raster(file)
        mask = raster::resample(mask, r)
        mask = disaggregate(mask, 5)
    }
    
    plot_map(cols)
    
    mask = openResampleMask('data/seamask.nc')
    contour(mask, add = TRUE, drawlabels = FALSE, lwd = 1)

    #plot_map(cols = make.transparent(cols,0.67), add = TRUE)

    mask = openResampleMask('data/icemask.nc')
    plot_raster_from_raster(mask, col = c('transparent', '#CCCCCC'), limits = c(0.5), coast.lwd = NULL, add = TRUE, quick = TRUE, add_legend = FALSE)
}