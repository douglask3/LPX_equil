library(rgdal)
rivers = try(readOGR(dsn = "../data/MajorRivers", layer = "MajorRivers"), silent = TRUE)
if (class(rivers) == "try-error") rivers = try(readOGR(dsn = "data/MajorRivers", layer = "MajorRivers"), silent = TRUE)

    
plot_map_standrd <- function(r, cols, limits, readyCut = TRUE, add_legend = FALSE,...) {
    r = crop(r,  c(-108, -33, -60, 25))
    plot_map <- function(colsi, ...) 
        plot_raster_from_raster(r, cols = colsi, limits = limits, quick = TRUE, readyCut = readyCut, coast.lwd = NULL, add_legend = add_legend, x_range = c(-83, -33),...)
    
    openResampleMask <- function(file, ...) {
        mask = raster(file)
        mask = raster::resample(mask, r)
        mask = disaggregate(mask, 5)
    }
    
    plot_map(cols)
    
    seamaskFile = 'data/seamask.nc'; icemaskFile = 'data/icemask.nc'
    if (!file.exists(seamaskFile)) {
        seamaskFile = paste0('../', seamaskFile); icemaskFile = paste0('../', icemaskFile)
    }
    mask = openResampleMask(seamaskFile)
    contour(mask, add = TRUE, drawlabels = FALSE, lwd = 1)

    #plot_map(cols = make.transparent(cols,0.67), add = TRUE)

    mask = openResampleMask(icemaskFile)
    plot_raster_from_raster(mask, col = c('transparent', '#CCCCCC'), limits = c(0.5), coast.lwd = NULL, add = TRUE, quick = TRUE, add_legend = FALSE)
    lines(rivers, col = "white", lwd = 1)
    polygon(c(-40, -30, -30, -40), c(-60, -60, -50, -50), col = "white", border = "white")
}
