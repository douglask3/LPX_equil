plot_SA_Map_standard <- function(dat, vname = '', limits, cols, 
                                 add_legend = TRUE, readyCut = FALSE,
                                quick = TRUE, ...) {
	dat[dat > 9E9] = NaN
	
	if (is.infinite(min.raster(dat, na.rm = TRUE))) dat[] = 0.0
    plot_map_standrd(dat, limits = limits, cols = cols, readyCut = readyCut,
                     quick = quick, plot_loc = c(0.7, 1.35, -0.4, -0.35),
						add_legend = add_legend, ...)
	mtext.units(vname, side = 3, adj = 0.9, line = -2)
}
