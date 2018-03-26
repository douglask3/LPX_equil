plot_SA_Map_standard <- function(dat, vname = '', limits, cols, add_legend = TRUE) {
	dat[dat > 9E9] = NaN
	
	if (is.infinite(min.raster(dat, na.rm = TRUE))) dat[] = 0.0
	plot_raster_from_raster(dat, limits = limits, cols = cols, quick = TRUE, plot_loc = c(0.7, 1.35, -0.4,	-0.35),
						add_legend = add_legend)
	mtext(vname, side = 1, adj = 0.85, line = -5, font = 1.67)
}
