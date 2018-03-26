source("plot_fracCovers.r")

dats = lapply(dats, function(i) i[[1]])


grad.raster <- function(r) {
	nr = nrow(r)
	nc = ncol(r)

	v = matrix(r[], nrow = nc)

	v = list(v[c(2:nc, 2), ],
				 v[c(nc, 1:(nc-1)), ],
				 v[, c(2:nr, 1)],
				 v[, c(nr, 1:(nr-1))])
				
	d = layer.apply(v, function(i) {r[] = as.vector(i); r})

	m = 1.1 * mean(addLayer(r, d), na.rm = TRUE)
	return(abs(m - r))
}

grad.brick <- function(b) layer.apply(b, grad.raster)

mmGrad.brick <- function(b, ...) sum(grad.brick(b, ...)) / nlayers(b)

plotGrad <- function(dat, mn) {
	dat = convert_pacific_centric_2_regular(dat)
	dat[dat > 9E9] = NaN
	mm = mmGrad.brick(dat)
	
	plot_SA_Map_standard(mm, mn, c(0, 0.01, 0.02, 0.04, 0.06, 0.08, 0.1), c("white", "green", "#220000"))
}
par(mfrow = c(1, 2), mar = rep(0, 4))
mapply(plotGrad, dats, names(dats))
