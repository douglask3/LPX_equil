
grad.raster <- function(r) {
	nr = nrow(r)
	nc = ncol(r)

	v = matrix(r[], nrow = nc)

	v = list(v[c(2:nc, 2), ],
				 v[c(nc, 1:(nc-1)), ],
				 v[, c(2:nr, 1)],
				 v[, c(nr, 1:(nr-1))])
				
	d = layer.apply(v, function(i) {r[] = as.vector(i); r})

	m = mean(addLayer(r, d), na.rm = TRUE)
	return(1.1 * abs(m - r))
}

grad.brick <- function(b) layer.apply(b, grad.raster)

mmGrad.brick <- function(b, ...) sum(grad.brick(b, ...)) / nlayers(b)
