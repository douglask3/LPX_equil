layers2list <- function(r) {
	r = layer.apply(r, c)
	r = lapply(r, function(i) i[[1]])
	return(r)
}