logit <- function(x, n = length(dats[[1]])) {
    x = (x * (n - 1) + 0.5)/n
    log(x/(1-x))
}

