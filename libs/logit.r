logit <- function(x, n = exp(100)) {
    x = (x * (n - 1) + 0.5)/n
    log(x/(1-x))
}

