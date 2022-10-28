Fmapply <- function(FUNS, rs, SIMPLIFY = FALSE, ...)
        mapply(function(i, j) i(j, ...), FUNS, rs, SIMPLIFY = SIMPLIFY)


