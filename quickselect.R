quick.select <- function(x, k, ...) {
    pivot <- x[runif(1, 1, length(x))]

    x1 <- c(); x2 <- c()
    for (i in seq_along(x)) {
        if (x[i] < pivot) {
            x1 <- c(x1, x[i])
        } else if (x[i] > pivot) {
            x2 <- c(x2, x[i])
        }
    }

    if ( k <= length(x1)) {
        return(quick.select(x1, k))
    } else if (k > length(x) - length(x2)) {
        return(quick.select(x2, k - (length(x)-length(x2))))
    } else {
        return(pivot)
    }
}

