source("quickselect.R")

Gauss <- function (vector, k = 30, ...) {
    mu <- mean(vector)
    s  <- sd(vector)
    
    tmp <- c()
    for (i in seq_along(1:k)) {
        tmp <- c(tmp, mu + s * pnorm(i/(k+1)))
    }
    tmp
}

KTile <- function (vector, k = 30, ...) {
    n   <- length(vector)
    tmp <- c()
    for (i in seq_along(1:k)) {
        tmp <- c(tmp, quick.select(vector, n*i/(k+1)))
    }
    tmp
}

Uniform <- function (vector, k = 30, ...) {
    a.min <- min(vector)
    a.max <- max(vector)
    
    tmp <- c()
    for (i in seq_along(1:k)) {
        tmp <- c(tmp, a.min + i * (a.max - a.min)/(k+1))
    }
    tmp
}

Brute <- function (vector, ...) {
    n   <- length(vector)
    vec <- sort(vector)

    if (n == 1) {
        return(vector)
    }

    tmp <- c()
    for (i in seq_along(1:(n-1))) {
        if (vec[i+1] == vec[i]) {
            tmp <- c(tmp, vec[i])
        } else {
            tmp <- c(tmp, vec[i] + (vec[i+1] - vec[i])/2)
        }
    }
    tmp
}
