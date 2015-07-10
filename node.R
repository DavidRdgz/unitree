source("purity.R")
source("thresh.R")

Node <- function (id, X, Y, label, col= 0, cutoff= 0, l.id = 0, r.id = 0, candidates = 0, gain = 0) {
    data <- list(
                 id         = id,
                 X          = X,
                 Y          = Y,
                 label      = label,
                 candidates = candidates,
                 gain       = gain,
                 l.id       = l.id,
                 r.id       = r.id,
                 col        = col,
                 cutoff     = cutoff
                )
    class(data) <- append(class(data), "Node")
    return(data)
}

BinarySplits <- function (X, Thresh, ...) {
    lapply(X, Thresh)
}

get.Gain <- function (Pure, Y, Rcandidates, ...) {
    IR     <- Pure(Y[Rcandidates])
    IL     <- Pure(Y[!Rcandidates])
    p      <- sum(Rcandidates)/length(Y)

    if (identical(Pure, match.fun("Twoing"))) {
        return(AbsSum(IL, IR, p))
    } else {
        return(Gain(Pure(Y), IL, IR, p))
    }
}

get.GainRatio <- function (gain, Rcandidates) {
    p <- sum(Rcandidates)/length(Rcandidates)
    gain/Potential(p)
}

load <- function (col, Rcandidates, gain, cutoff, ...) {
    list("col" = col, "candidates" = Rcandidates, "gain" = gain, "cutoff" = cutoff)
}

ImpurityMeasures <- function (LiRi, X, Y, Pure, ...) {
    measures <- list()
    
    for (col in seq_along(X)) {
        col.score      <- 0
        tmp.measures   <- list()

        for (cutoff in LiRi[[col]]) {

            Rcandidates <- X[,col] > cutoff
            gain        <- do.call(get.Gain, list(Pure, Y, Rcandidates))

            if (is.finite(gain) && gain >= col.score) {
                tmp.measures <- load(col, Rcandidates, gain, cutoff)
                col.score <- gain
            }
        }

        if (length(tmp.measures) > 0){
            measures[[length(measures)+1]] <- tmp.measures
        }
    }
    measures
}

ImpurityMeasures2 <- function (LiRi, col,  X, Y, Pure, ...) {
    col.score <- 0
    measures  <- list()

    for (cutoff in LiRi) {
        Rcandidates <- X > cutoff
        gain        <- do.call(get.Gain, list(Pure, Y, Rcandidates))

        if (is.finite(gain) && gain > col.score) {
            measures <- load(col, Rcandidates, gain, cutoff)
            col.score <- gain
        }
    }
    measures
}

TopBinary <- function (measures, ...) {
    max <- list(0,0,gain = 0,0)
    for (i in measures) {
        if (i[["gain"]] > max[["gain"]]) {
            max <- i
        } 
    }
    max
}

wig <- function (X, ...) {
    col <- ncol(X)
    list(col = col, X = X)
}

swig <- function (X, ...) {
    k <- ncol(X)
    n <- floor(log(k, base = 2)+1)
    col <- sample(1:k, n)
    #list(col = col, X = X[,col])
    X[,col]
}

twig <- function (X, f = 1, ...) {
    k <- ncol(X)
    if (f == 1) {
        col <- sample(1:k, f)
        return(list(col = col, X = X[,col]))
    } else {
        col <- sample(1:k, f)
        return(list(col = col, X = apply(X[,col], 1, sum)))
    }
}

Split <- function (X, Y, Thresh, Pure, is.forest, ...) {

    if (isTRUE(is.forest)) {
        X <- swig(X)
    }
    
    LiRi <- BinarySplits(X, Thresh)
    C    <- ImpurityMeasures(LiRi, X, Y, Pure)
    TopBinary(C)
}

Branch <- function (X, Y, Thresh, Pure, is.forest, ...) {

    if (isTRUE(is.forest)) {
        Xid <- twig(X)
    }

    LiRi <- BinarySplits(list(Xid[["X"]]), Gauss)
    do.call(ImpurityMeasures2, c(LiRi, Xid, Y, Pure))
}

MajClasses <- function (C,Y, ...) {
    c  <- C[["candidates"]]
    r  <- sum(c)
    l  <- sum(!c)
    R  <- Y[c]
    L  <- Y[!c]

    pr <- table(R)/r
    pl <- table(L)/l

    Cr <- pr >= pl
    CR <- names(Cr)[Cr]

    C[["candidates"]] <- Y %in% CR
    C
}

