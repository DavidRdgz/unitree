source("R/node.R")

Tree <- function (args = 0) {
    data <- list(
                 tree = list(),
                 call = args
                )
    class(data) <- append(class(data), "Tree")
    return(data)
}

get.top <- function (Y, ...) {
    names(sort(table(Y), decreasing = TRUE))[[1]]
}

get.X <- function (XY, subset, ...) {
   droplevels(XY[[2]][subset,])
}

get.Y <- function (XY, subset, ...) {
   droplevels(XY[[3]][subset])
}

setup.queue <- function (id, X, Y, ...) {
    list(list(id = id, X = X, Y = Y, label = get.top(Y)))
}

push.queue <- function (q, id, X, Y, ...) {
    q[[length(q) +1]] <- list(id = id,X = X,Y= Y,label= get.top(Y))
    q
}

children.payload <- function (XY, s, id,  ...) {
     children <- list(r.id = id + 1 , l.id = id + 2)
     c(XY, s, children)
}

nochildren.payload <- function (XY, s, ...) {
    c(XY, s)
}

grow.tree <- function (t, n, ...) UseMethod("grow.tree")
grow.tree.Tree <- function (t, n, ...) {
    t$tree[[length(t$tree)+1]] <- n
    t
}

get.branch <- function(t, n, ...) UseMethod("get.branch")
get.branch.Tree <- function (t, n, ...) {
    for (node in t$tree) {
        if (node[["id"]] == n) {
            return(node)
        }
    }
}

get.leaves <- function (t, ...) {
    leaves <- list()
    for (node in t$tree) {
        if (node$l.id == 0 && node$r.id == 0) {
            leaves[[length(leaves) + 1 ]] <- node
        }
    }
    leaves
}

is.a.leaf <- function (n, ...) {
    n$l.id == 0
}

get.parent <- function (t, id) {
    for (node in t$tree) {
        if (node$l.id == id || node$r.id == id) {
            return(node)
        }
    }
}

#' Predicting classes from a univariate decision tree
#'
#' This function predict classes from a dataframe 'x' to classes 'y' based on a \code{unitree} object.
#'
#' @param t is a tree from fitted by \code{unitree}
#' @param X is a dataframe with the same columns fitting 't'.
#'
#' @return a vector of class labels for each row from 'x'
#'
#' @author David Rodriguez
#' @details
#' This function provides class predictions from a dataframe 'x' based on a \code{unitree} object. This prediction function traverses the tree until reaching a leaf node where the label is returned.
#'
#' @seealso \code{unitree, u.predict}
#' @examples
#' Y  <- iris[,5]
#' X  <- iris[,1:4]
#' dt <- unitree(X, Y)
#' p  <- u.predict(dt, X)
#' table(pred = p, actu = Y)

u.predict <- function(t, X, ...) {
    predictions <- c()
    for (iter in seq_along(1:nrow(X))) {
        node <- get.branch(t, 1)

        while (node$r.id != 0 && node$l.id != 0) {
            if (identical(t$call$splitter, Branch)) {
                go.right <- sum(X[iter, node$col]) > node$cutoff
            } else {
                go.right <- X[iter, node$col] > node$cutoff
            }

            if (go.right) {
                node <- get.branch(t, node$r.id)
            } else {
                node <- get.branch(t, node$l.id)
            }
        }
        predictions <- c(predictions, node$label)
    }
    predictions
}

bag <- function (X, Y, n = 200, ...) {
    s <- length(Y)
    sub <- sample(1:s, n, replace = TRUE)
    list(X = X[sub,], Y = Y[sub])
}

get.children <- function (X, Y, subset, ...) {
    RX <- droplevels(X[subset,])
    RY <- droplevels(Y[subset])
    LX <- droplevels(X[!subset,])
    LY <- droplevels(Y[!subset])
    list( "Right" = list(X = RX, Y = RY), "Left" = list( X = LX, Y = LY))
}

push.children <- function (q, n.id, children, ...) {
    q <- do.call(push.queue, c(list(q = q, id = n.id+1), children[["Right"]]))
    q <- do.call(push.queue, c(list(q = q, id = n.id+2), children[["Left"]]))
    q
}

is.impure <- function (Y, ...) {
    length(names(table(Y))) > 1
}

no.children <- function (t, XY, s, ...) {
    payload <- nochildren.payload(XY, s)
    n       <- do.call(Node, payload)
    t       <- grow.tree(t, n)
}

yes.children <- function (t, XY, s, id, ...) {
    payload  <- children.payload(XY, s, id)
    n        <- do.call(Node, payload)
    t        <- grow.tree(t, n)
    t
}

#' A decision tree and random forest classifier
#'
#' A function to construct a univariate decision tree classifier or random forest.
#'
#' @param X dataframe of real-values (does not contain missing values).
#' @param Y a vector of class set as factors.
#' @param Thresh Either: Gauss, KTile, Uniform, Brute. Approximation splitting techniques.
#' @param Pure Either: Gini, Info, Twoing. Funcitons to calculate impurity at a given node.
#' @param is.forest If \code{TRUE} then either bagging or random selection of columns of \code{X}.
#' @param splitter Either: Split or Branch. If split, then traditional univariate decision tree. If branch, then random forest, and potential sum of attributes.
#'
#' @return a \code{tree} object consisting of nodes.
#' @author David Rodriguez
#' @details This function fits a decision tree with the above options. It is therefore, a potentially, exhaustive approach to fitting and selecting a decision tree.
#'
#' @seealso \code{u.predict, force.graph}
#'
#' @examples
#' X <- iris[,1:4]
#' Y <- iris[,5]
#' dt <- unitree(X, Y)

unitree <- function(X, Y, Thresh = Gauss, Pure =Info, is.forest = FALSE, splitter =Split, ...) {
    args <- mget(names(formals()),sys.frame(sys.nframe()))[-c(1,2)]

    if (isTRUE(is.forest)) {
        B <- bag(X, Y)
        X <- B[["X"]]; Y <- B[["Y"]]
    }

    id <- 1
    q  <- do.call(setup.queue, list(id, X, Y))
    t  <- Tree(args)

    while(length(q) > 0) {
        XY <- q[[1]]; q <- q[-1]

        s      <- do.call(splitter, c(XY, args))
        subset <- s[["candidates"]]

        if (is.null(subset) || !is.impure(XY[["Y"]])) {
            t        <- no.children(t, XY, s)
        } else {
            children <- do.call(get.children, c(XY, list(subset = subset)))
            q        <- push.children(q, id, children)

            t        <- yes.children(t, XY, s, id)
            id <- id + 2
        }
    }
    t
}

