source("node.R")

Tree <- function () {
    data <- list(
                 tree = list()
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

children.payload <- function (XY, s, ...) {
     children <- list(r.id = XY[["id"]] + 1 , l.id = XY[["id"]] + 2)
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

predict <- function(t, X, ...) UseMethod("predict")
predict.Tree <- function(t, X, ...) {
    predictions <- c()
    for (iter in seq_along(1:nrow(X))) {
        node <- get.branch(t, 1)

        while (node$r.id != 0 && node$l.id != 0) {
            go.right <- X[iter, node$col] > node$cutoff
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

bag <- function (X, Y, n = 300, ...) {
    s <- length(Y)
    sub <- sample(1:s, n, replace = TRUE)
    list(X = X[sub,], Y = Y[sub])
}

get.children <- function (X, Y, subset, ...) {
    RX <- X[subset,]
    RY <- Y[subset]
    LX <- X[!subset,]
    LY <- Y[!subset]
    list( "Right" = list(X = RX, Y = RY), "Left" = list( X = LX, Y = LY))
}

push.children <- function (q, id, children, ...) {
    q <- do.call(push.queue, c(list(q, id = id+1), children[["Right"]]))
    q <- do.call(push.queue, c(list(q, id = id+2), children[["Left"]]))
    q
}

tree <- function(X, Y, Thresh = Gauss, Pure =Twoing, is.forest = FALSE, splitter =Split, ...) {
    args <- mget(names(formals()),sys.frame(sys.nframe()))[-c(1,2)]

    if (isTRUE(is.forest)) {
        B <- bag(X, Y)
        X <- B[["X"]]; Y <- B[["Y"]]
    }

    id <- 1
    q  <- do.call(setup.queue, list(id, X, Y))
    t  <- Tree()

    while(length(q) > 0) {
        XY <- q[[1]]; q <- q[-1]
        s  <- do.call(splitter, c(XY, args))

        if (s[["gain"]] > .05) {
            subset   <- s[["candidates"]]
            children <- do.call(get.children, c(XY, list(subset)))
            q        <- push.children(q, id, children)

            payload  <- children.payload(XY, s)
            n        <- do.call(Node, payload)
            t        <- grow.tree(t, n)

            id <- id + 2
        } else {
            payload <- nochildren.payload(XY, s)
            n       <- do.call(Node, payload)
            t       <- grow.tree(t, n)
        }
    }
    t
}

