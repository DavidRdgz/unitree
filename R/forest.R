source("R/tree.R")

Forest <- function () {
    data <- list(
                 forest = list()
                 )
    class(data) <- append(class(data), "Tree")
    return(data)
}

add.Tree <- function (f, t, ...) {
    f$forest[[length(f$forest) + 1]] <- t
    f
}

top.pred <- function (tmp.pred, ...) {
    names(sort(table(tmp.pred)))[[1]]
}

rf.predict <- function (f, X, ...) {
    pred <- c()
    for (row in 1:nrow(X)){
        tmp.pred <- c()
        for (t in f$forest) {
            tmp.pred <- c(tmp.pred, t.predict(t, X[row,]))
        }
    pred <- c(pred, top.pred(tmp.pred))
    }
    pred
}

#' A random forest classifier
#'
#' A function to construct a univariate and sum of attributes random forest.
#'
#' @param X dataframe of real-values (does not contain missing values).
#' @param Y a vector of class set as factors.
#' @param n the number of trees to make.
#' @param Thresh Either: Gauss, KTile, Uniform, Brute. Approximation splitting techniques.
#' @param Pure Either: Gini, Info, Twoing. Funcitons to calculate impurity at a given node.
#' @param grow If \code{TRUE} then either bagging or random selection of columns of \code{X}.
#' @param splitter Either: Split or Branch. If split, then traditional univariate decision tree. If branch, then random forest, and potential sum of attributes.
#'
#' @return a \code{uniforest} object consisting of \code{unitree} objects consisting of nodes.
#' @author David Rodriguez
#' @details This function fits a random forest with the above options. It is therefore, a potentially, exhaustive approach to fitting and selecting a random forest classifier
#'
#' @seealso \code{rf.predict}
#'
#' @examples
#' X <- iris[,1:4]
#' Y <- iris[,5]
#' dt <- uniforest(X, Y, 10)


uniforest <- function (X, Y, n, grow = TRUE, splitter = Branch, thresh = KTile, ...) {
    f <- Forest()

    for (i in 1:n) {
        print(i)
        t <- unitree(X,Y, thresh = thresh, is.forest = grow, splitter = splitter)
        f <- add.Tree(f, t)
    }
    f
}
