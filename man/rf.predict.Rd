% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/forest.R
\name{rf.predict}
\alias{rf.predict}
\title{Predicting classes from a random forest generated from \code{unitree} objects.}
\usage{
rf.predict(f, X, ...)
}
\arguments{
\item{X}{is a dataframe with the same columns fitting 't'.}

\item{t}{is a tree from fitted by \code{uniforest}: from an ensemble of \code{unitree}s.}
}
\value{
a vector of class labels for each row from 'x'
}
\description{
This function predict classes from a dataframe 'x' to classes 'y' based on a \code{uniforest} object.
}
\details{
This function provides class predictions from a dataframe 'x' based on a \code{uniforest} object. This prediction function traverses all the trees until reaching a leaf node for each tree. The majority vote determines the returned label.
}
\examples{
Y  <- iris[,5]
X  <- iris[,1:4]
dt <- uniforest(X, Y, 10)
p  <- rf.predict(dt, X)
table(pred = p, actu = Y)
}
\author{
David Rodriguez
}
\seealso{
\code{uniforest, unitree, u.predict}
}

