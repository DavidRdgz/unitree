source("R/tree.R")
source("R/forest.R")
source("R/plot.R")

####
#
# Pre-Bagging (assuming not random forest)

#s <- sample(1:nrow(iris), 200, replace = TRUE)
#X <- iris[s,1:4]; Y <- iris[s,5]


####
#
# No-Bagging

X <- iris[,1:4]; Y <- iris[,5]

####
#
# Fit decision tree

#dt <- unitree(X, Y, Thresh = Gauss, Pure = Info, is.forest = FALSE, splitter = Split)
#print(table(pred = u.predict(dt, iris[,1:4]), actu = iris[,5]))
#force.graph(dt)

####
#
# Fit random forest

#rf <- uniforest(X, Y, 10, splitter = Branch, f =2)
#print(table(pred = rf.predict(rf, X), actu = Y))
