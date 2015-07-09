source("tree.R")
source("forest.R")

####
#
# Create a tree
s <- sample(1:nrow(iris), 100, replace = TRUE)
X <- iris[s,1:4]; Y <- iris[s,5]

dt <- tree(X,Y)
print(table(pred = predict(dt, iris[,1:4]), actu = iris[,5]))

#rf <- forest(X, Y, 100)
#print(table(pred = rf.predict(rf, X), actu = Y))
