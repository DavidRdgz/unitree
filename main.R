source("tree.R")
source("forest.R")

####
#
# Create a tree
X <- iris[,1:4]; Y <- iris[,5]

dt <- tree(X,Y)
print(table(pred = predict(dt, X), actu = Y))

#rf <- forest(X, Y, 100)
#print(table(pred = rf.predict(rf, X), actu = Y))
