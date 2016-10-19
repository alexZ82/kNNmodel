source("./kNNmodelProjectFunctions.R")
#Iris example
iris2 <- iris[,-5]
species_labels <- iris[,5]
distances <- as.matrix(dist(iris2,method ='euclidean',upper=TRUE))
labels <- as.numeric(species_labels)
kNN_iris <- globalNeighbourhoodWithPruning(distances, 1, labels)
# 
#Glass example
library(mlbench)
data(Glass)
glass <-Glass[,-10]
glclasses<-as.numeric(as.matrix(Glass[,10]))
g_dist<-as.matrix(dist(glass,method ='euclidean',upper=TRUE))
kNglass<-globalNeighbourhoodWithPruning(g_dist,1,glclasses)
kNglass_p<-pruneFinalModel(kNglass,1)
