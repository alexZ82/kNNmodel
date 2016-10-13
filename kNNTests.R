source("./kNNmodelProjectFunctions.R")
#Iris example
iris2 <- iris[,-5]
species_labels <- iris[,5]
i_dist<-as.matrix(dist(iris2,method ='euclidean',upper=TRUE))
theClasses<-rep(1:3,each=50)
kNN_iris <- globalNeighbourhoodWithPruning(i_dist,1,theClasses)
# 
#Glass example
glass <-Glass[,-10]
glclasses<-as.numeric(as.matrix(Glass[,10]))
g_dist<-as.matrix(dist(glass,method ='euclidean',upper=TRUE))
kNglass<-globalNeighbourhoodWithPruning(g_dist,1,glclasses)
kNglass_p<-pruneFinalModel(kNglass,1)
