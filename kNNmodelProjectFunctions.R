localNeighbourhood<-function(distances,labels){
  temp<-data.frame(distances,labels)
  temp<-temp[order(temp$distances),]
  groups<-rle(temp$labels)
  cluster.size<-groups$lengths[1]
  cluster.distance<-temp$distances[cluster.size]
  cluster.class<-groups$values[1]
  return(c(cluster.class=cluster.class,cluster.distance=cluster.distance,cluster.size=cluster.size))
}

localInstances<-function(distances,labels){
  temp<-data.frame(distances,labels,inst=1:length(labels))
  temp<-temp[order(temp$distances),]
  groups<-rle(temp$labels)
  instances <- temp$inst[1:groups$lengths[1]]
  return(sort(instances))
}

##' Instance local neighbourhood. (Step 3 in the paper)
##'
##' "For each ‘ungrouped’ data tuple, find its largest local neighbourhood which
##'  covers the largest number of neighbours with the same category."
##'
##' The local neighbourhood for an instance is the largest circle around an 
##' instance which contains only instances of the same class (if r=0). With
##' increasing r, the local neighbourhood may contain instances from other 
##' classes. 
##'
##' @param distances The distances from this instance to all other instances 
##'                  including itself.
##' @param r         Error tolerance degree. Where r=0, the local neighbourhood 
##'                  will only contain instances of the same class. If r=1, the
##'                  local neighbourhood may contain 1 instance from another
##'                  class. 
##' @param labels    The class labels corresponding to the distance vector.       
##' @return The neighbourhood class, distance to the outermost instance in this
##'         neighbourhood and the number of instances inside the neighbourhood.
##'
##' @examples
##'
##' # cluster.size = 6
##' localNeighbourhoodWithPruningComplete(c(1,2,3,4,5,6), 0, c(1,1,1,1,1,1)) 
##'
##' # cluster.size = 1
##' localNeighbourhoodWithPruningComplete(c(1,2,3,4,5,6), 0, c(2,1,1,1,1,1))
##'
##' # cluster size = 3
##' localNeighbourhoodWithPruningComplete(c(1,2,3,4,5,6), 1, c(1,2,1,3,3,3))
localNeighbourhoodWithPruningComplete<-function(distances,r=0,labels){
  temp<-data.frame(distances,labels)
  temp<-temp[order(temp$distances),]
  groups<-rle(temp$labels)
  tmp <- groups$values-groups$values[1] # difference from first class
  tmpS <- which(tmp==0) # which ones are the same 
  tmpN<-which(tmp!=0) # which ones are different
  place<-tail(which(cumsum(groups$lengths[tmpN])<=r),1)
  if(length(place)>0){
    cluster.size <- sum(groups$lengths[1:min(length(groups$lengths), tmpN[place]+1)])
  }else{
    cluster.size <- groups$lengths[1]
  }
  cluster.distance<-temp$distances[cluster.size]
  cluster.class<-groups$values[1]
  return(c(cluster.class=cluster.class,cluster.distance=cluster.distance,cluster.size=cluster.size))
}

localInstancesWithPruning<-function(distances,clusterSize){
  temp<-data.frame(distances,inst=1:length(distances))
  temp<-temp[order(temp$distances),]
  instances <- temp$inst[1:clusterSize]
  return(sort(instances))
}

globalNeighbourhood<-function(distances,labels){
  allLocals<-apply(distances,2,localNeighbourhood,labels)
  theLocals<-data.frame(t(allLocals))
  theLocals$instances<-1:nrow(theLocals)
  theLocals$bound<-rep('u',nrow(theLocals))
  clusters<-array(NA,c(nrow(theLocals),4))
  clstr<-0
  while(length(theLocals$bound[theLocals$bound=='u'])>0){
    clstr<-clstr+1
    tmp<- subset(theLocals, bound=='u')
    tmp <- tmp[with(tmp, order(-cluster.size,cluster.distance)),]
    clusters[clstr,]<-as.matrix(tmp[1,1:4])
    clusterMembers<-localInstances(distances[clusters[clstr,4],],labels)
    theLocals$bound[theLocals$instances %in% c(clusterMembers)]<-'b'
  }
  theClusters<-clusters[!is.na(clusters[,1]),]
  return(theClusters)
}

globalNeighbourhoodWithPruning<-function(distances,r=0,labels){

  # step 3
  allLocals<-apply(distances,2,localNeighbourhoodWithPruningComplete,r,labels)

  theLocals<-data.frame(t(allLocals))
  theLocals$instances<-1:nrow(theLocals)
  theLocals$bound <- F
  clusters <- matrix(NA, nrow=nrow(theLocals), ncol=4)
  clstr<-0
  while(!all(theLocals$bound)) {
    clstr<-clstr+1
    tmp<- subset(theLocals, !bound)
    tmp <- tmp[with(tmp, order(-cluster.size, -cluster.distance)),]
    clusters[clstr,]<-as.matrix(tmp[1,1:4])
    cluster.size <- clusters[clstr, 3]
    instance <- clusters[clstr, 4]
    clusterMembers <- localInstancesWithPruning(distances[instance, ], cluster.size)
    theLocals$bound[theLocals$instances %in% clusterMembers] <- T
  }
  theClusters<-clusters[!is.na(clusters[,1]),]
  return(theClusters)
}


pruneFinalModel<-function(kNNmodel,N){
  return(kNNmodel[-which(kNNmodel[,3]<=N),])
}
