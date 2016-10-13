localNeighbourhood<-function(distances,labels){
  temp<-data.frame(distances,labels)
  temp<-temp[order(temp$distances),]
  groups<-rle(temp$labels)
  cluster.size<-groups$lengths[1]
  cluster.distance=temp$distances[cluster.size]
  cluster.class=groups$values[1]
  return(c(cluster.class=cluster.class,cluster.distance=cluster.distance,cluster.size=cluster.size))
}

localInstances<-function(distances,labels){
  temp<-data.frame(distances,labels,inst=1:length(labels))
  temp<-temp[order(temp$distances),]
  groups<-rle(temp$labels)
  instances <- temp$inst[1:groups$lengths[1]]
  return(sort(instances))
}

localNeighbourhoodWithPruningComplete<-function(distances,r,labels){
  temp<-data.frame(distances,labels)
  temp<-temp[order(temp$distances),]
  groups<-rle(temp$labels)
  tmp <- groups$values-groups$values[1]
  tmpS <- which(tmp==0)
  tmpN<-which(tmp!=0)
  place=tail(which(cumsum(groups$lengths[tmpN])<=r),1)
  if((length(tmpS)>1)&(length(place)!=0)){
    cluster.size<-cumsum(groups$lengths)[tmpN[place]-1]
  }else{
    cluster.size<-groups$lengths[1]
  }
  cluster.distance=temp$distances[cluster.size]
  cluster.class=groups$values[1]
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
    head(tmp)
    clusters[clstr,]<-as.matrix(tmp[1,1:4])
    head(clusters)
    clusterMembers<-localInstances(distances[clusters[clstr,4],],labels)
    theLocals$bound[theLocals$instances %in% c(clusterMembers)]='b'
  }
  theClusters<-clusters[!is.na(clusters[,1]),]
  return(theClusters)
}

globalNeighbourhoodWithPruning<-function(distances,r,labels){
  allLocals<-apply(distances,2,localNeighbourhoodWithPruningComplete,r,labels)
  theLocals<-data.frame(t(allLocals))
  theLocals$instances<-1:nrow(theLocals)
  theLocals$bound<-rep('u',nrow(theLocals))
  clusters<-array(NA,c(nrow(theLocals),4))
  clstr<-0
  while(length(theLocals$bound[theLocals$bound=='u'])>0){
    clstr<-clstr+1
    tmp<- subset(theLocals, bound=='u')
    tmp <- tmp[with(tmp, order(-cluster.size,cluster.distance)),]
    head(tmp)
    clusters[clstr,]<-as.matrix(tmp[1,1:4])
    head(clusters)
    clusterMembers<-localInstancesWithPruning(distances[clusters[clstr,4],],clusters[clstr,3])
    theLocals$bound[theLocals$instances %in% c(clusterMembers)]='b'
  }
  theClusters<-clusters[!is.na(clusters[,1]),]
  return(theClusters)
}


pruneFinalModel<-function(kNNmodel,N){
  return(kNNmodel[-which(kNNmodel[,3]<=N),])
}
