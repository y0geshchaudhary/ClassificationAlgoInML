euDistBasedMatrix<- function(x1,x2,label,k){
  tempDist<-(x1-x2)
  for (i in 1:length(tempDist)) {
    tempDist[i] <- tempDist[i]^2
  }
  eu<- matrix(rep(0), nrow = nrow(tempDist), ncol = 1)
  for (x in 1:nrow(tempDist)) {
    eu[x,1] <- sqrt(sum(tempDist[x,]))
  }
  
  eu<- cbind(eu, as.character(label))
  sortedTempEuDist<- eu[order(eu[,1]),]
  sortedTempEuDist<- sortedTempEuDist[1:k,]
  return(sortedTempEuDist[,2])
}


dataPreprocessing<- function(dat){
  
  indx <- sapply(dat, is.factor)
  indx[names(indx[length(indx)])]<-FALSE
  dat[indx] <- lapply(dat[indx], function(x) as.numeric(x))
  return(dat)
}

knn.prediction<- function(trainD, testD, k){
  
  trainD<- dataPreprocessing(trainD)
  testD<- dataPreprocessing(testD)
  
  temp<- as.matrix(trainD[,1:8])
  temptest<- as.matrix(testD[,1:8])
  euDistBasedLabelMatrix<- matrix(rep(0), nrow = k, ncol = nrow(temptest))
  #euDistMatrix<- eu.dist1(train.df[,1:8],test.df[1,1:8])
  
  for(j in 1:nrow(temptest)){
    euDistBasedLabelMatrix[,j]<- euDistBasedMatrix(as.matrix(temp[,1:8]), matrix(temptest[j,1:8], nrow = nrow(temp), ncol = 8, byrow = TRUE),trainD[,9],k)
  }
  
  predList<- list()
  for(i in 1:ncol(euDistBasedLabelMatrix)){
    predList[i]<- names(which.max(prop.table(table(euDistBasedLabelMatrix[,i]))))
  }
  
  return(as.factor(unlist(predList)))
}