#refer project report for reference
calculateW = function(data, label, C=NULL) {
  n = nrow(data)
  distanceMatrix = matrix(rep(0, n*n), nrow=n)
  for (i in 1:n){
    for (j in 1:n){
      distanceMatrix[i,j] = data[i,] %*% data[j,]
    }
  }
  #parameters for solve.QP
  bVector=c(0, rep(0, n), rep(-C, n))
  dVector=rep(1, n)
  aMatrix=t(rbind(label, diag(n), -1*diag(n)))
  dMatrix = as.matrix(nearPD(outer(label,label) * distanceMatrix)$mat) 
  solveQPResult = solve.QP(dMatrix,dVector,aMatrix,bvec=bVector, meq=1)
  r = solveQPResult$solution 
  w = apply(r*label*data,2,sum)
  return(w)
}

center = function(z) (z-mean(z))/sd(z)

buildModel<- function(training){
  M = as.matrix(training[,1:8])
  for(j in 1:8) M[,j] = center(M[,j])
  W = calculateW(cbind(1,M),training$nursery*2-1,C=.5)
  return(W)
}

pred<- function(model,testing){
  M = as.matrix(testing[,1:8])
  for(j in 1:8) M[,j] = center(M[,j])
  y_pred = 2*((cbind(1,M)%*%model)>0)-1
  return(y_pred)
}


modelSVM<- function(trainingData){
  #preprocessing
  indx <- sapply(trainingData, is.factor)
  indx[names(indx[length(indx)])]<-FALSE
  trainingData[indx] <- lapply(trainingData[indx], function(x) as.numeric(x))
  rm(indx)
  classs=levels(trainingData$nursery)
  
  trainDataSamples<-sample(3,nrow(trainingData), replace = TRUE, prob = c(0.33,0.33,0.34))
  train1<- trainingData[trainDataSamples==1,]
  train2<- trainingData[trainDataSamples==2,]
  train3<- trainingData[trainDataSamples==3,]
  trList<- list(train1,train2,train3)
  modelDataFrame<- data.frame()
  modelList<- list()
  for(c in classs){
    
    #for(i in 1:3){
    trD1<-as.data.frame(trList[1])
    trD1$nursery = (trD1$nursery==c)*1
    trD1$nursery = 2*trD1$nursery-1
    model1<-buildModel(trD1)
    
    trD2<-as.data.frame(trList[2])
    trD2$nursery = (trD2$nursery==c)*1
    trD2$nursery = 2*trD2$nursery-1
    model2<-buildModel(trD2)
    
    trD3<-as.data.frame(trList[3])
    trD3$nursery = (trD3$nursery==c)*1
    trD3$nursery = 2*trD3$nursery-1
    model3<-buildModel(trD3)
    tempDataFrame<- data.frame(model1=model1,model2=model2,model3=model3)
    #}
    tempList<- list(tempDataFrame)
    names(tempList)<- c
    if(length(modelList)==0){
      modelList<- tempList
    } else {
      modelList<- c(modelList,tempList)
    }
  }
  return(modelList)
}


predictSVM<- function(testingData, model){
  
  indx <- sapply(testingData, is.factor)
  indx[names(indx[length(indx)])]<-FALSE
  testingData[indx] <- lapply(testingData[indx], function(x) as.numeric(x))
  rm(indx)
  
  M = as.matrix(testingData[,1:8])
  for(j in 1:8) M[,j] = center(M[,j])
  
  classs<- levels(testingData$nursery)
  overAllPredDF<- data.frame()
  for(c in classs){
    modelList<- model[c]
    model1<- modelList[[c]]$model1
    model2<- modelList[[c]]$model2
    model3<- modelList[[c]]$model3
    
    pred1 = cbind(1,M)%*%model1
    pred2 = cbind(1,M)%*%model2
    pred3 = cbind(1,M)%*%model3
    
    predictionList<- list()
    for(i in 1:nrow(pred1)){
      temp<- c(pred1[i,],pred2[i,],pred3[i,])
      predictionList[i]<- as.numeric(temp[which.max(temp)])
    }
    tempPredDF<- data.frame(unlist(predictionList))
    names(tempPredDF)<- c
    if(length(overAllPredDF)==0){
      overAllPredDF<- tempPredDF
    } else {
      overAllPredDF<- data.frame(overAllPredDF, tempPredDF)
    }
  }
  preedict<- list()
  for(i in 1: nrow(overAllPredDF)){
    preedict[i]<- names(which.max(overAllPredDF[i,]))  
  }
  return(as.factor(unlist(preedict)))
}
