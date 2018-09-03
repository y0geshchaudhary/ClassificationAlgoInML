source("customAlgorithms/DecisionTree.R")

modelRF<- function(trainD,k=3){
  datasets<- sample(k,nrow(trainD), replace=TRUE)
  for(i in 1:k){
    trainingData<- trainD[datasets==i,]
    model<- modelDT(trainingData)
    saveRDS(model,paste("model",i))
  }
}

predictRF<- function(testD, k=3){
  
  modelsPred<- data.frame()
  for(i in 1:k){
    model<- readRDS(paste("model",i))
    pred<- predictDT(model,testD)
    file.remove(paste("model",i))
    temp<- data.frame(pred)
    names(temp)<- paste("model",i)
    if(length(modelsPred)==0){
      modelsPred<-temp
    }else{
      modelsPred<- cbind(modelsPred,temp)
    }
  }
  predList<- list()
  for(i in 1:nrow(modelsPred)){
    predList[i]<- names(which.max(table(as.matrix(modelsPred[i,]))))
  }
  return(as.factor(unlist(predList)))
}
