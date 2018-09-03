BaggingEnsemble <- function(dt.predict, nb.predict, knn.predict,rf.predict){
  predictionList<- list()
  for(indexCount in 1: length(dt.predict)){
    predictionList[indexCount] <- names(which.max(table(c(as.character(dt.predict[indexCount]),NB.predict[indexCount],as.character(knn.predict[indexCount]),as.character(rf.predict[indexCount])))))
  }
  return(as.factor(unlist(predictionList)))
}


BoostingEnsemble <- function(dt.predict, nb.predict, knn.predict, rf.predict, testD){
  dt.cm<- confusionMatrix(dt.predict,testD$nursery)
  nb.cm<- confusionMatrix(as.factor(nb.predict),testD$nursery)
  knn.cm<- confusionMatrix(knn.predict,testD$nursery)
  rf.cm<- confusionMatrix(rf.predict,testD$nursery)
  
  dt.accuracy <-as.numeric(dt.cm$overall["Accuracy"]*100)
  nb.accuracy <-as.numeric(nb.cm$overall["Accuracy"]*100)
  knn.accuracy <-as.numeric(knn.cm$overall["Accuracy"]*100)
  rf.accuracy <-as.numeric(rf.cm$overall["Accuracy"]*100)
  
  totalWeight <- sum(dt.accuracy,nb.accuracy,knn.accuracy,rf.accuracy)
  dtWeight <- dt.accuracy/totalWeight
  nbWeight <- nb.accuracy/totalWeight
  knnWeight <- knn.accuracy/totalWeight
  rfWeight <- rf.accuracy/totalWeight
  
  predDataFrame <- data.frame() 
  for(indexCount in 1: length(dt.predict)){
    not_recom = 0
    priority = 0
    spec_prior = 0
    very_recom= 0
    
    if(dt.predict[indexCount] == "not_recom") not_recom = not_recom + dtWeight
    else if(dt.predict[indexCount] == "priority") priority = priority + dtWeight
    else if(dt.predict[indexCount] == "spec_prior") spec_prior = spec_prior + dtWeight
    else if(dt.predict[indexCount] == "very_recom") very_recom = very_recom + dtWeight
    
    if(nb.predict[indexCount] == "not_recom") not_recom = not_recom + nbWeight
    else if(nb.predict[indexCount] == "priority") priority = priority + nbWeight
    else if(nb.predict[indexCount] == "spec_prior") spec_prior = spec_prior + nbWeight
    else if(nb.predict[indexCount] == "very_recom") very_recom = very_recom + nbWeight
    
    if(knn.predict[indexCount] == "not_recom") not_recom = not_recom + knnWeight
    else if(knn.predict[indexCount] == "priority") priority = priority + knnWeight
    else if(knn.predict[indexCount] == "spec_prior") spec_prior = spec_prior + knnWeight
    else if(knn.predict[indexCount] == "very_recom") very_recom = very_recom + knnWeight
    
    if(rf.predict[indexCount] == "not_recom") not_recom = not_recom + rfWeight
    else if(rf.predict[indexCount] == "priority") priority = priority + rfWeight
    else if(rf.predict[indexCount] == "spec_prior") spec_prior = spec_prior + rfWeight
    else if(rf.predict[indexCount] == "very_recom") very_recom = very_recom + rfWeight
    
    df<- data.frame(not_recom=not_recom,priority=priority,spec_prior=spec_prior,very_recom,very_recom)
    predDataFrame<- rbind(predDataFrame,df)
    
  }
  boostingPredication <- list()
  for(i in 1:nrow(predDataFrame)){
    boostingPredication[i] <- names(which.max(predDataFrame[i,]))
  }
  return(as.factor(unlist(boostingPredication)))
}
