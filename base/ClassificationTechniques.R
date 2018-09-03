library(readr) # Reading data file
library(data.tree) # for decision tree data structure
library(caret) # for confusion matrix
library(Matrix) # for matrix operations
library(quadprog) # for SVM

source("customAlgorithms/KNN.R")
source("customAlgorithms/NaiveBayes.R")
source("customAlgorithms/DecisionTree.R")
source("customAlgorithms/RandomForest.R")
source("customAlgorithms/Ensembler.R")
source("customAlgorithms/SVM.R")

data<- read.csv("dataset/nursery.csv",strip.white = TRUE)

set.seed(2)
datasets <- sample(2,nrow(data), replace = TRUE, prob = c(0.70,0.30))
trainD <- data[datasets==1,]
testD <- data[datasets==2,]


NB.model <- modelNB(trainD,"nursery")
NB.predict <- predictNB(NB.model,testD)
confusionMatrix(as.factor(NB.predict),testD$nursery)


DT.model <- modelDT(trainD)
DT.predict <- predictDT(DT.model,testD)
confusionMatrix(DT.predict,testD$nursery)


modelRF(trainD,5)
RF.predict <- predictRF(testD,5)
confusionMatrix(RF.predict,testD$nursery)


KNN.predict <- knn.prediction(trainD, testD,5)
confusionMatrix(KNN.predict,as.factor(testD$nursery))


Bagging.predict<- BaggingEnsemble(DT.predict,NB.predict,KNN.predict,RF.predict)
confusionMatrix(Bagging.predict,testD$nursery)


Boosting.predict<- BoostingEnsemble(DT.predict,NB.predict,KNN.predict,RF.predict, testD)
confusionMatrix(Boosting.predict,testD$nursery)


SVM.model<- modelSVM(trainD)
SVM.predict<- predictSVM(testD,SVM.model)
confusionMatrix(SVM.predict, testD$nursery)
