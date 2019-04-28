# ClassificationAlgoInML
This is a project where I work on various classification algorithm from scratch and compared their accuracy with others. Also I have used techniques like bagging and boosting to improve the accuracy. In SVM we used binary classifier to create a multinomial classifier.


- I have used below libraries

library(readr) # Reading data file

library(data.tree) # for decision tree data structure

library(caret) # for confusion matrix and sample creation from data

library(Matrix) # for matrix operations

library(quadprog) # for SVm quadratic equation


#- We need to load the project in the RStudio as follows:
Go to File in RStudio > open project..> Navigate to "ClassificationAlgoInML" and open it > click on "ClassificationAlgoInML.Rproj" and open it. 


#-Directory structure in ClassificationAlgoInML folder

ClassificationAlgoInML

.RData #RStudio related file

base

	ClassificationTechniques.R #Consolidated file from where all the algorithms are executed 

customAlgorithms

	DecisionTree.R #Decision tree algorithm code

	KNN.R	#KNN algorithm code

	NaiveBayes.R	##NaiveBayes algorithm code

	RandomForest.R	#Random Forest algorithm code

	SVM.R	#SVM algorithm code

	Ensembler.R #Boosting and Bagging technique code

dataset

	nursery.csv	#nursery dataset
	
ClassificationAlgoInML.Rproj	#RStudio .project file

readme.txt	#readme.txt for this project


#- to execute algorithm
	
"ClassificationTechniques.R" is the main file from where all the algorithms are loaded and executed.


1. to load the algorithm code/functions in RStudio, execute following lines of code on ClassificationTechniques.R file.

source("customAlgorithms/KNN.R")

source("customAlgorithms/NaiveBayes.R")

source("customAlgorithms/DecisionTree.R")

source("customAlgorithms/RandomForest.R")

source("customAlgorithms/Ensembler.R")

source("customAlgorithms/SVM.R")



2. to load the data, execute following lines of code on ClassificationTechniques.R file.

data<- read.csv("dataset/nursery.csv",strip.white = TRUE)


3. to partition data in training and testing sets, execute following lines of code on ClassificationTechniques.R file.

set.seed(2)

datasets <- sample(2,nrow(data), replace = TRUE, prob = c(0.70,0.30))

trainD <- data[datasets==1,]

testD <- data[datasets==2,]


4. to create Naive Bayes model, do prediction on testing set and create confusion matrix, execute following lines of code on 

ClassificationTechniques.R file.

NB.model <- modelNB(trainD,"nursery")

NB.predict <- predictNB(NB.model,testD)

confusionMatrix(as.factor(NB.predict),testD$nursery)


5. to create Decision Tree model, do prediction on testing set and create confusion matrix, execute following lines of code on 

ClassificationTechniques.R file.

DT.model <- modelDT(trainD)

DT.predict <- predictDT(DT.model,testD)

confusionMatrix(DT.predict,testD$nursery)


6. to create Random Forest model, do prediction on testing set and create confusion matrix, execute following lines of code on 

ClassificationTechniques.R file.

modelRF(trainD,5)

RF.predict <- predictRF(testD,5)

confusionMatrix(RF.predict,testD$nursery)


7. to create KNN model, do prediction on testing set and create confusion matrix, execute following lines of code on 

ClassificationTechniques.R file.

KNN.predict <- knn.prediction(trainD, testD,5)

confusionMatrix(KNN.predict,as.factor(testD$nursery))


8. to use Bagging for prediction on testing set and create confusion matrix, execute following lines of code on ClassificationTechniques.R file.

Boosting.predict<- BoostingEnsemble(DT.predict,NB.predict,KNN.predict,RF.predict, testD)

confusionMatrix(Boosting.predict,testD$nursery)


9. to use Boosting for prediction on testing set and create confusion matrix, execute following lines of code on ClassificationTechniques.R file.

Bagging.predict<- BaggingEnsemble(DT.predict,NB.predict,KNN.predict,RF.predict)

confusionMatrix(Bagging.predict,testD$nursery)


10. to create SVM model, do prediction on testing set and create confusion matrix, execute following lines of code on 

ClassificationTechniques.R file.

SVM.model<- modelSVM(trainD)

SVM.predict<- predictSVM(testD,SVM.model)

confusionMatrix(SVM.predict, testD$nursery)

