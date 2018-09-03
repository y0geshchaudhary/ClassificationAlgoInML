
modelNB <- function(data,classColumn) {
  c <- data[,classColumn]
  classes <- levels(c)
  noOfClasses <- length(classes)
  n <- nrow(data)
  attribute <- data[,1:8]
  priorsprob <- setNames(numeric(noOfClasses), classes)
  likelihoodsprob <- vector("list", noOfClasses)
  
  for (a in classes) {
    Subattribute <- attribute[c == a,]
    priorsprob[a] <- nrow(Subattribute) / n
    likelihoodsprob[[a]] <- lapply(Subattribute, function(x) {
      temp <- prop.table(table(x))
      temp <- setNames(as.numeric(temp), names(temp))
      ifelse(temp == 0, 0.001, temp)
    })
  }
  list(classes=classes, likelihoodsprob=likelihoodsprob, classColumn=classColumn, priorsprob=priorsprob)
}

predictNB <- function(model,newdata) {
  n <- nrow(newdata)
  #labels <- levels(newdata[, model$classColumn])
  newdata[, model$classColumn] = NULL
  classes <- model$classes
  pred = matrix(NA, nrow=n, ncol=length(classes))
  colnames(pred) = classes
  for (i in 1:nrow(newdata)) {
    x <- sapply(newdata[i,], as.character)
    for (class in classes) {
      p <- model$likelihoodsprob[[class]]
      q <- sapply(names(x), function(n) p[[n]][x[n]])
      pred[i, class] <- prod(q, na.rm=TRUE) * model$priorsprob[[class]]
    }
  }
  
  predList<- list()
  for(row in 1:nrow(pred)){
    predList[row]<-names(which.max(pred[row,]))
  }
  predList<- unlist(predList)
  return(predList)
}