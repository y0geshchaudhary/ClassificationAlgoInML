modelDT <- function(data){
  dt <- Node$new("Root")
  createDecisionTree(trainD,dt)
  return(dt)
}

informationRequired <- function(d){
  if(nrow(d)==0){return(0)}
  b <- prop.table(table(d$nursery))
  classes <- labels(b)
  df <- data.frame()
  for(class in classes[[1]]){
    new.df <- data.frame(as.numeric(b[class]))
    names(new.df)<-class
    if(length(df)==0){
      df<-new.df
    }
    else{
      df<-data.frame(df,new.df)
    }
  }
  rm("class","new.df","classes","b")
  
  entrop <- 0
  for(p in df){
    if(p==0 || p==1){
      entrop<- entrop
    }
    else{
      entrop<- entrop - p*log2(p)
    }
  }
  rm(p)
  return(entrop)
}

selectAttribute <- function(d){
  c<- colnames(d)
  columns <- c[1:length(c)-1]
  rm(c)
  info_gain_from_various_attribute <- data.frame()
  for(column in columns ){
    r<- prop.table(table(d[column]))
    sum<- 0
    values <- names(r)
    counter <- 1
    for(partValue in r){
      sum<- sum + as.numeric(partValue)*informationRequired(d[d[column]==values[counter],])
      counter<- counter+1
    }
    df<- data.frame(col1=sum)
    names(df)<-column
    if(length(info_gain_from_various_attribute)==0){
      info_gain_from_various_attribute<-df
    }
    else{
      info_gain_from_various_attribute<- data.frame(info_gain_from_various_attribute,df)
    }
  }
  ir<- informationRequired(d)
  ig <- sapply(info_gain_from_various_attribute, function(x){ ir-x })
  attrrib <- which.max(ig)
  return(names(attrrib))
}

haveSameClass <- function(d){
  length(unique(d[,ncol(d)])) == 1
}

createDecisionTree <- function(data,dt){
  if(haveSameClass(data)){
    dt$nodeName<- names(data)[ncol(data)]
    childNode <- dt$AddChild(unique(data[,ncol(data)]))
  }
  else{
    
    nodeName <- selectAttribute(data)
    dt$nodeName<-nodeName
    dt$maxClass <- names(which.max(prop.table(table(data$nursery))))
    childNodeData<- split(data[,names(data)!=nodeName, drop=FALSE], data[,nodeName], drop = TRUE)
    
    for(c in 1: length(childNodeData)){
      childNode <- dt$AddChild(names(childNodeData[c]))
      updatedChildNodeData <- childNodeData[c][[1]] 
      createDecisionTree(updatedChildNodeData,childNode)
    }
  }
}


predict <- function(dt, row) {
  if (dt$children[[1]]$isLeaf) return (dt$children[[1]]$name)
  child <- dt$children[[row[[dt$nodeName]]]]
  if(is.null(child)){
    return(dt$maxClass)
  }
  return ( predict(child, row))
}


predictDT<- function(dt, testingData){
  predictList <- list()
  index<-1
  for(row in 1:nrow(testingData)){
    f <- testingData[row,]
    f<- subset(f,select = -c(nursery))
    f<-as.matrix(f)
    colnames(f)<-NULL
    f<-c(parents=f[1],
         has_nurs=f[2],
         form=f[3],
         children=f[4],
         housing=f[5],
         finance=f[6],
         social=f[7],
         health=f[8])
    result<-predict(dt,f)
    predictList[index] <- predict(dt,f)
    index<- index+1
  }
  return(as.factor(unlist(predictList)))
}