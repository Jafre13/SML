library("ggplot2")

#Accuracy Function
accuracy <- function(results, testlabels){
  count <- 0
  for (i in 1:length(results)){
    if (results[i] == testlabels[i]){
      count<-count+1
    }
  }
  acc <- (count/length(results))*100
  return(acc);
  
}


shuffleData <- function(tmp){
  #Shuffle data set
  dataset_shuffle <- tmp[sample(nrow(tmp)),]
  
  #Extract classification labels
  labels<-dataset_shuffle$X1
  
  #Remove classification labels from dataset, to not use them in knn
  dataset_shuffle<-subset(dataset_shuffle,select = -c(X1))
  
  
  #Assign training and test data
  train <- dataset_shuffle[1:trainsize,]
  trainLabels <- labels[1:trainsize]
  test <- dataset_shuffle[(trainsize+1):4000,]
  testLabels <- labels[(trainsize+1):4000]
}
#Set seet and train size

#Load data from images
tmp<-loadSinglePersonsData(100,"group12","1","C:/SML/2017/")
#Convert to dataframe
tmp<-data.frame(tmp)


