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

#Set seet and train size
set.seed(1)
trainsize<-3600

#Load data from images
tmp<-loadSinglePersonsData(100,"group10","1","C:/SML/2017/")
#Convert to dataframe
tmp<-data.frame(tmp)

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

#Run Knn
results<-knn(train = train,test=test,cl=trainLabels,k=25)
print(accuracy(results,testLabels))
