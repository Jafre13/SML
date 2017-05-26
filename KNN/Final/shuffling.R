train = c()
test = c()
trainLabels = c()
testLabels = c()
t=c()

getTrainSize <- function(people,percent){
  return(floor(people*percent)*4000)
}

getPeople <- function(datasize) {
  return(datasize/4000)
}

AllShuffle <- function(dataset,train_p,single=FALSE){
  train = c()
  test = c()
  trainLabels = c()
  testLabels = c()
  
  
  datasize = NROW(dataset)
  if (single){
    trainSize = as.integer(nrow(dataset)*train_p)
  }
  else {
    trainSize = as.integer(getTrainSize(getPeople(datasize),train_p))
  }
 
  #Shuffle data set
  dataset_shuffle <- dataset[sample(nrow(dataset)),]
  #Extract classification labels
  labels<-dataset_shuffle$X1
  #Remove classification labels from dataset, to not use them in knn
  dataset_shuffle<-subset(dataset_shuffle,select = -c(X1))
  #Assign training and test data
  train <<- dataset_shuffle[1:trainSize,]
  trainLabels <<- labels[1:trainSize]
  test <<- dataset_shuffle[(trainSize+1):datasize,]
  testLabels <<- labels[(trainSize+1):datasize]
}

PersonShuffle <- function(dataset,train_p){
  train = c()
  test = c()
  trainLabels = c()
  testLabels = c()

  datasize = NROW(dataset)
  trainSize = as.integer(getTrainSize(getPeople(datasize),train_p))
  labels = dataset$X1
  dataset<-subset(dataset,select = -c(X1))
  people = getPeople(trainSize)
  listofall =c(1:getPeople(NROW(dataset)))
  listofpeeps = sample(1:getPeople(NROW(dataset)),people,replace=F)
  listoftest = setdiff(listofall,listofpeeps)
  for (i in 1:length(listofall)){
    one = listofall[i]
    low = (one-1)*4000+1
    high = one*4000
    tmp <- dataset[low:high,]
    if (i %in% listofpeeps){
      train = rbind(train, tmp)
      trainLabels = c(trainLabels,labels[low:high])
    }
    else {
      test = rbind(test,tmp)
      testLabels = c(testLabels,labels[low:high])
    }
  }
  train <<- train
  test <<- test
  trainLabels <<- trainLabels
  testLabels <<- testLabels
  
  
}

