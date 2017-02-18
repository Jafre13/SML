library("ggplot2")
library("EBImage")


#Get All Data

getAllData <- function(dataList){
  id <- data.frame()
  alldata <- list()
  for(i in 1:length(dataList))
  {
    if( length(dataList[[i]]) > 0  ){
      for(j in 1:length(dataList[[i]])){
        idTemp <- loadSinglePersonsData(100,i - 1,j,folder)
        alldata <- append(alldata, list(idTemp))
      }
    }
  }
  return(alldata)
}

# Set a "list of list" for each group and member and run
folder <- "~/trunk/2017/group"

dataList <- list( list(1,2,3),
                  list(1,2,3),
                  list(1,2,3),
                  list(1,2,3,4)
                  #list(),
                  #list(1),
                  #list(),
                  #list(1,2),
                  #list(1,2),
                  #list(),
                  #list(1),
                  #list(),
                  #list(1)
                  )


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
smoothImage <- function(grayImg){
  kern = makeBrush(5,shape="Gaussian")
  smooth = filter2(grayImg,kern)
  return(smooth)  
  }

labels <- c()
train <-c()
test<-c()
testLabels <- c()
trainLabels <- c()


shuffleData <- function(somedata){
  #Shuffle data set
  dataset_shuffle <<- somedata[sample(nrow(somedata)),]
  
  #Extract classification labels
  labels<<-dataset_shuffle$X1
  
  #Remove classification labels from dataset, to not use them in knn
  dataset_shuffle<<-subset(dataset_shuffle,select = -c(X1))
  
  
  #Assign training and test data
  train <<- dataset_shuffle[1:trainSize,]
  trainLabels <<- labels[1:trainSize]
  test <<- dataset_shuffle[(trainSize+1):datasize,]
  testLabels <<- labels[(trainSize+1):datasize]
}
#Set seet and train size

#Load data from images
tmp<-loadSinglePersonsData(100,"group12","1","~/trunk/2017/")
#Convert to dataframe
tmp<-data.frame(tmp)
#getSize
alldata <- getAllData(dataList)
# You can now iterate trough the list
dataset <- alldata[1]
dataset <- data.frame(dataset)

for(i in 2:length(alldata)){
  idTemp <- alldata[i]
  idTemp <- data.frame(idTemp)
  dataset <- rbind(dataset, idTemp)
}

datasize<-NROW(tmp)
trainSize <- as.integer(datasize*0.9)

