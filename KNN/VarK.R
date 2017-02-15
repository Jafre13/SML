library("ggplot2")
library("class")
k <- seq(1,100,by = 2)

acc<-c()
time<-c()

shuffleData(tmp)
for (i in k){
  startTime <- Sys.time()
  results<-knn(train = train,test=test,cl=trainLabels,k=i)
  acc <-c(acc,accuracy(testLabels,results))
  endTime <- Sys.time()
  time <- c(time,(endTime-startTime))
}

qplot(k,acc,geom = "line")
qplot(k,time,geom = "line")