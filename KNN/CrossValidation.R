#Cross Validation
trainsize<-2000
acc<-c()

for (i in 1:10){
  shuffleData(tmp)
  #Run Knn
  results<-knn(train = train,test=test,cl=trainLabels,k=3)
  acc <-c(acc,accuracy(testLabels,results))
}

meanAcc <- mean(acc)
stdAcc <- sd(acc)
print(meanAcc)
print(stdAcc)


