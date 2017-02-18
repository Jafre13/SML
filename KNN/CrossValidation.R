#Cross Validation

acc<-c()

for (i in 1:10){
  print(i)
  shuffleData(dataset)
  #Run Knn
  results<-knn(train = train,test=test,cl=trainLabels,k=1)
  acc <-c(acc,accuracy(testLabels,results))
}

meanAcc <- mean(acc)
stdAcc <- sd(acc)
print(meanAcc)
print(stdAcc)


