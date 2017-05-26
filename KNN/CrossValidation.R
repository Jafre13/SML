#Cross Validation

acc<-c()

#for (i in 1:10){
startTime<-Sys.time()
print(i)
shuffleData(tmp)
#Run Knn
results<-knn(train = train,test=test,cl=trainLabels,k=1)
accu <- accuracy(results,testLabels)
acc <-c(acc,accuracy(testLabels,results))
endTime<-Sys.time()
time <-endTime-startTime
#}

print(accu)
print(time)
meanAcc <- mean(acc)
stdAcc <- sd(acc)
print(meanAcc)
print(stdAcc)


