ks = seq(1,20,by = 3)
print(ks)
kAccuracy = c()
kAccStd = c()
kTime = c()
kTimeStd = c()
for (k in ks){
  AllShuffle(AllDataset,0.7,single = TRUE)
  acc = c()
  time = c()
  for (i in 1:2){
    start = Sys.time()
    results = knn(train,test,trainLabels,k)
    end = Sys.time()
    t = end-start
    acc = c(acc,accuracy(results,testLabels))
    time = c(time,t)
    print(k)
  }
  kAccuracy = c(kAccuracy,mean(acc))
  kAccStd = c(kAccStd,sd(acc))
  kTime = c(kTime,mean(time))
  kTimeStd = c(kTimeStd,sd(time))
  
}

VarKAccPlot <- {qplot(ks,kAccuracy,xlab = "k",ylab = "Accuracy in %",geom = "line",main = "Accuracy according to varying k for all persons pooled")+
    geom_errorbar(aes(x=ks, ymin=kAccuracy-kAccStd, ymax=kAccuracy+kAccStd), width=0.5, alpha= I(1/2))
}
ggsave("Final/pooledAccVarK.png")

{
  varKAccPlottimePlot <-  qplot(ks,kTime,xlab = "k",ylab = "Time in Seconds",main = "Time according to varying k for all persons pooled")+
    geom_errorbar(aes(x=ks, ymin=kTime-kTimeStd, ymax=kTime+kTimeStd), width=0.5, alpha= I(1/2))
}
ggsave("Final/pooledTimeVarK.png")



