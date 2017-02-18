library("ggplot2")
library("class")
k <- seq(1,10,by = 2)

acc<-c()
time<-c()


totalacc <- c()
totaltime  <- c()
totalsd <- c()
totalmean  <- c()


for (i in k){
  print(paste0("k ",i))
  kacc<-c()
  kmean <- c()
  kstd <- c()
  ktime <- c()
  for (j in 1:1){
    print(paste0("fold ",j))
    startTime <- Sys.time()
    shuffleData(tmp)
    #Run Knn
    results<-knn(train = train,test=test,cl=trainLabels,k=i)
    endTime <- Sys.time()
    
    kacc <-c(kacc,accuracy(testLabels,results))
    ktime <-c(ktime,(endTime-startTime))
    }
  
  totalmean <- c(totalmean,mean(kacc))
  totalsd <- c(totalsd,sd(kacc))
  totaltime <- c(totaltime,mean(ktime))
  }

qplot(k,totalmean)+geom_smooth(method="glm")+geom_errorbar(aes(x=k, ymin=totalmean-totalsd, ymax=totalmean+totalsd), width=0.1, alpha= I(1/2))

qplot(k,totaltime)+geom_smooth(method="gml")
