library("ggplot2")
library("class")
k <- seq(1,1,by = 2)

acc<-c()
time<-c()

lab1 <- dataset1$X1
print("lab1")
dataset1<<-subset(dataset1,select = -c(X1))
lab2 <- dataset2$X1
print("lab2")
dataset2<<-subset(dataset2,select = -c(X1))

totalacc <- c()
totaltime  <- c()
totalsd <- c()
totalmean  <- c()
totaltimesd <-c()

for (i in k){
  print(paste0("k ",i))
  kacc<-c()
  kmean <- c()
  kstd <- c()
  ktime <- c()
  for (j in 1:1){
    print(paste0("fold ",j))
    startTime <- Sys.time()
    #shuffleData(dataset)
    #Run Knn
    
    results<-knn(train = dataset1,test=dataset2,cl=lab1,k=i)
    endTime <- Sys.time()
    
    kacc <-c(kacc,accuracy(lab2,results))
    ktime <-c(ktime,(endTime-startTime))
    print(accuracy(lab2,results))
    print(endTime-startTime)
    }
  
  totalmean <- c(totalmean,mean(kacc))
  totalsd <- c(totalsd,sd(kacc))
  totaltime <- c(totaltime,mean(ktime))
  totaltimesd <- c(totaltimesd,sd(ktime))
  }

qplot(k,totalmean,ylab ="Accuracy in %" )+geom_smooth(method="glm")+geom_errorbar(aes(x=k, ymin=totalmean-totalsd, ymax=totalmean+totalsd), width=0.1, alpha= I(1/2))

qplot(k,totaltime,ylab="Time in seconds")+geom_smooth(method="gml")+geom_errorbar(aes(x=k, ymin=totaltime-totaltimesd, ymax=totaltime+totaltimesd), width=0.1, alpha= I(1/2))
