
getAllData <- function(dataList){
  id <- data.frame()
  alldata <- list()
  for(i in 1:length(dataList))
  {
    if( length(dataList[[i]]) > 0  ){
      for(j in dataList[[i]]){
        print(j)
        idTemp <- loadSinglePersonsData(100,i - 1,j,"C:/Code/SMLIMAGES/preProcessed/2017/group")
        alldata <- append(alldata, list(idTemp))
      }
    }
  }
  return(alldata)
}

personList <- list( list(1,2,3),      #Group 0
                    list(1,2,3),        #Group 1
                    list(1,2,3),        #Group 2
                    list(1,2,3,4),      #Group 3
                    list(0,2,3),        #Group 4
                    list(1,2,3),        #Group 5
                    list(1,2,3),        #Group 6
                    list(1,2),          #Group 7
                    list(),             #Group 8
                    list(1),            #Group 9
                    list(1),            #Group 10
                    list(1,2),        #Group 11
                    list(1)             #Group 12
)



SingleData = data.frame(loadSinglePersonsData(100,"group10","1","C:/Code/SMLIMAGES/preProcessed/2017/"))

AllData = getAllData(personList)
AllDataset = AllData[1]
AllDataset = data.frame(AllDataset)
for(i in 2:length(AllData)){
  idTemp <- AllData[i]
  idTemp <- data.frame(idTemp)
  AllDataset <- rbind(AllDataset, idTemp)
}
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