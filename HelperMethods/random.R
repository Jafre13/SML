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