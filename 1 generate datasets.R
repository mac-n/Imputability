prcompdata60<-prcompdata
#prcompdata<-sapply(prcompdata,as.data.frame)
for (i in 1:20){
  cutdataset<-prcompdata[[i]]
  #for each column in this row set its value to NA with probability 
  for (j in 1:ncol(cutdataset)){
    set.seed(j+i)
    prcompdata60[[i]][j] = ifelse(as.logical(rbinom(nrow(cutdataset), size=1, prob=0.6)), NA, prcompdata[[i]][,j])
  }
}

