require(missForest)
RFimputed<-list()
forests<-list()
ptmforest<-proc.time()
for (i in 1:20){
  temp<- prcompdata60[[i]]
  temp<-data.matrix(temp)
  set.seed(i)
  temp<-missForest(temp)
  RFimputed[[i]]<-temp$ximp
  forests[[i]]<-temp
}
ptmforest<-proc.time()-ptmforest