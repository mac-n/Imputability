library(randomForest)
comparereg<-function(imputed,original){
  
  tempvec<-vector(length=51)
  for(i in 2:(ncol(imputed)+1)){
    (templm<-lm(imputed[,i-1]~original[,i-1]))
    summary(templm)
    tempvec[i]<-summary(templm)$adj.r.squared
  }
  tempvec[1]<-mean(tempvec[2:ncol(imputed)+1])
  return(tempvec)
}
#Calculate overall imputability of each dataset (Column 4 of Table B-1)
thematrix<-matrix(nrow=20,ncol=51)
for (i in 1:20){
 thematrix[i,]<-comparereg(RFimputed[[i]],prcompdata[[i]])
}
#Generate Column 2 and 3  of Table B-1
#variance explained by first and second principal components 
varianceexplainedvector=vector(length=20)
varianceexplainedvector2=vector(length=20)
for (i in 1:20){
  temp<-princomp(prcompdata[[i]],center=TRUE,scale=TRUE,cor=TRUE)
  
  #temp<-nipals(prcompdata[[i]],ncomp=1)
  PoV<-temp$sdev^2/sum(temp$sdev^2)
  varianceexplainedvector[i]<-PoV[1]
  varianceexplainedvector2[i]<-PoV[2]
}
df<-data.frame(varianceexplainedvector,varianceexplainedvector2)

#Generate Column 4 and 5 of table B-1 (R2 and p-value of regression)
rsquaredvector=vector(length=20)
pvector=vector(length=20)
rfrsquared=vector(length=20)
for (i in 1:20){
  featureimputability<-scale(thematrix[i,2:51],center=TRUE)
  #pcomp<-princomp(prcompdata[[i]],cor=TRUE,center=TRUE,scale=TRUE)
  loadings<-npcomp[[i]]$loadings[,1]
  
  testthis<-data.frame(cbind(featureimputability,npcomp[[i]]$loadings[,1]))
  colnames(testthis)<-c("featureimputability","loadings")
  lm<-lm(featureimputability ~ .,data=testthis)
  
  rsquaredvector[i]<-summary(lm)$adj.r.sq
  pvector[i]<-lmp(lm)
  set.seed(i)

}

