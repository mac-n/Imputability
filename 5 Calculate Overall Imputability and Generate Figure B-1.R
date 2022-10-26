varianceexplainedmatrix=matrix(nrow=20,ncol=50)
for (i in 1:20){
  temp<-princomp(prcompdata[[i]],center=TRUE,scale=TRUE,cor=TRUE)
  
  #temp<-nipals(prcompdata[[i]],ncomp=1)
  PoV<-temp$sdev^2/sum(temp$sdev^2)
  varianceexplainedmatrix[i,]<-PoV[1:50]
}
overallimputability<-scale(thematrix[,1],center=TRUE)
testthis<-data.frame(cbind(overallimputability,varianceexplainedmatrix[,1:10]))
colnames(testthis)[1]<-"overallimputability"
l<-lm(formula = overallimputability ~ ., data = testthis[1:20,])
p<-predict(l,newdata=testthis)
summary(l)
library(ggplot2)

d<-data.frame(p,overallimputability)

ggplot(d,aes(x=p,y=overallimputability))+geom_point()+theme_bw()

d2<-data.frame(thematrix[,1],rowSums(testthis[,2:11]))
  colnames(d2)<-c("overall","varianceexplained10")
l<-lm(formula = overallimputability ~ varianceexplained10, data = d2)

ggplot(d,aes(x=varianceexplained10,y=overallimputability))+geom_point()+theme_bw()
