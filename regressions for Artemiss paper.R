


RFaccuracies5<-RFmatrix[5,2:13]

library(nipals)
data<-basedataset[,colnames(dataset[[1]])]
df<-data[,which(sapply(data,is.numeric))]
dm<-data.matrix(df)
df<-scale(df,center=TRUE)
nip<-nipals(df,ncomp=1)
summary(lm(nip$loadings[,1]~RFaccuracies5))
p2@loadings[,1]

data<-dataset[[5]]
df<-data[,which(sapply(data,is.numeric))]
nip<-nipals(df)
summary(lm(nip$loadings[,1]~RFaccuracies5
