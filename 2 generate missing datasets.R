prcompdata<-list()
Data5<-data.frame(Data5)
Data4<-data.frame(Data4)
Data2<-data.frame(Data2)
Data1<-data.frame(Data1)
Data3<-data.frame(Data3)
for (i in 1:4){
  prcompdata[[i]]<-Data5[,(3+(i-1)*50):(2+(i*50))]
  print(summary(princomp(prcompdata[[i]])))
}

for (i in 1:4){
  prcompdata[[i+4]]<-Data1[,(3+(i-1)*50):(2+(i*50))]
  print(summary(princomp(prcompdata[[i]])))
}

for (i in 1:4){
  prcompdata[[i+8]]<-Data2[,(3+(i-1)*50):(2+(i*50))]
  print(summary(princomp(prcompdata[[i]])))
}
for (i in 1:4){
  prcompdata[[i+12]]<-Data4[,(3+(i-1)*50):(2+(i*50))]
  print(summary(princomp(prcompdata[[i]])))
}
for (i in 1:4){
  prcompdata[[i+16]]<-Data3[,(3+(i-1)*50):(2+(i*50))]
  print(summary(princomp(prcompdata[[i]])))
}