rm(list=ls())
deprof<-read.table("C:\\0mywork\\survey\\2019design\\assignveh.csv", header = TRUE, sep = ",")
fname<-names(deprof)[-1]
fnamef<-fname
for (i in 1:(length(fname)-1)){
  
  
  fnamef<-c(fnamef,paste(fname[i],fname[(i+1):length(fname)]))
}

fitr<-deprof[,-1]
for (i in 1:16){
  if (max(fitr[,i])==2){
  fitr[,i][which(fitr[,i]==1)]<- -1
  fitr[,i][which(fitr[,i]==2)]<- 1
  } else if (max(fitr[,i])==3){
    fitr[,i][which(fitr[,i]==1)]<- -1
    fitr[,i][which(fitr[,i]==2)]<- 0
    fitr[,i][which(fitr[,i]==3)]<- 1
  }
}
fitr1<-fitr
for (i in 1:(length(fname)-1)){
  
  fitr<- cbind(fitr, fitr1[,i] * fitr1[,c((i+1):length(fname))] )
}
names(fitr)<-fnamef
write.csv(fitr,"C:\\0mywork\\survey\\2019design\\vehicleassign0413.csv")