rm(list=ls())
#install.packages("AlgDesign")
library(AlgDesign)
#dat<-gen.factorial(levels=c(rep(2,7),rep(3,9)),nVars=16)
#desT<-optFederov(~.,dat,nTrials=144) 
n2l<-4
n4l<-10

dat<-gen.factorial(levels=c(rep(2,n2l),rep(4,n4l)),nVars=n2l+n4l, factors = "all")
desT<-optFederov(~.,dat,nTrials=64) 
deprof<-data.frame(desT$design)
tertdesign<-desT$design
#as.numeric(unlist(deprof))
#interaction 
deprof<-data.frame(matrix(as.numeric(unlist(deprof)),nrow=nrow(deprof)))
fname<-names(desT$design)
fnamef<-fname
for (i in 1:(length(fname)-1)){
  
  
  fnamef<-c(fnamef,paste(fname[i],fname[(i+1):length(fname)]))
}

fitr<-deprof
for (i in 1:n2l){
  
  fitr[,i][which(fitr[,i]==1)]<- -1
  fitr[,i][which(fitr[,i]==2)]<- 1
}
for (i in (n2l+1):(n2l+n4l)){
  
  fitr[,i][which(fitr[,i]==1)]<- -3
  fitr[,i][which(fitr[,i]==2)]<- -1
  fitr[,i][which(fitr[,i]==3)]<- 1
  fitr[,i][which(fitr[,i]==4)]<- 3
}
fitr1<-fitr
for (i in 1:(length(fname)-1)){
  
  fitr<- cbind(fitr, fitr1[,i] * fitr1[,c((i+1):length(fname))] )
}
names(fitr)<-fnamef




write.csv(fitr,"C:\\0mywork\\TERTS\\interactioncheck.csv")


#TEST BALANCE
joinvar<-deprof
for (i in 1:ncol(deprof)){
  for (j in 1:ncol(deprof)){
    joinvar<-cbind(joinvar,paste(deprof[,i],deprof[,j], sep = "_"))
  }
  
  
}

for (i in 1:ncol(joinvar)){
  print(table(joinvar[,i]))
}
