rm(list=ls())
#install.packages("AlgDesign")
library(AlgDesign)
#dat<-gen.factorial(levels=c(rep(2,7),rep(3,9)),nVars=16)
#desT<-optFederov(~.,dat,nTrials=144) 
n2l<-3
n3l<-3
dat<-gen.factorial(levels=c(rep(2,n2l),rep(3,n3l)),nVars=n2l+n3l, factors = "all")
desT<-optFederov(~.,dat,nTrials=36) 
deprof<-data.frame(desT$design)
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
for (i in (n2l+1):(n2l+n3l)){
  
  fitr[,i][which(fitr[,i]==1)]<- -1
  fitr[,i][which(fitr[,i]==2)]<- 0
  fitr[,i][which(fitr[,i]==3)]<- 1
}

for (i in 1:(length(fname)-1)){
  
  fitr<- cbind(fitr, deprof[,i] * deprof[,c((i+1):length(fname))] )
}
names(fitr)<-fnamef




#write.csv(fitr,"G:\\recover\\survey\\2019design\\testpro.csv")


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
