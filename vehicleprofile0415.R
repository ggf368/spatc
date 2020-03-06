rm(list=ls())
vehn<-read.table("C:\\0mywork\\survey\\2019design\\assignveh.csv", header = TRUE, sep = ",")
veh<-vehn

#names(veh)
rvppr<-0
rvppr[vehn$rvpp==1]<-15000
rvppr[vehn$rvpp==2]<-30000 
rvppr[vehn$rvpp==3]<-45000

evppr<-0
evppr[vehn$evpp==1]<-0.1
evppr[vehn$evpp==2]<-0.2
evppr[vehn$evpp==3]<-0.3

evppc<-rvppr*(evppr+1)


veh$rvpp[vehn$rvpp==1]<-"15.000 euro"
veh$rvpp[vehn$rvpp==2]<-"30.000 euro"
veh$rvpp[vehn$rvpp==3]<-"45.000 euro"

veh$rvmc[vehn$rvmc==1]<-"100"
veh$rvmc[vehn$rvmc==2]<-"200"

veh$rvoc[vehn$rvoc==1]<-"0,10"
veh$rvoc[vehn$rvoc==2]<-"0,15"

veh$evpp<-paste(as.character(prettyNum(evppc,big.mark = ".") ),"euro")

veh$evmc[vehn$evmc==1]<-"100"
veh$evmc[vehn$evmc==2]<-"200"

veh$evoc[vehn$evoc==1]<-"0,04"
veh$evoc[vehn$evoc==2]<-"0,08"

veh$evtr[vehn$evtr==1]<-"5%"
veh$evtr[vehn$evtr==2]<-"10%"
veh$evtr[vehn$evtr==3]<-"15%"

veh$evdr[vehn$evdr==1]<-"150"
veh$evdr[vehn$evdr==2]<-"300"
veh$evdr[vehn$evdr==3]<-"450"

veh$evsc[vehn$evsc==1]<-"1,5"
veh$evsc[vehn$evsc==2]<-"3,0"

veh$evfc[vehn$evfc==1]<-"5"
veh$evfc[vehn$evfc==2]<-"10"
veh$evfc[vehn$evfc==3]<-"15"

veh$evtc[vehn$evtc==1]<-"5"
veh$evtc[vehn$evtc==2]<-"10"
veh$evtc[vehn$evtc==3]<-"15"

veh$shmc[vehn$shmc==1]<-"0"
veh$shmc[vehn$shmc==2]<-"10"
veh$shmc[vehn$shmc==3]<-"20"

veh$shoc[vehn$shoc==1]<-"0,2"
veh$shoc[vehn$shoc==2]<-"0,4"

veh$shhr[vehn$shhr==1]<-"3"
veh$shhr[vehn$shhr==2]<-"6"

veh$shat[vehn$shat==1]<-"5"
veh$shat[vehn$shat==2]<-"10"
veh$shat[vehn$shat==3]<-"15"

veh$shav[vehn$shav==1]<-"60%"
veh$shav[vehn$shav==2]<-"80%"
veh$shav[vehn$shav==3]<-"100%"



write.csv(veh,"C:\\0mywork\\survey\\2019design\\vehicleprofile.csv")


#TEST BALANCE
vehh<-veh[,-1]
joinvar<-vehh
for (i in 1:ncol(vehh)){
  for (j in 1:ncol(vehh)){
    joinvar<-cbind(joinvar,paste(vehh[,i],vehh[,j], sep = "_"))
  }
  
  
}

for (i in 1:ncol(joinvar)){
  print(table(joinvar[,i]))
}



