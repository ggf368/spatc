library(maxLik)

rm(list=ls())

ve<-read.table("C:\\0mywork\\survey\\2019design\\vehicleprofile1.csv", header = TRUE, sep = ",")

attach(ve)
ve$evpp_low<-0
ve$evpp_mid<-0
ve$evpp_low[(ve$evpp/ve$rvpp-1)== "0.1"]<- 1
ve$evpp_mid[(ve$evpp/ve$rvpp-1)== "0.2"]<- 1
ve$evpp_low[(ve$evpp/ve$rvpp-1)== "0.3"]<--1
ve$evpp_mid[(ve$evpp/ve$rvpp-1)== "0.3"]<--1


ve$evmc_low<-1
ve$evmc_low[evmc == 200]<- -1

ve$evoc_low<-1
ve$evoc_low[evoc == "0,08"]<- -1

ve$evtr_low<-0
ve$evtr_mid<-0
ve$evtr_low[evtr== "5%"] <- 1
ve$evtr_mid[evtr== "10%"]<- 1
ve$evtr_low[evtr== "15%"]<--1
ve$evtr_mid[evtr== "15%"]<--1

ve$evdr_sht<-0
ve$evdr_mid<-0
ve$evdr_sht[evdr== 150]<- 1
ve$evdr_mid[evdr== 300]<- 1
ve$evdr_sht[evdr== 450]<--1
ve$evdr_mid[evdr== 450]<--1

ve$evsc_fast<-1
ve$evsc_fast[evsc == "3,0"]<- -1

ve$evfc_fast<-0
ve$evfc_mid <-0
ve$evfc_fast[evfc==  5]<- 1
ve$evfc_mid [evfc== 10]<- 1
ve$evfc_fast[evfc== 15]<--1
ve$evfc_mid [evfc== 15]<--1

ve$evtc_fast<-0
ve$evtc_mid <-0
ve$evtc_fast[evtc==  5]<- 1
ve$evtc_mid [evtc== 10]<- 1
ve$evtc_fast[evtc== 15]<--1
ve$evtc_mid [evtc== 15]<--1

ve$rvpp_low<-0
ve$rvpp_mid<-0
ve$rvpp_low[rvpp== 15]<- 1
ve$rvpp_mid[rvpp== 30]<- 1
ve$rvpp_low[rvpp== 45]<--1
ve$rvpp_mid[rvpp== 45]<--1

ve$rvmc_low<-1
ve$rvmc_low[rvmc == 200]<- -1

ve$rvoc_low<-1
ve$rvoc_low[rvoc == "0,15"]<- -1

ve$shmc_low<-0
ve$shmc_mid<-0
ve$shmc_low[shmc== "0"] <- 1
ve$shmc_mid[shmc== "10"]<- 1
ve$shmc_low[shmc== "20"]<--1
ve$shmc_mid[shmc== "20"]<--1

ve$shoc_low<-1
ve$shoc_low[shoc == "0,4"]<- -1

ve$shhr_low<-1
ve$shhr_low[shhr == 6]<- -1

ve$shat_fast<-0
ve$shat_mid <-0
ve$shat_fast[shat==  5]<- 1
ve$shat_mid [shat== 10]<- 1
ve$shat_fast[shat== 15]<--1
ve$shat_mid [shat== 15]<--1

ve$shav_low<-0
ve$shav_mid<-0
ve$shav_low[shav== "60%"] <- 1
ve$shav_mid[shav== "80%"]<- 1
ve$shav_low[shav== "100%"]<--1
ve$shav_mid[shav== "100%"]<--1

detach(ve)
# 
# for (i in 3:ncol(ve)){
#   print(i)
#   print(table(ve[,i]))
#   
# }

b0<-c()

b0["evpp_low"]<-0.4
b0["evpp_mid"]<-0.2



b0["evmc_low"]<-0.2


b0["evoc_low"]<-0.2

b0["evtr_low"]<--0.2
b0["evtr_mid"]<--0.1


b0["evdr_sht"]<--0.3
b0["evdr_mid"]<--0.1


b0["evsc_fast"]<-0.2


b0["evfc_fast"]<-0.3
b0["evfc_mid"]<-0.1


b0["evtc_fast"]<-0.2
b0["evtc_mid"]<-0.1


b0["rvpp_low"]<-0.4
b0["rvpp_mid"]<-0.2


b0["rvmc_low"]<-0.2


b0["rvoc_low"]<-0.2


b0["shmc_low"]<-0.3
b0["shmc_mid"]<--0.1


b0["shoc_low"]<-0.2


b0["shhr_low"]<-0.1


b0["shat_fast"]<-0.3
b0["shat_mid"]<-0.1


b0["shav_low"]<--0.2
b0["shav_mid"]<--0.1





vev<-b0["evpp_low"]*ve$evpp_low + b0["evpp_mid"]*ve$evpp_mid + b0["evmc_low"]*ve$evmc_low + b0["evoc_low"]*ve$evoc_low + b0["evtr_low"]*ve$evtr_low + b0["evtr_mid"]*ve$evtr_mid +
     b0["evdr_sht"]*ve$evdr_sht + b0["evdr_mid"]*ve$evdr_mid + b0["evsc_fast"]*ve$evsc_fast + b0["evfc_fast"]*ve$evfc_fast + b0["evfc_mid"]*ve$evfc_mid + b0["evtc_fast"]*ve$evtc_fast + b0["evtc_mid"]*ve$evtc_mid

vrv<-b0["rvpp_low"]*ve$rvpp_low + b0["rvpp_mid"]*ve$rvpp_mid + b0["rvmc_low"]*ve$rvmc_lo + b0["rvoc_low"]*ve$rvoc_low

vsh<-b0["shmc_low"]*ve$shmc_low + b0["shmc_mid"]*ve$shmc_mid + b0["shoc_low"]*ve$shoc_low + 
     b0["shhr_low"]*ve$shhr_low + b0["shat_fast"]*ve$shat_fast + b0["shat_mid"]*ve$shat_mid + b0["shav_low"]*ve$shav_low + b0["shav_mid"]*ve$shav_mid

ve$pev<- exp(vev)/(exp(vev)+exp(vrv)+exp(vsh))

ve$prv<- exp(vrv)/(exp(vev)+exp(vrv)+exp(vsh))

ve$psh<- exp(vsh)/(exp(vev)+exp(vrv)+exp(vsh))


scale<-100

ve1 <- ve[rep(1:nrow(ve),each=scale),]

ve1$rpb<-runif(nrow(ve1)) 


ve1$choice[ve1$rpb<=ve1$pev]<-1
ve1$choice[ve1$rpb>ve1$pev & ve1$rpb<=(ve1$pev+ve1$prv)]<-2
ve1$choice[ve1$rpb>(ve1$pev+ve1$prv)]<-3



# en1$choice[en1$psp>=en1$pne & en1$psp>=en1$php]<-1
# en1$choice[en1$psp<=en1$php & en1$pne<=en1$php]<-2
# en1$choice[en1$psp<=en1$pne & en1$php<=en1$pne]<-3

table(ve1$choice)
D1   <- ve1$choice == 1
D2   <- ve1$choice == 2
D3   <- ve1$choice == 3

f <- function(b){
  
  
  v1<-b["evpp_low"]*ve1$evpp_low + b["evpp_mid"]*ve1$evpp_mid + b["evmc_low"]*ve1$evmc_low + b["evoc_low"]*ve1$evoc_low + b["evtr_low"]*ve1$evtr_low + b["evtr_mid"]*ve1$evtr_mid +
      b["evdr_sht"]*ve1$evdr_sht + b["evdr_mid"]*ve1$evdr_mid + b["evsc_fast"]*ve1$evsc_fast + b["evfc_fast"]*ve1$evfc_fast + b["evfc_mid"]*ve1$evfc_mid + b["evtc_fast"]*ve1$evtc_fast + b["evtc_mid"]*ve1$evtc_mid
  
  v2<-b["rvpp_low"]*ve1$rvpp_low + b["rvpp_mid"]*ve1$rvpp_mid + b["rvmc_low"]*ve1$rvmc_lo + b["rvoc_low"]*ve1$rvoc_low
  
  v3<-b["shmc_low"]*ve1$shmc_low + b["shmc_mid"]*ve1$shmc_mid + b["shoc_low"]*ve1$shoc_low + 
      b["shhr_low"]*ve1$shhr_low + b["shat_fast"]*ve1$shat_fast + b["shat_mid"]*ve1$shat_mid + b["shav_low"]*ve1$shav_low + b["shav_mid"]*ve1$shav_mid
  
  p1 <- exp(v1)/(exp(v1)+exp(v2)+exp(v3))
  p2 <- exp(v2)/(exp(v1)+exp(v2)+exp(v3))
  p3 <- exp(v3)/(exp(v1)+exp(v2)+exp(v3))
  
  log_p <- D1*log(p1) + D2*log(p2) + D3*log(p3)
  
  LL<-sum(log_p)
  
  return(LL)
}

start_time <- date()
print(start_time)
res <- maxLik(f, start = b0, method = "BFGS",
              control = list(printLevel = 1, iterlim = 1000))
end_time <- date()
print(end_time)

estimate        <- res$estimate
Final_Lik       <- f(estimate)
hhh             <- res$hessian
t0              <- estimate/sqrt(-diag(solve(hhh)))
se              <- sqrt(-diag(solve(hhh)))
k               <- length(b0)
#b0["panel"]     <- 0 
Nobs            <- nrow(ve1)
Initial_Lik     <- -log(3)*Nobs
Rho_squared     <- 1-Final_Lik/Initial_Lik
Adj_Rho_squared <- 1-(Final_Lik-k)/Initial_Lik


AIC             <- (2*k-2*Final_Lik)/Nobs
BIC             <- (log(Nobs)*k-2*Final_Lik)/Nobs

summary(res) 
GOF <- list(Initial_Lik     = round(Initial_Lik, 3),
            Final_Lik       = round(Final_Lik, 3),
            Rho_squared     = round(Rho_squared, 4),
            Adj_Rho_squared = round(Adj_Rho_squared, 4),
            AIC             = round(AIC, 4),
            BIC             = round(BIC, 4))
print(GOF)

bias<-(b0-estimate)/b0
print(bias)

#for (i in 3:26){
#  print(table(en1[,i]))
#}
