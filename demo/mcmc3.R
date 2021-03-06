#0.Libraries
library(pubBonneryLahiriTran2016)
library(sqldf)
library(R2jags)
library(dataASPEP)
library(ggplot2)
data(aspep2007,aspep2011,aspep2012,aspep2007_gov)
xy<-xyf(aspep2007, aspep2012, aspep2007_gov)
xys<-xysf(aspep2011,xy)
xr<-xrf(xy,xys)
xr_aggr<-sqldf("select distinct state, itemcode, type_of_gov, count(*) as n, sum(ftemp07) as ftemp07,sum(ftemp12) as ftemp12, from xr
               group by state, itemcode, type_of_gov")

usefullvariables<-c("state","itemcode","type_of_gov","lftemp12","lftemp07")
dime<-sapply(xys[c("state","itemcode","type_of_gov")],nlevels)
dimnamesd<-sapply(xys[c("state","itemcode","type_of_gov")],levels)

#3.2. model we discussed
fit2 <-jags(
  data=c(list(N=nrow(xys),dime=dime),xys[usefullvariables]),
  inits=list(list("a1"=rep(0,dime[1]),
                  "a2"=rep(0,dime[2]),
                  "a3"=rep(0,dime[3]),
                  "b1"=array(0,dime[-1]),
                  "b2"=array(0,dime[-2]),
                  "b3"=array(0,dime[-3]),
                  "c1"=rep(0,dime[1]),
                  "c2"=rep(0,dime[2]),
                  "c3"=rep(0,dime[3]),
                  "d1"=array(0,dime[-1]),
                  "d2"=array(0,dime[-2]),
                  "d3"=array(0,dime[-3]),
                  "tau_1"=matrix(1,2,3),
                  "sigma_1"=1)),
  n.chains =1,
  parameters.to.save=c(outer(letters[1:4],1:3,paste0),"beta0","beta1","tau","sigma"),
  n.iter =10 ,
  n.burnin =3 ,
  model.file= textConnection (
    "model {
    for (i in 1:N) {
    lftemp12[i]~dnorm(beta0[state[i],itemcode[i],type_of_gov[i]]+beta1[state[i],itemcode[i],type_of_gov[i]]*lftemp07[i],sigma_1)}
    for (i1 in 1:dime[1]) {
    for (i2 in 1:dime[2]) {
    for (i3 in 1:dime[3]) {
    beta0[i1,i2,i3]~ dnorm (a1[i1]+a2[i2]+a3[i3]+b1[i2,i3]+b2[i1,i3]+b3[i1,i2] ,1.0E-4);
    beta1[i1,i2,i3]~ dnorm (c1[i1]+c2[i2]+c3[i3]+d1[i2,i3]+d2[i1,i3]+d3[i1,i2] ,1.0E-4);}}}
    for (i1 in 1:dime[1]) {a1[i1]~dnorm(0,1.0E-4);c1[i1]~dnorm(0,1.0E-4)}
    for (i2 in 1:dime[2]) {a2[i2]~dnorm(0,1.0E-4);c2[i2]~dnorm(0,1.0E-4)}
    for (i3 in 1:dime[3]) {a3[i3]~dnorm(0,1.0E-4);c3[i3]~dnorm(0,1.0E-4)}
    for (i1 in 1:dime[1]) {
    for (i2 in 1:dime[2]) {b3[i1,i2]~dnorm(0,tau_1[1,1]);d3[i1,i2]~dnorm(0,tau_1[2,1])}}
    for (i1 in 1:dime[1]) {
    for (i3 in 1:dime[3]) {b2[i1,i3]~dnorm(0,tau_1[1,2]);d2[i1,i3]~dnorm(0,tau_1[2,2])}}
    for (i2 in 1:dime[2]) {
    for (i3 in 1:dime[3]) {b1[i2,i3]~dnorm(0,tau_1[1,3]);d1[i2,i3]~dnorm(0,tau_1[2,3])}}
    for (i in 1:2) {for (j in 1:3){tau_1[i,j]~dgamma (1.0E-4 ,1.0E-4)}} 
    sigma_1~ dgamma (1.0E-4 ,1.0E-4);
    sigma <- 1/sigma_1
    tau<-1/tau_1
    }"))

d=xr[xr$state=="Alabama"&xr$itemcode=="Water Supply " &xr$type_of_gov=="Municipal government",]
fit=fit2
sims.list<-fit$BUGSoutput$sims.list
predictions<-function(sims.list){
  n<-dim(sims.list$beta0)[1]
  i=1
  attach(sims.list)
  dimnames(beta0)<-c(list(NULL),dimnamesd)
  dimnames(beta1)<-c(list(NULL),dimnamesd)
  plyr::aaply(1:n,1,function(i){
    prediction=xy_aggr[,,,"ftemp"]+
      plyr::daply(xr,.variables=~state+itemcode+type_of_gov,.fun = function(d){
        state=levels(d$state)[unique(d$state)]
        itemcode=levels(d$itemcode)[unique(d$itemcode)]
        type_of_gov=levels(d$type_of_gov)[unique(d$type_of_gov)]
        if(nrow(d)>0){
          sum(10^(beta0[i,state,itemcode,type_of_gov]+
                    beta1[i,state,itemcode,type_of_gov]*d$lftemp07+
                    rnorm(nrow(d),sd=sigma[i]))-1)}else{0}
      })})}

AA<-predictions(sims.list)
plot1<-lattice::densityplot(AA[,2,1,2])



