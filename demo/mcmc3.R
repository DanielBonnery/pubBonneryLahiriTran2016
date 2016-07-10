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
xys_aggrd<-sqldf("select count(*) as n, state,itemcode, type_of_gov, sum(lftemp12) as lftemp12, sum(lftemp07) as lftemp07 
                 from xys group by type_of_gov, itemcode, state")

#3. Computations
usefullvariables<-c("state","itemcode","type_of_gov","lftemp12","lftemp07")
dime<-sapply(xys[c("state","itemcode","type_of_gov")],nlevels)
dimnamesd<-sapply(xys[c("state","itemcode","type_of_gov")],levels)

#3.2. model we discussed
fit2 <-jags(
  data=c(list(N=nrow(xys),dime=dime,xy=xy),xy_aggrd[usefullvariables]),
  inits=list(list("a1"=array(0,dime),"beta1"=array(0,dime),"tau"=array(1,dime))),
  n.chains =1,
  parameters.to.save=c("sigma","beta0", "beta1"),
  n.iter =10 ,
  n.burnin =3 ,
  model.file= textConnection (
    "model {
    for (i in 1:N) {
    lftemp[i]~dnorm(beta0[state[i],itemcode[i],type_of_gov[i]]+beta1[state[i],itemcode[i],type_of_gov[i]]*lftemp07[i],tau)}
    for (i1 in 1:dime[1]) {
    for (i2 in 1:dime[2]) {
    for (i3 in 1:dime[3]) {
    beta0[i1,i2,i3]~ dnorm (a1[i1]+a2[i2]+a3[i3]+b1[i2,i3]+b2[i1,i3]+b3[i1,i2] ,1.0E-4);
    beta1[i1,i2,i3]~ dnorm (c1[i1]+c2[i2]+c3[i3]+d1[i2,i3]+d2[i1,i3]+d3[i1,i2] ,1.0E-4);}}}
    for (i1 in 1:dime[1]) {a1[i1]~dnorm(0,1.0E-4);c1[i1]~dnorm(0,1.0E-4)}
    for (i2 in 1:dime[2]) {a2[i2]~dnorm(0,1.0E-4);c2[i2]~dnorm(0,1.0E-4)}
    for (i3 in 1:dime[3]) {a3[i3]~dnorm(0,1.0E-4);c3[i3]~dnorm(0,1.0E-4)}
    for (i1 in 1:dime[1]) {
    for (i2 in 1:dime[2]) {}}
for (i1 in 1:dime[1]) {
    for (i3 in 1:dime[3]) {}}
for (i2 in 1:dime[2]) {
    for (i3 in 1:dime[3]) {}}


    tau~ dgamma (1.0E-4 ,1.0E-4);
    sigma <- 1/tau
    }"))

d=xr[xr$state=="Alabama"&xr$itemcode=="Water Supply " &xr$type_of_gov=="Municipal government",]
fit=fit1
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



