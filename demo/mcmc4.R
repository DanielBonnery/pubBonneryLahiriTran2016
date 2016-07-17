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
fit4 <-jags(
  data=c(list(N=nrow(xys),dime=dime),xys[usefullvariables]),
  inits=list(list(alpha=rep(0,dime[1]),
                  beta=rep(0,dime[2]),
                  gamma=rep(0,dime[3]),
                  a=1/3,
                  b=1/3,
                  c=1/3,
                  d=1/3,
                  tau_ab=1,
                  tau_ag=1,
                  tau_bg=1,
                  tau_abg=1,
                  tau_e=1)),
  n.chains =1,
  parameters.to.save=c("alpha","beta","gamma","a","b","c","d","tau_ab","tau_ag","tau_bg","tau_abg","tau_e"),
  n.iter =10 ,
  n.burnin =3 ,
  model.file= textConnection (
    "model {
    for (i in 1:N) {
    lftemp12[i]~dnorm(alpha[state[i]]+beta[itemcode[i]]+gamma[type_of_gov[i]]+alphabetagamma[state[i],itemcode[i],type_of_gov[i]]+
      (alphabeta[state[i],itemcode[i]]+
       alphagamma[state[i],type_of_gov[i]]+
       betagamma[itemcode[i],type_of_gov[i]])*lftemp07[i],tau_e)}
    a~dnorm(0,1.0E-4)
    b~dnorm(0,1.0E-4)
    c~dnorm(0,1.0E-4)
    d~dnorm(0,1.0E-4)
    tau_ab~dgamma(1.0E-4,1.0E-4)
    tau_ag~dgamma(1.0E-4,1.0E-4)
    tau_bg~dgamma(1.0E-4,1.0E-4)
    tau_abg~dgamma(1.0E-4,1.0E-4)
    tau_e~dgamma(1.0E-4,1.0E-4)
    for (i in 1:dime[1]) {alpha[i]~dnorm(0,1.0E-4)}
    for (i in 1:dime[2]) {beta[i]~dnorm(0,1.0E-4)}
    for (i in 1:dime[3]) {gamma[i]~dnorm(0,1.0E-4)}
    for (i1 in 1:dime[1]) {for (i2 in 1:dime[2]) {alphabeta[i1,i2]~dnorm(a,tau_ab)}}
    for (i1 in 1:dime[1]) {for (i3 in 1:dime[3]) {alphagamma[i1,i3]~dnorm(0,tau_ag)}}
    for (i2 in 1:dime[2]) {for (i3 in 1:dime[3]) {betagamma[i2,i3]~dnorm(0,tau_bg)}}
    for (i1 in 1:dime[1]) {for (i2 in 1:dime[2]) {for (i3 in 1:dime[3]){alphabetagamma[i1,i2,i3]~dnorm(0,tau_abg)}}}
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



