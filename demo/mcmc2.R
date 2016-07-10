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

#3. Computations
usefullvariables<-c("state","itemcode","type_of_gov","lftemp12","lftemp07")
dime<-sapply(xys[c("state","itemcode","type_of_gov")],nlevels)
dimnamesd<-sapply(xys[c("state","itemcode","type_of_gov")],levels)
#3.1. basic model to test that everything is fine
fit2 <-jags(
  data=c(list(N=nrow(xys),dime=dime),xys[usefullvariables]),
  inits=list(list("beta0"=array(0,dime),"beta1"=array(1,dime),"tau"=array(1,dime))),
  n.chains =1,
  parameters.to.save=c("sigma", "beta0", "beta1"),
  n.iter =10,
  n.burnin =3 ,
  model.file= textConnection (
    "model {
    for (i in 1:N) {
    lftemp12[i]~dnorm(beta0[state[i],itemcode[i],type_of_gov[i]]+
                      beta1[state[i],itemcode[i],type_of_gov[i]]*lftemp07[i],
                      tau[state[i],itemcode[i],type_of_gov[i]])}
    for (i1 in 1:dime[1]) {
    for (i2 in 1:dime[2]) {
    for (i3 in 1:dime[3]) {
    beta0[i1,i2,i3]~ dnorm (0 ,1.0E-4);
    beta1[i1,i2,i3]~ dnorm (1 ,1.0E-4);
    tau[i1,i2,i3]  ~ dgamma (1.0E-4 ,1.0E-4);}}}
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
    prediction=xy_aggr[,,,"ftemp12"]+
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

print(plot1)


#see 2006 gelman paper on inverted gamma that does not work see own suggestion of gelman
#see 2006 tyry half Cauchy and other proposals.
# see 2005 annals of statistics analysis of variance how to do it.
# see half normal , but make sure that difference of 1 means big thing.
