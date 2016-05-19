library(R2jags)
asp<-Extract_years_0712_for_11_sample()
dime=unlist(lapply(asp[c("state","itemcode","type_of_gov")],nlevels))
fit<-jags(
  data=c(list(N=nrow(asp),dime=dime),asp),
  inits=list(list("beta0"=array(0,dime),"beta1"=array(0,dime),"tau"=1)),
  n.chains =1,
  parameters.to.save=c("sigma", "beta0", "beta1"),
  n.iter =100,
  n.burnin =3 ,
  model.file= textConnection (
    "model {
    for (i in 1:N) {
    lftpay[i]~dnorm(beta0[state[i],itemcode[i],type_of_gov[i]]+beta1[state[i],itemcode[i],type_of_gov[i]]*lftpay07[i],tau)}
    for (i1 in 1:dime[1]) {
    for (i2 in 1:dime[2]) {
    for (i3 in 1:dime[3]) {
    beta0[i1,i2,i3]~ dnorm (0 ,1.0E-4);
    beta1[i1,i2,i3]~ dnorm (0 ,1.0E-4);}}}
    tau~ dgamma (1.0E-4 ,1.0E-4);
    sigma <- 1/tau
    }"))
print(fit)