# Working document 

`pubBonneryLahiriTran2016` is an R package that contains the source code to reproduce the simulations of [""]() by Bonnery, Lahiri and Tran

## Install package

```r
devtools::install_github("DanielBonnery/pubBonneryLahiriTran2016")
```



#2. Models

##2.1. Simple model

$$\mathrm{ftpay}_{2012,k}=\beta_{0,\mathrm{state}_k,\mathrm{code}_k,\mathrm{type}_k}+
\beta_{0,\mathrm{state}_k,\mathrm{code}_k,\mathrm{type}_k}\times \mathrm{ftpay}_{2007,k}+\varepsilon_k$$


```r
library(pubBonneryLahiriTran2016)
demo(mcmc)
```

