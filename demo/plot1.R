asp<-Extract_years_0712_for_11_sample()
library(ggplot2)
plot1<-
  ggplot(asp, aes(x = lftpay07, y = lftpay)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")+
  geom_abline(intercept = 0,slope=1,col="blue")
