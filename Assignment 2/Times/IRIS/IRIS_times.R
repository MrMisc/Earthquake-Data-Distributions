setwd('F:/Uni/Esci451/Assignment 2/Times/')
library(fitdistrplus)
library(logspline)
library(ggthemes)
library(ggplot2)
library(CDFt)


#Define standardizing function
stand<-function(vector){
  return((vector-min(vector))/(range(vector)[2]-range(vector)[1]))
}


##100 days ago | 4-5

arr<-read.csv('IRIS_4to5_100days.csv', header=FALSE)
A<-rep(1,length(arr))
for (i in 1:length(arr)){A[i]<-as.numeric(arr[2,i])}

A0<-A[order(A)] #Needed to add this to arrange the events in ascending order in order to actually 
inter<-rep(1,length(arr)-1)
for (i in 1:length(A0)-1){inter[i]<-A0[i+1]-A0[i]}

png("IRIS/IRIS_4to5_100daysTimes.png", 1280,720)
ggplot(data=as.data.frame(inter), aes(x=inter))+geom_histogram(aes(y=..density..),,color="blue", fill="black",bins = 35)+
  scale_color_brewer(palette="Dark2")+theme_fivethirtyeight()+
  ggtitle("Inter-arrival times for the past 100 days | Magn 4-5 IRIS")
dev.off()

png("IRIS/IRIS_4to5_100days_Times_CullenFrey.png", 1280,720)
descdist(data=inter, discrete = FALSE, boot=100, boot.col="red")
dev.off()

#Let us try to narrow down to the more likely distributions which I think are: exp and lognormal
EQ_exp <- fitdist(inter, "exp", method = 'mme')
png("IRIS/IRIS_4to5_100days_Times_fittoexp.png", 1280,720)
plot(EQ_exp)
dev.off()


EQ_lognormal <- fitdist(inter, "lnorm", method = 'mme')
png("IRIS/IRIS_4to5_100days_Times_fittolognormal.png", 1280,720)
plot(EQ_lognormal)
dev.off()



EQ_beta<-fitdist(stand(inter), "beta", method = "mme")
png("IRIS/IRIS_4to5_100days_Times_fittobeta.png", 1280,720)
plot(EQ_beta)
dev.off()

rm(EQ_gamma2)
EQ_gamma2<- fitdist(inter,"gamma", "mme")
png("IRIS/IRIS_4to5_100days_Times_fittogamma2.png", 1280,720)
plot(EQ_gamma2)
dev.off()


#Let us now remake functions to calculate the probailities that the distributions do indeed fit


GOF_beta<-function(vector,test, no = 1000){
  y<-rbeta(no, test$estimate[1], test$estimate[2])
  res<-CramerVonMisesTwoSamples(vector,y)
  return(1/6*exp(-res))
}


GOF_exp<-function(vector,test, no = 1000){
  rate<-summary(test)$estimate
  y<-rexp(no, rate)
  res<-CramerVonMisesTwoSamples(vector,y)
  return(1/6*exp(-res))
}


GOF_lnorm <-function(vector,test, no = 1000){
  one<-summary(test)$estimate[1]
  two<-summary(test)$estimate[2]
  y<-rlnorm(no, one,two)
  res<-CramerVonMisesTwoSamples(vector,y)
  return(1/6*exp(-res))
}


GOF_exp(inter,EQ_exp)   #[1] 0.03898443
GOF_lnorm(inter,EQ_lognormal)   #[1] 0.007543514
GOF_beta(stand(inter),EQ_beta)  #[1] 0.04002018


#Let us try using the Kolmogorov-Smirnov test


GOF2_exp<-function(vector,test, no = 1000){
  y<-rexp(no, test$estimate)
  res<-ks.test(vector,y)
  return(res$p.value)
}


GOF2_beta<-function(vector,test, no = 1000){
  y<-rbeta(no, test$estimate[1], test$estimate[2])
  res<-ks.test(vector,y)
  return(res$p.value)
}

GOF2_lnorm <-function(vector,test, no = 1000){
  one<-summary(test)$estimate[1]
  two<-summary(test)$estimate[2]
  y<-rlnorm(no, one,two)
  res<-ks.test(vector,y)
  return(res$p.value)
}

GOF2_exp(inter,EQ_exp)      #[1] 0.02470084
GOF2_lnorm(inter,EQ_lognormal)  #[1] 1.507699e-08
GOF2_beta(stand(inter),EQ_beta)  #[1] 4.610532e-05

#Note the parameters to the fitted models are 
EQ_exp$estimate
#        rate 
#0.0004195228 


EQ_lognormal$estimate
#  meanlog     sdlog 
#7.3535871 0.9195712 


EQ_beta$estimate
#   shape1    shape2 
#0.6201672 7.7041459
















## 5-6 magnitudes


arr<-read.csv('IRIS_5to6_100days.csv', header=FALSE)
A<-rep(1,length(arr))
for (i in 1:length(arr)){A[i]<-as.numeric(arr[2,i])}

A0<-A[order(A)] #Needed to add this to arrange the events in ascending order in order to actually 
inter<-rep(1,length(arr)-1)
for (i in 1:length(A0)-1){inter[i]<-A0[i+1]-A0[i]}

png("IRIS/IRIS_5to6_100daysTimes.png", 1280,720)
ggplot(data=as.data.frame(inter), aes(x=inter))+geom_histogram(aes(y=..density..),,color="blue", fill="black",bins = 35)+
  scale_color_brewer(palette="Dark2")+theme_fivethirtyeight()+
  ggtitle("Inter-arrival times for the past 100 days | Magn 5-6 IRIS")
dev.off()

png("IRIS/IRIS_5to6_100days_Times_CullenFrey.png", 1280,720)
descdist(data=inter, discrete = FALSE, boot=100, boot.col="red")
dev.off()

#Let us try to narrow down to the more likely distributions which I think are: exp and lognormal
EQ_exp <- fitdist(inter, "exp", method = 'mme')
png("IRIS/IRIS_5to6_100days_Times_fittoexp.png", 1280,720)
plot(EQ_exp)
dev.off()


EQ_lognormal <- fitdist(inter, "lnorm", method = 'mme')
png("IRIS/IRIS_5to6_100days_Times_fittolognormal.png", 1280,720)
plot(EQ_lognormal)
dev.off()



EQ_beta<-fitdist(stand(inter), "beta", method = "mme")
png("IRIS/IRIS_5to6_100days_Times_fittobeta.png", 1280,720)
plot(EQ_beta)
dev.off()

rm(EQ_gamma2)
EQ_gamma2<- fitdist(inter,"gamma", "mme")
png("IRIS/IRIS_5to6_100days_Times_fittogamma2.png", 1280,720)
plot(EQ_gamma2)
dev.off()

#pvalues

GOF_exp(inter,EQ_exp)   #[1] 0.1432205
GOF_lnorm(inter,EQ_lognormal)   #[1] 0.02337594
GOF_beta(stand(inter),EQ_beta)  #[1] 0.1405588



GOF2_exp(inter,EQ_exp)      #[1] 0.424202
GOF2_lnorm(inter,EQ_lognormal)  #[1] 6.282174e-06
GOF2_beta(stand(inter),EQ_beta)  #[1] 0.279259


#Note the parameters to the fitted models are 
EQ_exp$estimate
#        rate 
#6.02862e-05


EQ_lognormal$estimate
#  meanlog     sdlog 
#9.3456902 0.8610659 


EQ_beta$estimate
#   shape1    shape2 
#0.6501119 4.1987951














## 6-7 magnitudes


arr<-read.csv('IRIS_6to7_100days.csv', header=FALSE)
A<-rep(1,length(arr))
for (i in 1:length(arr)){A[i]<-as.numeric(arr[2,i])}

A0<-A[order(A)] #Needed to add this to arrange the events in ascending order in order to actually 
inter<-rep(1,length(arr)-1)
for (i in 1:length(A0)-1){inter[i]<-A0[i+1]-A0[i]}

png("IRIS/IRIS_6to7_100daysTimes.png", 1280,720)
ggplot(data=as.data.frame(inter), aes(x=inter))+geom_histogram(aes(y=..density..),,color="blue", fill="black",bins = 35)+
  scale_color_brewer(palette="Dark2")+theme_fivethirtyeight()+
  ggtitle("Inter-arrival times for the past 100 days | Magn 6-7 IRIS")
dev.off()

png("IRIS/IRIS_6to7_100days_Times_CullenFrey.png", 1280,720)
descdist(data=inter, discrete = FALSE, boot=100, boot.col="red")
dev.off()

#Let us try to narrow down to the more likely distributions which I think are: exp and lognormal
EQ_exp <- fitdist(inter, "exp", method = 'mme')
png("IRIS/IRIS_6to7_100days_Times_fittoexp.png", 1280,720)
plot(EQ_exp)
dev.off()


EQ_lognormal <- fitdist(inter, "lnorm", method = 'mme')
png("IRIS/IRIS_6to7_100days_Times_fittolognormal.png", 1280,720)
plot(EQ_lognormal)
dev.off()



EQ_beta<-fitdist(stand(inter), "beta", method = "mme")
png("IRIS/IRIS_6to7_100days_Times_fittobeta.png", 1280,720)
plot(EQ_beta)
dev.off()


rm(EQ_gamma2)
EQ_gamma2<- fitdist(inter,"gamma", "mme")
png("IRIS/IRIS_6to7_100days_Times_fittogamma2.png", 1280,720)
plot(EQ_gamma2)
dev.off()

#pvalues

GOF_exp(inter,EQ_exp)   #[1] 0.1461152
GOF_lnorm(inter,EQ_lognormal)   #[1] 0.097367
GOF_beta(stand(inter),EQ_beta)  #[1] 0.1618009



GOF2_exp(inter,EQ_exp)      #[1] 0.6408923
GOF2_lnorm(inter,EQ_lognormal)  #[1] 0.01425421
GOF2_beta(stand(inter),EQ_beta)  #[1] 0.9887386


#Note the parameters to the fitted models are 
EQ_exp$estimate
#        rate 
#5.262042e-06 


EQ_lognormal$estimate
#   meanlog      sdlog 
#11.8665098  0.7595809


EQ_beta$estimate
#    shape1    shape2 
#0.5599189 1.2114687 


























## 500 days 4-5 magnitudes


arr<-read.csv('IRIS_4to5_500days.csv', header=FALSE)
A<-rep(1,length(arr))
for (i in 1:length(arr)){A[i]<-as.numeric(arr[2,i])}

A0<-A[order(A)] #Needed to add this to arrange the events in ascending order in order to actually 
inter<-rep(1,length(arr)-1)
for (i in 1:length(A0)-1){inter[i]<-A0[i+1]-A0[i]}

png("IRIS/IRIS_4to5_500daysTimes.png", 1280,720)
ggplot(data=as.data.frame(inter), aes(x=inter))+geom_histogram(aes(y=..density..),,color="blue", fill="black",bins = 35)+
  scale_color_brewer(palette="Dark2")+theme_fivethirtyeight()+
  ggtitle("Inter-arrival times for the past 500 days | Magn 4-5 IRIS")
dev.off()

png("IRIS/IRIS_4to5_500days_Times_CullenFrey.png", 1280,720)
descdist(data=inter, discrete = FALSE, boot=100, boot.col="red")
dev.off()

#Let us try to narrow down to the more likely distributions which I think are: exp and lognormal
EQ_exp <- fitdist(inter, "exp", method = 'mme')
png("IRIS/IRIS_4to5_500days_Times_fittoexp.png", 1280,720)
plot(EQ_exp)
dev.off()


EQ_lognormal <- fitdist(inter, "lnorm", method = 'mme')
png("IRIS/IRIS_4to5_500days_Times_fittolognormal.png", 1280,720)
plot(EQ_lognormal)
dev.off()



EQ_beta<-fitdist(stand(inter), "beta", method = "mme")
png("IRIS/IRIS_4to5_500days_Times_fittobeta.png", 1280,720)
plot(EQ_beta)
dev.off()

rm(EQ_gamma2)
EQ_gamma2<- fitdist(inter,"gamma", "mme")
png("IRIS/IRIS_4to5_500days_Times_fittogamma2.png", 1280,720)
plot(EQ_gamma2)
dev.off()
#pvalues

GOF_exp(inter,EQ_exp)   #[1] 4.726182e-06
GOF_lnorm(inter,EQ_lognormal)   #[1] 2.775558e-15
GOF_beta(stand(inter),EQ_beta)  #[1] 0.01358386



GOF2_exp(inter,EQ_exp)      #[1] 0.00813423
GOF2_lnorm(inter,EQ_lognormal)  #[1] 1.235678e-13
GOF2_beta(stand(inter),EQ_beta)  #[1] 2.140519e-06


#Note the parameters to the fitted models are 
EQ_exp$estimate
#       rate 
#0.000445051 


EQ_lognormal$estimate
#  meanlog     sdlog 
#7.2868220 0.9279006 


EQ_beta$estimate
#    shape1    shape2 
#0.6265642 9.6370516 

EQ_gamma2$estimate
#       shape         rate 
#0.7323197735 0.0003259196 


summary(EQ_exp)[7:8]
summary(EQ_lognormal)[7:8]
summary(EQ_beta)[7:8]
summary(EQ_gamma2)[7:8]




## 500 days 5-6 magnitudes


arr<-read.csv('IRIS_5to6_500days.csv', header=FALSE)
A<-rep(1,length(arr))
for (i in 1:length(arr)){A[i]<-as.numeric(arr[2,i])}

A0<-A[order(A)] #Needed to add this to arrange the events in ascending order in order to actually 
inter<-rep(1,length(arr)-1)
for (i in 1:length(A0)-1){inter[i]<-A0[i+1]-A0[i]}

png("IRIS/IRIS_5to6_500daysTimes.png", 1280,720)
ggplot(data=as.data.frame(inter), aes(x=inter))+geom_histogram(aes(y=..density..),,color="blue", fill="black",bins = 35)+
  scale_color_brewer(palette="Dark2")+theme_fivethirtyeight()+
  ggtitle("Inter-arrival times for the past 500 days | Magn 5-6 IRIS")
dev.off()

png("IRIS/IRIS_5to6_500days_Times_CullenFrey.png", 1280,720)
descdist(data=inter, discrete = FALSE, boot=100, boot.col="red")
dev.off()

#Let us try to narrow down to the more likely distributions which I think are: exp and lognormal
EQ_exp <- fitdist(inter, "exp", method = 'mme')
png("IRIS/IRIS_5to6_500days_Times_fittoexp.png", 1280,720)
plot(EQ_exp)
dev.off()


EQ_lognormal <- fitdist(inter, "lnorm", method = 'mme')
png("IRIS/IRIS_5to6_500days_Times_fittolognormal.png", 1280,720)
plot(EQ_lognormal)
dev.off()



EQ_beta<-fitdist(stand(inter), "beta", method = "mme")
png("IRIS/IRIS_5to6_500days_Times_fittobeta.png", 1280,720)
plot(EQ_beta)
dev.off()

rm(EQ_gamma2)
EQ_gamma2<- fitdist(inter,"gamma", "mme")
png("IRIS/IRIS_5to6_500days_Times_fittogamma2.png", 1280,720)
plot(EQ_gamma2)
dev.off()
#pvalues

GOF_exp(inter,EQ_exp)   #[1] 0.001825772
GOF_lnorm(inter,EQ_lognormal)   #[1] 0.0001095257
GOF_beta(stand(inter),EQ_beta)  #[1] 0.1282681



GOF2_exp(inter,EQ_exp)      #[1] 4.395595e-11
GOF2_lnorm(inter,EQ_lognormal)  
GOF2_beta(stand(inter),EQ_beta)  #[1] 0.01250803


#Note the parameters to the fitted models are 
EQ_exp$estimate
#        rate 
#6.330165e-05 


EQ_lognormal$estimate
#  meanlog     sdlog 
#9.1887898 0.9785798 


EQ_beta$estimate
#   shape1    shape2 
#0.4500607 3.7777442 



EQ_gamma2$estimate
#       shape         rate 
#6.228651e-01 3.942839e-05 



summary(EQ_exp)[7:8]
summary(EQ_lognormal)[7:8]
summary(EQ_beta)[7:8]
summary(EQ_gamma2)[7:8]




## 500 days 6-7 magnitudes


arr<-read.csv('IRIS_6to7_500days.csv', header=FALSE)
A<-rep(1,length(arr))
for (i in 1:length(arr)){A[i]<-as.numeric(arr[2,i])}

A0<-A[order(A)] #Needed to add this to arrange the events in ascending order in order to actually 
inter<-rep(1,length(arr)-1)
for (i in 1:length(A0)-1){inter[i]<-A0[i+1]-A0[i]}

png("IRIS/IRIS_6to7_500daysTimes.png", 1280,720)
ggplot(data=as.data.frame(inter), aes(x=inter))+geom_histogram(aes(y=..density..),,color="blue", fill="black",bins = 35)+
  scale_color_brewer(palette="Dark2")+theme_fivethirtyeight()+
  ggtitle("Inter-arrival times for the past 500 days | Magn 6-7 IRIS")
dev.off()

png("IRIS/IRIS_6to7_500days_Times_CullenFrey.png", 1280,720)
descdist(data=inter, discrete = FALSE, boot=100, boot.col="red")
dev.off()

#Let us try to narrow down to the more likely distributions which I think are: exp and lognormal
EQ_exp <- fitdist(inter, "exp", method = 'mme')
png("IRIS/IRIS_6to7_500days_Times_fittoexp.png", 1280,720)
plot(EQ_exp)
dev.off()


EQ_lognormal <- fitdist(inter, "lnorm", method = 'mme')
png("IRIS/IRIS_6to7_500days_Times_fittolognormal.png", 1280,720)
plot(EQ_lognormal)
dev.off()



EQ_beta<-fitdist(stand(inter), "beta", method = "mme")
png("IRIS/IRIS_6to7_500days_Times_fittobeta.png", 1280,720)
plot(EQ_beta)
dev.off()

rm(EQ_gamma2)
EQ_gamma2<- fitdist(inter,"gamma", "mme")
png("IRIS/IRIS_6to7_500days_Times_fittogamma2.png", 1280,720)
plot(EQ_gamma2)
dev.off()
#pvalues

GOF_exp(inter,EQ_exp)   #[1] 0.1101473
GOF_lnorm(inter,EQ_lognormal)   #[1] 0.03817739
GOF_beta(stand(inter),EQ_beta)  #[1] 0.1396662



GOF2_exp(inter,EQ_exp)      #[1] 0.04840755
GOF2_lnorm(inter,EQ_lognormal)  #[1] 6.277832e-06
GOF2_beta(stand(inter),EQ_beta)  #[1] 0.3323193


#Note the parameters to the fitted models are 
EQ_exp$estimate
#         rate 
#4.653808e-06 


EQ_lognormal$estimate
#   meanlog      sdlog 
#11.8772146  0.8951091 


EQ_beta$estimate
#    shape1    shape2 
#0.5531887 3.2923870 

EQ_gamma2$estimate
#       shape         rate 
#8.141610e-01 3.788949e-06 



summary(EQ_exp)[7:8]
summary(EQ_lognormal)[7:8]
summary(EQ_beta)[7:8]
summary(EQ_gamma2)[7:8]

