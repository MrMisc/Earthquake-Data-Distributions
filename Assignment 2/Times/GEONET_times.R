setwd('F:/Uni/Esci451/Assignment 2/Times/')
library(fitdistrplus)
library(logspline)
library(ggthemes)
library(ggplot2)
library(CDFt)
library(gofgamma)


#Define standardizing function
stand<-function(vector){
  return((vector-min(vector))/(range(vector)[2]-range(vector)[1]))
}


##100 days ago | 4-5

arr<-read.csv('GEONET_4to5_100days.csv', header=FALSE)
A<-rep(1,length(arr))
for (i in 1:length(arr)){A[i]<-as.numeric(arr[2,i])}

A0<-A[order(A)] #Needed to add this to arrange the events in ascending order in order to actually 
inter<-rep(1,length(arr)-1)
for (i in 1:length(A0)-1){inter[i]<-A0[i+1]-A0[i]}

png("GEONET_4to5_100daysTimes.png", 1920,1080)
ggplot(data=as.data.frame(inter), aes(x=inter))+geom_histogram(aes(y=..density..),,color="blue", fill="black",bins = 35)+
  scale_color_brewer(palette="Dark2")+theme_fivethirtyeight()+
  ggtitle("Inter-arrival times for the past 100 days | Magn 4-5 GEONET")
dev.off()

png("GEONET_100days_Times_CullenFrey.png", 1920,1080)
descdist(data=inter, discrete = FALSE, boot=100, boot.col="red")
dev.off()

#Let us try to narrow down to the more likely distributions which I think are: exp and lognormal
EQ_exp <- fitdist(inter, "exp", method = 'mme')
png("GEONET_100days_Times_fittoexp.png", 1920,1080)
plot(EQ_exp)
dev.off()


EQ_lognormal <- fitdist(inter, "lnorm", method = 'mme')
png("GEONET_100days_Times_fittolognormal.png", 1920,1080)
plot(EQ_lognormal)
dev.off()



EQ_beta<-fitdist(stand(inter), "beta", method = "mme")
png("GEONET_4to5_100days_Times_fittobeta.png", 1920,1080)
plot(EQ_beta)
dev.off()

EQ_gamma<-fitdist(inter, "gamma", method = "mme")
png("GEONET_4to5_100days_Times_fittogamma2.png", 1920,1080)
plot(EQ_gamma)
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


GOF_gamma<-function(vector,test, no = 1000){
  shape<-summary(test)$estimate[1]
  scale<-1/summary(test)$estimate[2]
  y<-rgamma(no, shape,scale)
  res<-CramerVonMisesTwoSamples(vector,y)
  return(1/6*exp(-res))
}


GOF_gamma(inter, EQ_gamma)  
GOF_exp(inter,EQ_exp)   #[1] 0.1565025
GOF_lnorm(inter,EQ_lognormal)   #[1] 0.05487013
GOF_beta(stand(inter),EQ_beta)  #[1] 0.1277601


#Let us try using the Kolmogorov-Smirnov test


GOF2_exp<-function(vector,test, no = 1000){
  y<-rexp(no, test$estimate)
  res<-ks.test(vector,y)
  return(res$p.value)
}

GOF2_gamma<-function(vector,test, no = 1000){
  shape<-summary(test)$estimate[1]
  scale<-1/summary(test)$estimate[2]
  y<-rgamma(no, shape,scale)
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

GOF2_gamma(inter,EQ_gamma)
GOF2_exp(inter,EQ_exp)      #[1] 0.3195497
GOF2_lnorm(inter,EQ_lognormal)  #[1] 9.552787e-06
GOF2_beta(stand(inter),EQ_beta)  #[1] 0.5313513

#Note the parameters to the fitted models are 
EQ_exp$estimate
#        rate 
#4.540885e-05 


EQ_lognormal$estimate
#  meanlog     sdlog 
#9.6526636 0.8332345 


EQ_beta$estimate
#   shape1    shape2 
#0.6862408 3.8913926 


## Now let us try this for the earthquakes of mag 5-6 instead of 4-5
#Histogram

arr<-read.csv('GEONET_5to6_100days.csv', header=FALSE)
A<-rep(1,length(arr))
for (i in 1:length(arr)){A[i]<-as.numeric(arr[2,i])}

A0<-A[order(A)] #Needed to add this to arrange the events in ascending order in order to actually 
inter<-rep(1,length(arr)-1)
for (i in 1:length(A0)-1){inter[i]<-A0[i+1]-A0[i]}

png("GEONET_5to6_100daysTimes.png", 1920,1080)
ggplot(data=as.data.frame(inter), aes(x=inter))+geom_histogram(aes(y=..density..),,color="blue", fill="black",bins = 35)+
  scale_color_brewer(palette="Dark2")+theme_fivethirtyeight()+
  ggtitle("Inter-arrival times for the past 100 days | Magn 5-6 GEONET")
dev.off()


png("GEONET_5to6_100days_Times_CullenFrey.png", 1920,1080)
descdist(data=inter, discrete = FALSE, boot=100, boot.col="red")
dev.off()

#Let us try to narrow down to the more likely distributions which I think are: exp and lognormal
EQ_exp <- fitdist(inter, "exp", method = 'mme')
png("GEONET_5to6_100days_Times_fittoexp.png", 1920,1080)
plot(EQ_exp)
dev.off()


EQ_lognormal <- fitdist(inter, "lnorm", method = 'mme')
png("GEONET_5to6_100days_Times_fittolognormal.png", 1920,1080)
plot(EQ_lognormal)
dev.off()


EQ_beta<-fitdist(stand(inter), "beta", method = "mme")
png("GEONET_5to6_100days_Times_fittobeta.png", 1920,1080)
plot(EQ_beta)
dev.off()

EQ_gamma2<- fitdist(inter,"gamma", "mme")
png("GEONET_5to6_100days_Times_fittogamma2.png", 1920,1080)
plot(EQ_gamma2)
dev.off()





GOF_exp(inter,EQ_exp)           #[1] 0.104276
GOF_lnorm(inter,EQ_lognormal)   #[1] 0.07388103
GOF_beta(stand(inter), EQ_beta) #[1] 0.119383
GOF_gamma(inter, EQ_gamma2)



GOF2_exp(inter,EQ_exp)          #[1] 0.3660251
GOF2_lnorm(inter,EQ_lognormal)  #[1] 0.0006085738
GOF2_beta(stand(inter), EQ_beta)#[1] 0.0887012



EQ_exp$estimate
#rate 
#1.925021e-05 

EQ_lognormal$estimate
#   meanlog      sdlog 
#10.4582752  0.8941066

EQ_beta$estimate
#   shape1    shape2 
#0.5039745 2.4728523 






## Now let us try this for the earthquakes of mag 6-7
#Histogram

arr<-read.csv('GEONET_6to7_100days.csv', header=FALSE)
A<-rep(1,length(arr))
for (i in 1:length(arr)){A[i]<-as.numeric(arr[2,i])}

A0<-A[order(A)] #Needed to add this to arrange the events in ascending order in order to actually 
inter<-rep(1,length(arr)-1)
for (i in 1:length(A0)-1){inter[i]<-A0[i+1]-A0[i]}


png("GEONET_6to7_100daysTimes.png", 1920,1080)
ggplot(data=as.data.frame(inter), aes(x=inter))+geom_histogram(aes(y=..density..),,color="blue", fill="black",bins = 35)+
  scale_color_brewer(palette="Dark2")+theme_fivethirtyeight()+
  ggtitle("Inter-arrival times for the past 100 days | Magn 6-7 GEONET")
dev.off()

png("GEONET_6to7_100days_Times_CullenFrey.png", 1920,1080)
descdist(data=inter, discrete = FALSE, boot=100, boot.col="red")
dev.off()

#Let us try to narrow down to the more likely distributions which I think are: exp and lognormal
EQ_exp <- fitdist(inter, "exp", method = 'mme')
png("GEONET_6to7_100days_Times_fittoexp.png", 1920,1080)
plot(EQ_exp)
dev.off()


EQ_lognormal <- fitdist(inter, "lnorm", method = 'mme')
png("GEONET_6to7_100days_Times_fittolognormal.png", 1920,1080)
plot(EQ_lognormal)
dev.off()


EQ_beta<-fitdist(stand(inter), "beta", method = "mme")
png("GEONET_6to7_100days_Times_fittobeta.png", 1920,1080)
plot(EQ_beta)
dev.off()


rm(EQ_gamma2)
EQ_gamma2<- fitdist(inter,"gamma", "mme")
png("GEONET_6to7_100days_Times_fittogamma2.png", 1920,1080)
plot(EQ_gamma2)
dev.off()


#p-value
GOF_exp(inter,EQ_exp)           #[1] 0.1513808
GOF_lnorm(inter,EQ_lognormal)   #[1] 0.123288
GOF_beta(stand(inter), EQ_beta) #[1] 0.1590335
GOF_gamma(inter, EQ_gamma2)   #Dont believe

GOF2_exp(inter,EQ_exp)          #[1] 0.8550323
GOF2_lnorm(inter,EQ_lognormal)  #[1] 0.203755
GOF2_beta(stand(inter), EQ_beta)#[1] 0.6836444
GOF2_gamma(inter, EQ_gamma2)  #Dont believe
#Parameters



EQ_exp$estimate
#rate 
#3.411383e-06 

EQ_lognormal$estimate
#   meanlog      sdlog 
#12.2789001  0.7867563 

EQ_beta$estimate
#shape1    shape2 
#0.6769806 2.3255171 


EQ_gamma2$estimate
#       shape         rate 
#1.166803e+00 3.980411e-06 





#Now let us look at 500 days


arr<-read.csv('GEONET_4to5_500days.csv', header=FALSE)
A<-rep(1,length(arr))
for (i in 1:length(arr)){A[i]<-as.numeric(arr[2,i])}

A0<-A[order(A)] #Needed to add this to arrange the events in ascending order in order to actually 
inter<-rep(1,length(arr)-1)
for (i in 1:length(A0)-1){inter[i]<-A0[i+1]-A0[i]}


png("GEONET_4to5_500daysTimes.png", 1920,1080)
ggplot(data=as.data.frame(inter), aes(x=inter))+geom_histogram(aes(y=..density..),,color="blue", fill="black",bins = 35)+
  scale_color_brewer(palette="Dark2")+theme_fivethirtyeight()+
  ggtitle("Inter-arrival times for the past 500 days | Magn 4-5 GEONET")
dev.off()

png("GEONET_4to5_500days_Times_CullenFrey.png", 1920,1080)
descdist(data=inter, discrete = FALSE, boot=100, boot.col="red")
dev.off()

#Let us try to narrow down to the more likely distributions which I think are: exp and lognormal
EQ_exp <- fitdist(inter, "exp", method = 'mme')
png("GEONET_4to5_500days_Times_fittoexp.png", 1920,1080)
plot(EQ_exp)
dev.off()


EQ_lognormal <- fitdist(inter, "lnorm", method = 'mme')
png("GEONET_4to5_500days_Times_fittolognormal.png", 1920,1080)
plot(EQ_lognormal)
dev.off()


EQ_beta<-fitdist(stand(inter), "beta", method = "mme")
png("GEONET_4to5_500days_Times_fittobeta.png", 1920,1080)
plot(EQ_beta)
dev.off()

rm(EQ_gamma2)
EQ_gamma2<- fitdist(inter,"gamma", "mme")
png("GEONET_4to5_500days_Times_fittogamma2.png", 1920,1080)
plot(EQ_gamma2)
dev.off()


#p-value
GOF_exp(inter,EQ_exp)           #[1] 0.00368998
GOF_lnorm(inter,EQ_lognormal)   #[1] 0.0007730928
GOF_beta(stand(inter), EQ_beta) #[1] 0.1047346
GOF_gamma(inter, EQ_gamma2)

GOF2_exp(inter,EQ_exp)          #[1] 1.179052e-07
GOF2_lnorm(inter,EQ_lognormal)  
GOF2_beta(stand(inter), EQ_beta)#[1] 0.01702073
GOF2_gamma(inter, EQ_gamma2)



#Parameters
EQ_exp$estimate
#rate 
#4.430954e-05 

EQ_lognormal$estimate
#  meanlog     sdlog 
#9.5480813 0.9759397 

EQ_beta$estimate
#   shape1    shape2 
#0.4866773 5.1159602



EQ_gamma2$estimate
#       shape         rate 
#6.281116e-01 2.783134e-05 






#Now let us look at 500 days


arr<-read.csv('GEONET_4to5_500days.csv', header=FALSE)
A<-rep(1,length(arr))
for (i in 1:length(arr)){A[i]<-as.numeric(arr[2,i])}

A0<-A[order(A)] #Needed to add this to arrange the events in ascending order in order to actually 
inter<-rep(1,length(arr)-1)
for (i in 1:length(A0)-1){inter[i]<-A0[i+1]-A0[i]}


png("GEONET_4to5_500daysTimes.png", 1920,1080)
ggplot(data=as.data.frame(inter), aes(x=inter))+geom_histogram(aes(y=..density..),,color="blue", fill="black",bins = 35)+
  scale_color_brewer(palette="Dark2")+theme_fivethirtyeight()+
  ggtitle("Inter-arrival times for the past 500 days | Magn 4-5 GEONET")
dev.off()

png("GEONET_4to5_500days_Times_CullenFrey.png", 1920,1080)
descdist(data=inter, discrete = FALSE, boot=100, boot.col="red")
dev.off()

#Let us try to narrow down to the more likely distributions which I think are: exp and lognormal
EQ_exp <- fitdist(inter, "exp", method = 'mme')
png("GEONET_4to5_500days_Times_fittoexp.png", 1920,1080)
plot(EQ_exp)
dev.off()


EQ_lognormal <- fitdist(inter, "lnorm", method = 'mme')
png("GEONET_4to5_500days_Times_fittolognormal.png", 1920,1080)
plot(EQ_lognormal)
dev.off()


EQ_beta<-fitdist(stand(inter), "beta", method = "mme")
png("GEONET_4to5_500days_Times_fittobeta.png", 1920,1080)
plot(EQ_beta)
dev.off()

rm(EQ_gamma2)
EQ_gamma2<- fitdist(inter,"gamma", "mme")
png("GEONET_4to5_500days_Times_fittogamma2.png", 1920,1080)
plot(EQ_gamma2)
dev.off()


#p-value
GOF_exp(inter,EQ_exp)           #[1] 0.00368998
GOF_lnorm(inter,EQ_lognormal)   #[1] 0.0007730928
GOF_beta(stand(inter), EQ_beta) #[1] 0.1047346
GOF_gamma(inter, EQ_gamma2)

GOF2_exp(inter,EQ_exp)          #[1] 1.179052e-07
GOF2_lnorm(inter,EQ_lognormal)  
GOF2_beta(stand(inter), EQ_beta)#[1] 0.01702073
GOF2_gamma(inter, EQ_gamma2)



#Parameters
EQ_exp$estimate
#rate 
#4.430954e-05 

EQ_lognormal$estimate
#  meanlog     sdlog 
#9.5480813 0.9759397 

EQ_beta$estimate
#   shape1    shape2 
#0.4866773 5.1159602



EQ_gamma2$estimate
#       shape         rate 
#6.281116e-01 2.783134e-05 





#Now let us look at 500 days


arr<-read.csv('GEONET_4to5_500days.csv', header=FALSE)
A<-rep(1,length(arr))
for (i in 1:length(arr)){A[i]<-as.numeric(arr[2,i])}

A0<-A[order(A)] #Needed to add this to arrange the events in ascending order in order to actually 
inter<-rep(1,length(arr)-1)
for (i in 1:length(A0)-1){inter[i]<-A0[i+1]-A0[i]}


png("GEONET_4to5_500daysTimes.png", 1280,720)
ggplot(data=as.data.frame(inter), aes(x=inter))+geom_histogram(aes(y=..density..),,color="blue", fill="black",bins = 35)+
  scale_color_brewer(palette="Dark2")+theme_fivethirtyeight()+
  ggtitle("Inter-arrival times for the past 500 days | Magn 4-5 GEONET")
dev.off()

png("GEONET_4to5_500days_Times_CullenFrey.png", 1280,720)
descdist(data=inter, discrete = FALSE, boot=100, boot.col="red")
dev.off()

#Let us try to narrow down to the more likely distributions which I think are: exp and lognormal
EQ_exp <- fitdist(inter, "exp", method = 'mme')
png("GEONET_4to5_500days_Times_fittoexp.png", 1280,720)
plot(EQ_exp)
dev.off()


EQ_lognormal <- fitdist(inter, "lnorm", method = 'mme')
png("GEONET_4to5_500days_Times_fittolognormal.png", 1280,720)
plot(EQ_lognormal)
dev.off()


EQ_beta<-fitdist(stand(inter), "beta", method = "mme")
png("GEONET_4to5_500days_Times_fittobeta.png", 1280,720)
plot(EQ_beta)
dev.off()

rm(EQ_gamma2)
EQ_gamma2<- fitdist(inter,"gamma", "mme")
png("GEONET_4to5_500days_Times_fittogamma2.png", 1280,720)
plot(EQ_gamma2)
dev.off()


#p-value
GOF_exp(inter,EQ_exp)           #[1] 0.00368998
GOF_lnorm(inter,EQ_lognormal)   #[1] 0.0007730928
GOF_beta(stand(inter), EQ_beta) #[1] 0.1047346
GOF_gamma(inter, EQ_gamma2)

GOF2_exp(inter,EQ_exp)          #[1] 1.179052e-07
GOF2_lnorm(inter,EQ_lognormal)  
GOF2_beta(stand(inter), EQ_beta)#[1] 0.01702073
GOF2_gamma(inter, EQ_gamma2)



#Parameters
EQ_exp$estimate
#rate 
#4.430954e-05 

EQ_lognormal$estimate
#  meanlog     sdlog 
#9.5480813 0.9759397 

EQ_beta$estimate
#   shape1    shape2 
#0.4866773 5.1159602



EQ_gamma2$estimate
#       shape         rate 
#6.281116e-01 2.783134e-05 




summary(EQ_exp)[7:8]
summary(EQ_lognormal)[7:8]
summary(EQ_beta)[7:8]
summary(EQ_gamma2)[7:8]





#5to6 500 days


arr<-read.csv('GEONET_5to6_500days.csv', header=FALSE)
A<-rep(1,length(arr))
for (i in 1:length(arr)){A[i]<-as.numeric(arr[2,i])}

A0<-A[order(A)] #Needed to add this to arrange the events in ascending order in order to actually 
inter<-rep(1,length(arr)-1)
for (i in 1:length(A0)-1){inter[i]<-A0[i+1]-A0[i]}


png("GEONET_5to6_500daysTimes.png", 1280,720)
ggplot(data=as.data.frame(inter), aes(x=inter))+geom_histogram(aes(y=..density..),,color="blue", fill="black",bins = 35)+
  scale_color_brewer(palette="Dark2")+theme_fivethirtyeight()+
  ggtitle("Inter-arrival times for the past 500 days | Magn 5-6 GEONET")
dev.off()

png("GEONET_5to6_500days_Times_CullenFrey.png", 1280,720)
descdist(data=inter, discrete = FALSE, boot=100, boot.col="red")
dev.off()

#Let us try to narrow down to the more likely distributions which I think are: exp and lognormal
EQ_exp <- fitdist(inter, "exp", method = 'mme')
png("GEONET_5to6_500days_Times_fittoexp.png", 1280,720)
plot(EQ_exp)
dev.off()


EQ_lognormal <- fitdist(inter, "lnorm", method = 'mme')
png("GEONET_5to6_500days_Times_fittolognormal.png", 1280,720)
plot(EQ_lognormal)
dev.off()


EQ_beta<-fitdist(stand(inter), "beta", method = "mme")
png("GEONET_5to6_500days_Times_fittobeta.png", 1280,720)
plot(EQ_beta)
dev.off()

rm(EQ_gamma2)
EQ_gamma2<- fitdist(inter,"gamma", "mme")
png("GEONET_5to6_500days_Times_fittogamma2.png", 1280,720)
plot(EQ_gamma2)
dev.off()


#p-value
GOF_exp(inter,EQ_exp)           #[1] 0.02500218
GOF_lnorm(inter,EQ_lognormal)   #[1] 0.003555106
GOF_beta(stand(inter), EQ_beta) #[1] 0.1012763
GOF_gamma(inter, EQ_gamma2)     #[1] 1.149751e-64

GOF2_exp(inter,EQ_exp)          #[1] 0.004130509
GOF2_lnorm(inter,EQ_lognormal)  #[1] 5.521583e-12
GOF2_beta(stand(inter), EQ_beta)#[1] 0.01727362
GOF2_gamma(inter, EQ_gamma2)



#Parameters
EQ_exp$estimate
# rate 
# 1.797879e-05 

EQ_lognormal$estimate
# meanlog      sdlog 
# 10.5116444  0.9106848 

EQ_beta$estimate
# shape1    shape2 
# 0.5150377 3.0361341 


EQ_gamma2$estimate
# shape        rate 
# 7.74101e-01 1.39174e-05



summary(EQ_exp)[7:8]
summary(EQ_lognormal)[7:8]
summary(EQ_beta)[7:8]
summary(EQ_gamma2)[7:8]





#5to6 500 days


arr<-read.csv('GEONET_5to6_500days.csv', header=FALSE)
A<-rep(1,length(arr))
for (i in 1:length(arr)){A[i]<-as.numeric(arr[2,i])}

A0<-A[order(A)] #Needed to add this to arrange the events in ascending order in order to actually 
inter<-rep(1,length(arr)-1)
for (i in 1:length(A0)-1){inter[i]<-A0[i+1]-A0[i]}


png("GEONET_5to6_500daysTimes.png", 1280,720)
ggplot(data=as.data.frame(inter), aes(x=inter))+geom_histogram(aes(y=..density..),,color="blue", fill="black",bins = 35)+
  scale_color_brewer(palette="Dark2")+theme_fivethirtyeight()+
  ggtitle("Inter-arrival times for the past 500 days | Magn 5-6 GEONET")
dev.off()

png("GEONET_5to6_500days_Times_CullenFrey.png", 1280,720)
descdist(data=inter, discrete = FALSE, boot=100, boot.col="red")
dev.off()

#Let us try to narrow down to the more likely distributions which I think are: exp and lognormal
EQ_exp <- fitdist(inter, "exp", method = 'mme')
png("GEONET_5to6_500days_Times_fittoexp.png", 1280,720)
plot(EQ_exp)
dev.off()


EQ_lognormal <- fitdist(inter, "lnorm", method = 'mme')
png("GEONET_5to6_500days_Times_fittolognormal.png", 1280,720)
plot(EQ_lognormal)
dev.off()


EQ_beta<-fitdist(stand(inter), "beta", method = "mme")
png("GEONET_5to6_500days_Times_fittobeta.png", 1280,720)
plot(EQ_beta)
dev.off()

rm(EQ_gamma2)
EQ_gamma2<- fitdist(inter,"gamma", "mme")
png("GEONET_5to6_500days_Times_fittogamma2.png", 1280,720)
plot(EQ_gamma2)
dev.off()


#p-value
GOF_exp(inter,EQ_exp)           #[1] 0.02500218
GOF_lnorm(inter,EQ_lognormal)   #[1] 0.003555106
GOF_beta(stand(inter), EQ_beta) #[1] 0.1012763
GOF_gamma(inter, EQ_gamma2)     #[1] 1.149751e-64

GOF2_exp(inter,EQ_exp)          #[1] 0.004130509
GOF2_lnorm(inter,EQ_lognormal)  #[1] 5.521583e-12
GOF2_beta(stand(inter), EQ_beta)#[1] 0.01727362
GOF2_gamma(inter, EQ_gamma2)



#Parameters
EQ_exp$estimate
# rate 
# 1.797879e-05 

EQ_lognormal$estimate
# meanlog      sdlog 
# 10.5116444  0.9106848 

EQ_beta$estimate
# shape1    shape2 
# 0.5150377 3.0361341 


EQ_gamma2$estimate
# shape        rate 
# 7.74101e-01 1.39174e-05



summary(EQ_exp)[7:8]
summary(EQ_lognormal)[7:8]
summary(EQ_beta)[7:8]
summary(EQ_gamma2)[7:8]





#6to7 500 days


arr<-read.csv('GEONET_6to7_500days.csv', header=FALSE)
A<-rep(1,length(arr))
for (i in 1:length(arr)){A[i]<-as.numeric(arr[2,i])}

A0<-A[order(A)] #Needed to add this to arrange the events in ascending order in order to actually 
inter<-rep(1,length(arr)-1)
for (i in 1:length(A0)-1){inter[i]<-A0[i+1]-A0[i]}


png("GEONET_6to7_500daysTimes.png", 1280,720)
ggplot(data=as.data.frame(inter), aes(x=inter))+geom_histogram(aes(y=..density..),,color="blue", fill="black",bins = 35)+
  scale_color_brewer(palette="Dark2")+theme_fivethirtyeight()+
  ggtitle("Inter-arrival times for the past 500 days | Magn 6-7 GEONET")
dev.off()

png("GEONET_6to7_500days_Times_CullenFrey.png", 1280,720)
descdist(data=inter, discrete = FALSE, boot=100, boot.col="red")
dev.off()

#Let us try to narrow down to the more likely distributions which I think are: exp and lognormal
EQ_exp <- fitdist(inter, "exp", method = 'mme')
png("GEONET_6to7_500days_Times_fittoexp.png", 1280,720)
plot(EQ_exp)
dev.off()


EQ_lognormal <- fitdist(inter, "lnorm", method = 'mme')
png("GEONET_6to7_500days_Times_fittolognormal.png", 1280,720)
plot(EQ_lognormal)
dev.off()


EQ_beta<-fitdist(stand(inter), "beta", method = "mme")
png("GEONET_6to7_500days_Times_fittobeta.png", 1280,720)
plot(EQ_beta)
dev.off()

rm(EQ_gamma2)
EQ_gamma2<- fitdist(inter,"gamma", "mme")
png("GEONET_6to7_500days_Times_fittogamma2.png", 1280,720)
plot(EQ_gamma2)
dev.off()


#p-value
GOF_exp(inter,EQ_exp)           #[1] 0.02500218
GOF_lnorm(inter,EQ_lognormal)   #[1] 0.003555106
GOF_beta(stand(inter), EQ_beta) #[1] 0.1012763
GOF_gamma(inter, EQ_gamma2)     #[1] 1.149751e-64

GOF2_exp(inter,EQ_exp)          #[1] 0.004130509
GOF2_lnorm(inter,EQ_lognormal)  #[1] 5.521583e-12
GOF2_beta(stand(inter), EQ_beta)#[1] 0.01727362
GOF2_gamma(inter, EQ_gamma2)



#Parameters
EQ_exp$estimate


EQ_lognormal$estimate


EQ_beta$estimate



EQ_gamma2$estimate




summary(EQ_exp)[7:8]
summary(EQ_lognormal)[7:8]
summary(EQ_beta)[7:8]
summary(EQ_gamma2)[7:8]



