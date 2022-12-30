setwd('F:/Uni/Esci451/Assignment 2/')
library(fitdistrplus)
library(logspline)
library(ggthemes)
library(ggplot2)
library(CDFt)


#Lets look at the magnitudes
data<-read.csv('IRIS_Start4112022_100days_Above3_Magn.csv', header=FALSE)
V<-rep(1,length(data)) #Change number 
for (i in 1:length(data)){V[i]<-as.numeric(data[2,i])}

#Define standardizing function
stand<-function(vector){
  return((vector-min(vector))/(range(vector)[2]-range(vector)[1]))
}

hist(V)

ggplot(data=as.data.frame(V), aes(x=V))+geom_histogram(aes(y=..density..),,color="blue", fill="black",bins = 35)+
  geom_density(alpha=.4, fill="#34bab3") + scale_color_brewer(palette="Dark2")+theme_fivethirtyeight()+
  ggtitle("IRIS Magnitudes for the past 100 days | End date:11th April 2022")

par(mfrow=c(1,1))
descdist(data=V, discrete = FALSE, boot=100)


#Exponential fit to magnitude
EQ_exp <- fitdist(V, "exp", method = 'mme')
png("IRIS/IRIS_100days_Magnitude_fittoexp.png", 1920,1080)
plot(EQ_exp)
dev.off()

#Let us try to fit with gamma

EQ_gamma<- fitdist(V,"gamma")
plot(EQ_gamma)

#Let's start saving these images via the code itself! After all, coding is about convenience!
png("IRIS/IRIS_100days_Magnitude_fittogamma.png", 1920,1080)
plot(EQ_gamma)
dev.off()


EQ_Normal<- fitdist(V,"norm")
png("IRIS/IRIS_100days_Magnitude_fittonormal.png", 1920,1080)
plot(EQ_Normal)
dev.off()



EQ_beta<-fitdist(stand(V), "beta", method = "mme")
png("IRIS/IRIS_100days_Magnitude_fittobeta.png", 1920,1080)
plot(EQ_beta)
dev.off()

EQ_unif<-fitdist(V,"unif")
png("IRIS/IRIS_100days_Magnitude_fittouniform.png", 1920,1080)
plot(EQ_unif)
dev.off()


EQ_log<-fitdist(V,"logis")
png("IRIS/IRIS_100days_Magnitude_fittolog.png", 1920,1080)
plot(EQ_log)
dev.off()



##Let's quantify the goodness of fits!
#Goodness of fit p value returns

#First we are going to have a go at 



GOF_log<-function(vector,test, no = 1000){
  location<-summary(test)$estimate[1]
  scale<-summary(test)$estimate[2]
  y<-rlogis(no, location,scale)
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



GOF_exp<-function(vector,test, no = 1000){
  y<-rexp(no, test$estimate)
  res<-CramerVonMisesTwoSamples(vector,y)
  return(1/6*exp(-res))
}


GOF_beta<-function(vector,test, no = 1000){
  y<-rbeta(no, test$estimate[1], test$estimate[2])
  res<-CramerVonMisesTwoSamples(vector,y)
  return(1/6*exp(-res))
}



GOF_norm<-function(vector,test, no = 1000){
  y<-rnorm(no, test$estimate[1], test$estimate[2])
  res<-CramerVonMisesTwoSamples(vector,y)
  return(1/6*exp(-res))
}

GOF_unif<-function(vector,test, no = 1000){
  y<-runif(no, test$estimate[1], test$estimate[2])
  res<-CramerVonMisesTwoSamples(vector,y)
  return(1/6*exp(-res))
}


#Alright let us try the Kolmogorov-Smirnov test instead for a different set of p values


GOF2_log<-function(vector,test, no = 1000){
  location<-summary(test)$estimate[1]
  scale<-summary(test)$estimate[2]
  y<-rlogis(no, location,scale)
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


GOF2_norm<-function(vector,test, no = 1000){
  y<-rnorm(no, test$estimate[1], test$estimate[2])
  res<-ks.test(vector,y)
  return(res$p.value)
}

GOF2_unif<-function(vector,test, no = 1000){
  y<-runif(no, test$estimate[1], test$estimate[2])
  res<-ks.test(vector,y)
  return(res$p.value)
}


#Still beta and normal trailing behind in terms of p value. Under a 5% significance test, only beta distribution would work
GOF_gamma(V, EQ_gamma)  #[1] 3.344355e-122
GOF_exp(V,EQ_exp)      #[1] 6.230795e-26
GOF_beta(stand(V),EQ_beta)  #[1] 9.675588e-05
GOF_norm(V,EQ_Normal)    #[1] 0.006074728
GOF_unif(V,EQ_unif)     #[1] 1.310409e-26
GOF_log(V,EQ_log)       #[1] 0.0135529

#Mostly zeros
GOF2_gamma(V, EQ_gamma)
GOF2_exp(V,EQ_exp)
GOF2_beta(stand(V),EQ_beta)  
GOF2_norm(V,EQ_Normal)       
GOF2_unif(V,EQ_unif)
GOF2_log(V,EQ_log)    #[1] 3.583925e-08




#Generally there appears to be a seriously low set of p values returned for the Kolmogorov-Smirnov tests





##250 days set of data
#let us try doing the same for 250 days of data instead of 100. Do we get any improvement in fits?


data<-read.csv('IRIS_Start4112022_250days_Above3_Magn.csv', header=FALSE)
V<-rep(1,length(data)) #Change number 
for (i in 1:length(data)){V[i]<-as.numeric(data[2,i])}

ggplot(data=as.data.frame(V), aes(x=V))+geom_histogram(aes(y=..density..),,color="blue", fill="black",bins = 35)+
  geom_density(alpha=.4, fill="#34bab3") + scale_color_brewer(palette="Dark2")+theme_fivethirtyeight()+
  ggtitle("IRIS Magnitudes for the past 250 days | End date:11th April 2022")

#Lets have a look at some other possibilities w a Cullen-Frey 
par(mfrow=c(1,1))
png("IRIS/IRIS_250days_CullenFreyplot_Magnitude.png", 1920,1080)
descdist(data=V, discrete = FALSE, boot=100)
dev.off()
#Let us now plot the fits

#Exponential fit to magnitude
EQ_exp <- fitdist(V, "exp", method = 'mme')
png("IRIS/IRIS_250days_Magnitude_fittoexp.png", 1920,1080)
plot(EQ_exp)
dev.off()

#Let us try to fit with gamma

EQ_gamma<- fitdist(V,"gamma")
plot(EQ_gamma)

#Let's start saving these images via the code itself! After all, coding is about convenience!
png("IRIS/IRIS_250days_Magnitude_fittogamma.png", 1920,1080)
plot(EQ_gamma)
dev.off()


EQ_Normal<- fitdist(V,"norm")
png("IRIS/IRIS_250days_Magnitude_fittonormal.png", 1920,1080)
plot(EQ_Normal)
dev.off()



EQ_beta<-fitdist(stand(V), "beta", method = "mme")
png("IRIS/IRIS_250days_Magnitude_fittobeta.png", 1920,1080)
plot(EQ_beta)
dev.off()

EQ_unif<-fitdist(V,"unif")
png("IRIS/IRIS_250days_Magnitude_fittouniform.png", 1920,1080)
plot(EQ_unif)
dev.off()


EQ_log<-fitdist(V,"logis")
png("IRIS/IRIS_250days_Magnitude_fittolog.png", 1920,1080)
plot(EQ_log)
dev.off()


#Still beta and normal trailing behind in terms of p value. Under a 5% significance test, only beta distribution would work
GOF_gamma(V, EQ_gamma)  #[1] 3.170019e-136
GOF_exp(V,EQ_exp)      #[1] 1.369956e-32
GOF_beta(stand(V),EQ_beta)  #[1] 7.65712e-19
GOF_norm(V,EQ_Normal)    #[1] 4.224007e-14
GOF_unif(V,EQ_unif)     #[1] 8.629215e-43
GOF_log(V,EQ_log)      #[1] 0.01690593

#Mostly zeros
GOF2_gamma(V, EQ_gamma)
GOF2_exp(V,EQ_exp)
GOF2_beta(stand(V),EQ_beta)  
GOF2_norm(V,EQ_Normal)      
GOF2_unif(V,EQ_unif)
GOF2_log(V,EQ_log)    #[1] 5.985545e-12



##500 days


data<-read.csv('IRIS_Start4112022_500days_Above3_Magn.csv', header=FALSE)
V<-rep(1,length(data)) #Change number 
for (i in 1:length(data)){V[i]<-as.numeric(data[2,i])}

ggplot(data=as.data.frame(V), aes(x=V))+geom_histogram(aes(y=..density..),,color="blue", fill="black",bins = 35)+
  geom_density(alpha=.4, fill="#34bab3") + scale_color_brewer(palette="Dark2")+theme_fivethirtyeight()+
  ggtitle("IRIS Magnitudes for the past 500 days | End date:11th April 2022")

#Lets have a look at some other possibilities w a Cullen-Frey 
par(mfrow=c(1,1))
png("IRIS/IRIS_500days_CullenFreyplot_Magnitude.png", 1920,1080)
descdist(data=V, discrete = FALSE, boot=100)
dev.off()

#Let us now plot the fits

#Exponential fit to magnitude
EQ_exp <- fitdist(V, "exp", method = 'mme')
png("IRIS/IRIS_500days_Magnitude_fittoexp.png", 1920,1080)
plot(EQ_exp)
dev.off()


#Let us try to fit with gamma

EQ_gamma<- fitdist(V,"gamma")
png("IRIS/IRIS_500days_Magnitude_fittogamma.png", 1920,1080)
plot(EQ_gamma)
dev.off()


EQ_Normal<- fitdist(V,"norm")
png("IRIS/IRIS_500days_Magnitude_fittonormal.png", 1920,1080)
plot(EQ_Normal)
dev.off()



EQ_beta<-fitdist(stand(V), "beta", method = "mme")
png("IRIS/IRIS_500days_Magnitude_fittobeta.png", 1920,1080)
plot(EQ_beta)
dev.off()

EQ_unif<-fitdist(V,"unif")
png("IRIS/IRIS_500days_Magnitude_fittouniform.png", 1920,1080)
plot(EQ_unif)
dev.off()


EQ_log<-fitdist(V,"logis")
png("IRIS/IRIS_500days_Magnitude_fittolog.png", 1920,1080)
plot(EQ_log)
dev.off()


#Still beta and normal trailing behind in terms of p value. Under a 5% significance test, only beta distribution would work
GOF_gamma(V, EQ_gamma)  #[1] 4.629252e-141
GOF_exp(V,EQ_exp)      #[1] 5.725536e-33
GOF_beta(stand(V),EQ_beta)  #[1] 0.0004625718
GOF_norm(V,EQ_Normal)    #[1] 0.002883452
GOF_unif(V,EQ_unif)     #[1] 5.962226e-40
GOF_log(V,EQ_log)      #[1] 0.003897855

#Mostly zeros
GOF2_gamma(V, EQ_gamma)
GOF2_exp(V,EQ_exp)
GOF2_beta(stand(V),EQ_beta)  
GOF2_norm(V,EQ_Normal)       
GOF2_unif(V,EQ_unif)
GOF2_log(V,EQ_log)          #[1] 4.162618e-09






