setwd('F:/Uni/Esci451/Assignment 2/')
library(fitdistrplus)
library(logspline)
library(ggthemes)
library(ggplot2)
library(CDFt)


#Lets look at the magnitudes
data<-read.csv('GEONET_Start4112022_100days_Above3_Magn.csv', header=FALSE)
V<-rep(1,length(data)) #Change number 
for (i in 1:length(data)){V[i]<-as.numeric(data[2,i])}

#Define standardizing function
stand<-function(vector){
  return((vector-min(vector))/(range(vector)[2]-range(vector)[1]))
}

hist(V)

ggplot(data=as.data.frame(V), aes(x=V))+geom_histogram(aes(y=..density..),,color="blue", fill="black",bins = 35)+
  geom_density(alpha=.4, fill="#34bab3") + scale_color_brewer(palette="Dark2")+theme_fivethirtyeight()+
  ggtitle("GEONET Magnitudes for the past 100 days | End date:11th April 2022")



#Exponential fit to magnitude
EQ_exp <- fitdist(V, "exp", method = 'mme')
plot(EQ_exp)  #Doesn't seem very good at all
EQ_exp


#Let us try to fit with gamma

EQ_gamma<- fitdist(V,"gamma")
plot(EQ_gamma)



#Lets have a look at some other possibilities w a Cullen-Frey plot
descdist(data=V, discrete = FALSE, boot=100)



EQ_Normal<- fitdist(V,"norm")
plot(EQ_Normal)


EQ_beta<-fitdist(stand(V), "beta", method = "mme")
plot(EQ_beta)

EQ_unif<-fitdist(V,"unif")
plot(EQ_unif)


##Let's quantify the goodness of fits!
#Goodness of fit p value returns

#First we are going to have a go at 

GOF_gamma<-function(vector,test, no = 1000){
  shape<-summary(test)$estimate[1]
  scale<-1/summary(test)$estimate[2]
  y<-rgamma(no, shape,scale)
  res<-CramerVonMisesTwoSamples(vector,y)
  return(1/6*exp(-res))
}


#summary(EQ_gamma)$estimate[2]

GOF_gamma(V, EQ_gamma)
#[1] 5.849097e-74

GOF_exp<-function(vector,test, no = 1000){
  y<-rexp(no, test$estimate)
  res<-CramerVonMisesTwoSamples(vector,y)
  return(1/6*exp(-res))
}

GOF_exp(V,EQ_exp)
#[1] 1.045531e-16


GOF_beta<-function(vector,test, no = 1000){
  y<-rbeta(no, test$estimate[1], test$estimate[2])
  res<-CramerVonMisesTwoSamples(vector,y)
  return(1/6*exp(-res))
}

GOF_beta(stand(V),EQ_beta)
#[1] 0.07110096



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


GOF_norm(V,EQ_Normal) #[1] 0.08377379
GOF_unif(V,EQ_unif)   #[1] 3.760529e-11


#Alright let us try the Kolmogorov-Smirnov test instead for a different set of p values

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


#> GOF2_gamma(V,EQ_gamma)
#[1] 0

#> GOF2_exp(V,EQ_exp)
#[1] 0

#> GOF2_beta(stand(V),EQ_beta)
#[1] 0.004584417

#> GOF2_norm(V,EQ_Normal)
#[1] 1.238236e-05




#Generally there appears to be a seriously low set of p values returned for the Kolmogorov-Smirnov tests





##250 days set of data
#let us try doing the same for 250 days of data instead of 100. Do we get any improvement in fits?


data<-read.csv('GEONET_Start4112022_250days_Above3_Magn.csv', header=FALSE)
V<-rep(1,length(data)) #Change number 
for (i in 1:length(data)){V[i]<-as.numeric(data[2,i])}

ggplot(data=as.data.frame(V), aes(x=V))+geom_histogram(aes(y=..density..),,color="blue", fill="black",bins = 35)+
  geom_density(alpha=.4, fill="#34bab3") + scale_color_brewer(palette="Dark2")+theme_fivethirtyeight()+
  ggtitle("GEONET Magnitudes for the past 250 days | End date:11th April 2022")

#Lets have a look at some other possibilities w a Cullen-Frey 
par(mfrow=c(1,1))
descdist(data=V, discrete = FALSE, boot=100)

#Let us now plot the fits

#Exponential fit to magnitude
EQ_exp <- fitdist(V, "exp", method = 'mme')
png("GEONET_250days_Magnitude_fittoexp.png", 1920,1080)
plot(EQ_exp)
dev.off()

#Let us try to fit with gamma

EQ_gamma<- fitdist(V,"gamma")
plot(EQ_gamma)

#Let's start saving these images via the code itself! After all, coding is about convenience!
png("GEONET_250days_Magnitude_fittogamma.png", 1920,1080)
plot(EQ_gamma)
dev.off()


EQ_Normal<- fitdist(V,"norm")
png("GEONET_250days_Magnitude_fittonormal.png", 1920,1080)
plot(EQ_Normal)
dev.off()



EQ_beta<-fitdist(stand(V), "beta", method = "mme")
png("GEONET_250days_Magnitude_fittobeta.png", 1920,1080)
plot(EQ_beta)
dev.off()

EQ_unif<-fitdist(V,"unif")
png("GEONET_250days_Magnitude_fittouniform.png", 1920,1080)
plot(EQ_unif)
dev.off()



#Still beta and normal trailing behind in terms of p value. Under a 5% significance test, only beta distribution would work
GOF_gamma(V, EQ_gamma)  #[1] 5.509742e-102
GOF_exp(V,EQ_exp)      #[1] 6.349683e-19
GOF_beta(stand(V),EQ_beta)  #[1] 0.09016979
GOF_norm(V,EQ_Normal)    #[1] 0.03732831
GOF_unif(V,EQ_unif)     #[1] 1.047303e-21


#Mostly zeros
GOF2_gamma(V, EQ_gamma)
GOF2_exp(V,EQ_exp)
GOF2_beta(stand(V),EQ_beta)  #[1] 0.002590611
GOF2_norm(V,EQ_Normal)       #[1] 5.467637e-05
GOF2_unif(V,EQ_unif)




##500 days


data<-read.csv('GEONET_Start4112022_500days_Above3_Magn.csv', header=FALSE)
V<-rep(1,length(data)) #Change number 
for (i in 1:length(data)){V[i]<-as.numeric(data[2,i])}

ggplot(data=as.data.frame(V), aes(x=V))+geom_histogram(aes(y=..density..),,color="blue", fill="black",bins = 35)+
  geom_density(alpha=.4, fill="#34bab3") + scale_color_brewer(palette="Dark2")+theme_fivethirtyeight()+
  ggtitle("GEONET Magnitudes for the past 500 days | End date:11th April 2022")

#Lets have a look at some other possibilities w a Cullen-Frey 
par(mfrow=c(1,1))
descdist(data=V, discrete = FALSE, boot=100)

#Let us now plot the fits

#Exponential fit to magnitude
EQ_exp <- fitdist(V, "exp", method = 'mme')
png("GEONET_500days_Magnitude_fittoexp.png", 1280,720)
plot(EQ_exp)
dev.off()


#Let us try to fit with gamma

EQ_gamma<- fitdist(V,"gamma")
png("GEONET_500days_Magnitude_fittogamma.png", 1280,720)
plot(EQ_gamma)
dev.off()


EQ_Normal<- fitdist(V,"norm")
png("GEONET_500days_Magnitude_fittonormal.png", 1920,1080)
plot(EQ_Normal)
dev.off()



EQ_beta<-fitdist(stand(V), "beta", method = "mme")
png("GEONET_500days_Magnitude_fittobeta.png", 1280,720)
plot(EQ_beta)
dev.off()

EQ_unif<-fitdist(V,"unif")
png("GEONET_500days_Magnitude_fittouniform.png", 1920,1080)
plot(EQ_unif)
dev.off()



#Still beta and normal trailing behind in terms of p value. Under a 5% significance test, only beta distribution would work
GOF_gamma(V, EQ_gamma)  #[1] 1.053101e-126
GOF_exp(V,EQ_exp)      #[1] 3.284351e-24
GOF_beta(stand(V),EQ_beta)  #[1] 0.1016996
GOF_norm(V,EQ_Normal)    #[1] 0.01429841
GOF_unif(V,EQ_unif)     #[1] 3.625333e-42


#Mostly zeros
GOF2_gamma(V, EQ_gamma)
GOF2_exp(V,EQ_exp)
GOF2_beta(stand(V),EQ_beta)  #[1] 0.02073635
GOF2_norm(V,EQ_Normal)       #[1] 2.431721e-12
GOF2_unif(V,EQ_unif)


EQ_beta$estimate




