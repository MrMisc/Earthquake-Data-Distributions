setwd('F:/Uni/Esci451/Assignment 2/')
library(fitdistrplus)
library(logspline)
library(ggthemes)
library(ggplot2)


#Lets look at the magnitudes
data<-read.csv('GEONET_Start4102022_100days_Above4_Magn.csv', header=FALSE)
V<-rep(1,length(data)) #Change number 
for (i in 1:length(data)){V[i]<-as.numeric(data[2,i])}

descdist(data=V, discrete = FALSE, boot=100)

hist(V)


#Exponential fit to magnitude
EQ_exp <- fitdist(V, "exp", method = 'qme')
plot(EQ_exp)  #Doesn't seem very good at all
EQ_exp


#Let us try to fit with gamma

EQ_gamma<- fitdist(V,"gamma")
plot(EQ_gamma)


#Beta has always been tempting despite the fact that the distribution is only applicable for range [0,1]
#What if we tried scaling the data down to that range? Even if it fit, is there a meaning or any significance
#to a well fitted relationship with beta distribution for EQ magnitudes?

rangeofV<- range(V)[2] - range(V)[1]
V_standardized <- (V-min(V))/rangeofV

range(V_standardized)


EQ_beta <- fitdist(V_standardized, "beta", "mge")
plot(EQ_beta)




##Let's quickly compare gamma fit with this standardized eq catalogue instead to compare between the 2
EQ_gamma_standardized <- fitdist(V_standardized, "gamma", "mme")
plot(EQ_gamma_standardized)



#Goodness of fit parameter






















data<-read.csv('IRIS_mags_28days_min4.csv', header=FALSE)
V<-rep(1,length(data)) #Change number 
for (i in 1:length(data)){V[i]<-as.numeric(data[2,i])}

descdist(data=V, discrete = FALSE, boot=1000)
hist(V)

#To compare and fit with distribution

EQ_exp <- fitdist(V, "exp", method = "mge")
plot(EQ_exp)


summary(EQ_exp)


EQ_gamma <- fitdist(V, "gamma")
plot(EQ_gamma)


EQ


##INTER ARRIVAL TIMES | POSSIBLY COUNT DATA IN FUTURE
#7 days | mag 4-5
setwd('F:/Uni/GPHS445/Watch/')

#These are the estimated occurrence times of earthquakes between magnitudes 4 and 5 in the 
arr<-read.csv('times.csv', header=FALSE)
A<-rep(1,length(arr))
for (i in 1:length(arr)){A[i]<-as.numeric(arr[2,i])}

inter<-rep(1,length(arr)-1)
for (i in 1:length(A)-1){inter[i]<-A[i]-A[i+1]}

ggplot(data=as.data.frame(inter), aes(x=inter))+geom_histogram(aes(y=..density..),,color="blue", fill="black",bins = 35)+
  geom_density(alpha=.4, fill="#34bab3") + scale_color_brewer(palette="Dark2")+theme_fivethirtyeight()+
  ggtitle("Inter-arrival times for the past 7 days | Magn 4-5")
hist(inter)

descdist(data=inter, discrete = FALSE, boot=100)


interarrivals<-fitdist(inter,"exp",method="mme")
plot(interarrivals)

summary(interarrivals)

#7days | mag 5 -6 

higher_arr<-read.csv('times_5to6_7days_IRIS.csv', header=FALSE)
A<-rep(1,length(higher_arr))
for (i in 1:length(higher_arr)){A[i]<-as.numeric(higher_arr[2,i])}

inter<-rep(1,length(higher_arr)-1)
for (i in 1:length(A)-1){inter[i]<-A[i]-A[i+1]}


library(ggplot2)
ggplot2.histogram(data=inter, xName='weight',
                  fill="white", color="black",
                  addDensityCurve=TRUE, densityFill='#FF6666')
hist(inter)

descdist(data=inter, discrete = FALSE, boot=100)


interarrivals<-fitdist(inter,"exp",method="mme")
plot(interarrivals)

summary(interarrivals)


#28 days | mag 5-6


higher_arr<-read.csv('times_5to6_28days_IRIS.csv', header=FALSE)
A<-rep(1,length(higher_arr))
for (i in 1:length(higher_arr)){A[i]<-as.numeric(higher_arr[2,i])}

inter<-rep(1,length(higher_arr)-1)
for (i in 1:length(A)-1){inter[i]<-A[i]-A[i+1]}


ggplot(data=as.data.frame(inter), aes(x=inter))+geom_histogram(aes(y=..density..),,color="blue", fill="black",bins = 50)+
  geom_density(alpha=.4, fill="#34bab3") + scale_color_brewer(palette="Dark2")+theme_fivethirtyeight()+
  ggtitle("Inter-arrival times for the past 28 days | Magn 5-6")


hist(inter)

descdist(data=inter, discrete = FALSE, boot=100)


interarrivals<-fitdist(inter,"exp",method="mme")
plot(interarrivals)

summary(interarrivals)



interarrivals<-fitdist(inter,"gamma",method="mme")
plot(interarrivals)







