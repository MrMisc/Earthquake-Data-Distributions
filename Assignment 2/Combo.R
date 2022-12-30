setwd('F:/Uni/Esci451/Assignment 2/')
library(fitdistrplus)
library(logspline)
library(ggthemes)
library(ggplot2)
library(CDFt)

#Define standardizing function
stand<-function(vector){
  return((vector-min(vector))/(range(vector)[2]-range(vector)[1]))
}

#Lets look at the magnitudes
data<-read.csv('GEONETCombo_15422_100days.csv', header=FALSE)



V<-rep(1,length(data)) #Change number 
for (i in 1:length(data)){V[i]<-as.numeric(data[2,i])}

A<-rep(1,length(data))
for (i in 1:length(data)){A[i]<-as.numeric(data[4,i])}

#Just making in case
count<-rep(0,100)
DAY<-24*60^2
for(i in 1:length(count)){
  count[i] = length(A[A<=DAY*(i+1) & A>=DAY*i])
}



#order the time in ascending order and then order the magnitudes based off of that
Time<-A[order(A)]
Magnitude<-V[order(A)]
InterArrival_Times<-c(Time[1],Time[2:length(Time)] - Time[1:length(Time)-1])
Grade<-rep(0,length(Time))
for(i in 1:length(Time)){
  if(Magnitude[i]>=4 & Magnitude[i]<5){
    Grade[i] = '>4'
  }
  else if(Magnitude[i]>=5 & Magnitude[i]<6){
    Grade[i] = '>5'
  }
  else if(Magnitude[i]>=6 & Magnitude[i]<7){
    Grade[i] = '>6'
  }
  else if(Magnitude[i]>=7){
    Grade[i] = '>6'
  }
  else{
    Grade[i] = '>3'
  }
}

df<-data.frame(Magnitude,Time,InterArrival_Times,Grade)



png("GEONET_count_100days_InterarrivalTimes_Magnitude.png", 720,480)
ggplot(data=df, aes(x=InterArrival_Times, color = Grade))+geom_histogram(aes(y=..density.., fill = Grade),bins = 35)+
  geom_density(aes(y=..density..),color="darkseagreen4",size=1)+theme_fivethirtyeight()+
  ggtitle("Interarrival times | GEONET 100 Days to 15/4/22")
dev.off()


png("GEONET_count_100days_InterarrivalTimes_FACET_Magnitude.png", 720,480)
ggplot(data=df, aes(x=InterArrival_Times, color = Grade))+geom_histogram(aes(y=..density.., fill = Grade),bins = 35)+
  geom_density(aes(y=..density..),color="dodgerblue2",size=.75) +facet_wrap(Grade ~ .,shrink = FALSE)+theme_fivethirtyeight()+
  ggtitle("Facetted interrrival times >3 | GEONET 100 days to 15/4/22")
dev.off()




nBinom<-glm.nb(InterArrival_Times~Magnitude, data=df)
summary(nBinom)














# GEONET 500 days


data<-read.csv('GEONETCombo_15422_500days.csv', header=FALSE)



V<-rep(1,length(data)) #Change number 
for (i in 1:length(data)){V[i]<-as.numeric(data[2,i])}

A<-rep(1,length(data))
for (i in 1:length(data)){A[i]<-as.numeric(data[4,i])}

#Just making in case
count<-rep(0,100)
DAY<-24*60^2
for(i in 1:length(count)){
  count[i] = length(A[A<=DAY*(i+1) & A>=DAY*i])
}



#order the time in ascending order and then order the magnitudes based off of that
Time<-A[order(A)]
Magnitude<-V[order(A)]
InterArrival_Times<-c(Time[1],Time[2:length(Time)] - Time[1:length(Time)-1])
Grade<-rep(0,length(Time))
for(i in 1:length(Time)){
  if(Magnitude[i]>=4 & Magnitude[i]<5){
    Grade[i] = '>4'
  }
  else if(Magnitude[i]>=5 & Magnitude[i]<6){
    Grade[i] = '>5'
  }
  else if(Magnitude[i]>=6 & Magnitude[i]<7){
    Grade[i] = '>6'
  }
  else if(Magnitude[i]>=7){
    Grade[i] = '>7'
  }
  else{
    Grade[i] = '>3'
  }
}

df<-data.frame(Magnitude,Time,InterArrival_Times,Grade)



png("GEONET_count_500days_InterarrivalTimes_Magnitude.png", 720,480)
ggplot(data=df, aes(x=InterArrival_Times, color = Grade))+geom_histogram(aes(y=..density.., fill = Grade),bins = 35)+
  geom_density(aes(y=..density..),color="darkseagreen4",size=1)+theme_fivethirtyeight()+
  ggtitle("Interarrival times | GEONET 500 Days to 15/4/22")
dev.off()


png("GEONET_count_500days_InterarrivalTimes_FACET_Magnitude.png", 720,480)
ggplot(data=df, aes(x=InterArrival_Times, color = Grade))+geom_histogram(aes(y=..density.., fill = Grade),bins = 35)+
  geom_density(aes(y=..density..),color="dodgerblue2",size=.75) +facet_wrap(Grade ~ .,shrink = FALSE)+theme_fivethirtyeight()+
  ggtitle("Facetted interrrival times >3 | GEONET 500 days to 15/4/22")
dev.off()























#Lets look at the magnitudes
data<-read.csv('IRISCombo_15422_100days.csv', header=FALSE)



V<-rep(1,length(data)) #Change number 
for (i in 1:length(data)){V[i]<-as.numeric(data[2,i])}

A<-rep(1,length(data))
for (i in 1:length(data)){A[i]<-as.numeric(data[4,i])}

#Just making in case
count<-rep(0,100)
DAY<-24*60^2
for(i in 1:length(count)){
  count[i] = length(A[A<=DAY*(i+1) & A>=DAY*i])
}



#order the time in ascending order and then order the magnitudes based off of that
Time<-A[order(A)]
Magnitude<-V[order(A)]
InterArrival_Times<-c(Time[1],Time[2:length(Time)] - Time[1:length(Time)-1])
Grade<-rep(0,length(Time))
for(i in 1:length(Time)){
  if(Magnitude[i]>=4 & Magnitude[i]<5){
    Grade[i] = '>4'
  }
  else if(Magnitude[i]>=5 & Magnitude[i]<6){
    Grade[i] = '>5'
  }
  else if(Magnitude[i]>=6 & Magnitude[i]<7){
    Grade[i] = '>6'
  }
  else if(Magnitude[i]>=7){
    Grade[i] = '>7'
  }
  else{
    Grade[i] = '>3'
  }
}

df<-data.frame(Magnitude,Time,InterArrival_Times,Grade)



png("IRIS_count_100days_InterarrivalTimes_Magnitude.png", 720,480)
ggplot(data=df, aes(x=InterArrival_Times, color = Grade))+geom_histogram(aes(y=..density.., fill = Grade),bins = 35)+
  geom_density(aes(y=..density..),color="darkseagreen4",size=1)+theme_fivethirtyeight()+
  ggtitle("Interarrival times | IRIS 100 Days to 15/4/22")
dev.off()


png("IRIS_count_100days_InterarrivalTimes_FACET_Magnitude.png", 720,480)
ggplot(data=df, aes(x=InterArrival_Times, color = Grade))+geom_histogram(aes(y=..density.., fill = Grade),bins = 35)+
  geom_density(aes(y=..density..),color="dodgerblue2",size=.75) +facet_wrap(Grade ~ .,shrink = FALSE)+theme_fivethirtyeight()+
  ggtitle("Facetted interrrival times >3 | IRIS 100 days to 15/4/22")
dev.off()









#Lets look at the magnitudes
data<-read.csv('IRISCombo_15422_500days.csv', header=FALSE)



V<-rep(1,length(data)) #Change number 
for (i in 1:length(data)){V[i]<-as.numeric(data[2,i])}

A<-rep(1,length(data))
for (i in 1:length(data)){A[i]<-as.numeric(data[4,i])}

#Just making in case
count<-rep(0,100)
DAY<-24*60^2
for(i in 1:length(count)){
  count[i] = length(A[A<=DAY*(i+1) & A>=DAY*i])
}



#order the time in ascending order and then order the magnitudes based off of that
Time<-A[order(A)]
Magnitude<-V[order(A)]
InterArrival_Times<-c(Time[1],Time[2:length(Time)] - Time[1:length(Time)-1])
Grade<-rep(0,length(Time))
for(i in 1:length(Time)){
  if(Magnitude[i]>=4 & Magnitude[i]<5){
    Grade[i] = '>4'
  }
  else if(Magnitude[i]>=5 & Magnitude[i]<6){
    Grade[i] = '>5'
  }
  else if(Magnitude[i]>=6 & Magnitude[i]<7){
    Grade[i] = '>6'
  }
  else if(Magnitude[i]>=7){
    Grade[i] = '>7'
  }
  else{
    Grade[i] = '>3'
  }
}

df<-data.frame(Magnitude,Time,InterArrival_Times,Grade)



png("IRIS_count_500days_InterarrivalTimes_Magnitude.png", 720,480)
ggplot(data=df, aes(x=InterArrival_Times, color = Grade))+geom_histogram(aes(y=..density.., fill = Grade),bins = 35)+
  geom_density(aes(y=..density..),color="darkseagreen4",size=1)+theme_fivethirtyeight()+
  ggtitle("Interarrival times | IRIS 500 Days to 15/4/22")
dev.off()


png("IRIS_count_500days_InterarrivalTimes_FACET_Magnitude.png", 720,480)
ggplot(data=df, aes(x=InterArrival_Times, color = Grade))+geom_histogram(aes(y=..density.., fill = Grade),bins = 35)+
  geom_density(aes(y=..density..),color="dodgerblue2",size=.75) +facet_wrap(Grade ~ .,shrink = FALSE)+theme_fivethirtyeight()+
  ggtitle("Facetted interrrival times >3 | IRIS 500 days to 15/4/22")
dev.off()









