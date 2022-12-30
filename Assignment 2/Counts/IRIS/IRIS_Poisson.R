setwd('F:/Uni/Esci451/Assignment 2/Times/')
library(fitdistrplus)
library(logspline)
library(ggthemes)
library(ggplot2)
library(CDFt)
library(gofgamma)
require(MASS)
library(MASS)


#Define standardizing function
stand<-function(vector){
  return((vector-min(vector))/(range(vector)[2]-range(vector)[1]))
}


#Checking for fit with POisson
#1
dispersion_test <- function(x) 
{
  res <- 1-2 * abs((1 - pchisq((sum((x - mean(x))^2)/mean(x)), length(x) - 1))-0.5)
  
  cat("Dispersion test of count data:\n",
      length(x), " data points.\n",
      "Mean: ",mean(x),"\n",
      "Variance: ",var(x),"\n",
      "Probability of being drawn from Poisson distribution: ", 
      round(res, 3),"\n", sep = "")
  
  invisible(res)
}


residual_Pois<-function(data, model, n=5){
  RES=0
  for (i in 1:n){    
    sim<-rpois(length(data), model$estimate)
    res<-sum((data-sim)^2)
    RES = RES+res}
  return(RES/n)
}


residual_nbinom<-function(data, model,n=5){
  RES = 0
  for(i in 1:n){  
    sim<-rnbinom(length(data), size = model$estimate[1], mu = model$estimate[2])
    res<-sum((data-sim)^2)
    RES = RES+res}
  
  return(RES/n)
}




#4-5 500 days
arr<-read.csv('IRIS_4to5_500days.csv', header=FALSE)
A<-rep(1,length(arr))
for (i in 1:length(arr)){A[i]<-as.numeric(arr[2,i])}

A0<-A[order(A)] #Needed to add this to arrange the events in ascending order in order to actually 
inter<-rep(1,length(arr)-1)
for (i in 1:length(A0)-1){inter[i]<-A0[i+1]-A0[i]}



#make an empty dataset of 0s, each 0 pertaining to the day of 100 days

count4<-rep(0,500)
DAY<-24*60^2
for(i in 1:length(count3)){
  count4[i] = length(A0[A0<=DAY*(i+1) & A0>=DAY*i])
}
var=count4

#Fit diagnostic plots and histogram


png("../Counts/IRIS/IRIS_count_500days_5to6_hist.png", 1920,1080)
ggplot(as.data.frame(var), aes(var)) + geom_histogram(aes(y=..density..),,color="blue", fill="black",bins = 10)+
  scale_color_brewer(palette="Dark2")+theme_fivethirtyeight()+
  ggtitle("Count data for the past 500 days | Magn 4-5 IRIS")
dev.off()


png("../Counts/IRIS/IRIS_5to6_500days_Occ_CullenFrey.png", 800,450)
descdist(data=var, discrete = TRUE, boot=50, boot.col="red")
dev.off()


#looks like all of the possible distr s for discrete seem possible on the plot
#Distrs like geometric would likely not fit this situation despite it not being included on this CullFrey plot
#Poisson
EQ_Poi<-fitdist(var, "pois", method = 'mme')
png("../Counts/IRIS/IRIS_5to6_500days_Occ_fittopoisson.png", 800,450)
plot(EQ_Poi)
dev.off()

#Negative binomial
EQ_negativeb<-fitdist(var, "nbinom", method = 'mme')
png("../Counts/IRIS/IRIS_5to6_500days_Occ_fittonegativebinomial.png", 800,450)
plot(EQ_negativeb)
dev.off()


summary(EQ_Poi)[7:8]
# $aic
# [1] 2789.69
# 
# $bic
# [1] 2793.904


summary(EQ_negativeb)[7:8]
# $aic
# [1] 981.8237
# 
# $bic
# [1] 990.2529


EQ_negativeb$estimate









#5-6 500 days
arr<-read.csv('IRIS_5to6_500days.csv', header=FALSE)
A<-rep(1,length(arr))
for (i in 1:length(arr)){A[i]<-as.numeric(arr[2,i])}

A0<-A[order(A)] #Needed to add this to arrange the events in ascending order in order to actually 
inter<-rep(1,length(arr)-1)
for (i in 1:length(A0)-1){inter[i]<-A0[i+1]-A0[i]}



#make an empty dataset of 0s, each 0 pertaining to the day of 100 days

count4<-rep(0,500)
DAY<-24*60^2
for(i in 1:length(count4)){
  count4[i] = length(A0[A0<=DAY*(i+1) & A0>=DAY*i])
}
var=count4

#Fit diagnostic plots and histogram


png("../Counts/IRIS/IRIS_count_500days_5to6_hist.png", 1920,1080)
ggplot(as.data.frame(var), aes(var)) + geom_histogram(aes(y=..density..),,color="blue", fill="black",bins = 10)+
  scale_color_brewer(palette="Dark2")+theme_fivethirtyeight()+
  ggtitle("Count data for the past 500 days | Magn 4-5 IRIS")
dev.off()


png("../Counts/IRIS/IRIS_5to6_500days_Occ_CullenFrey.png", 800,450)
descdist(data=var, discrete = TRUE, boot=50, boot.col="red")
dev.off()


#looks like all of the possible distr s for discrete seem possible on the plot
#Distrs like geometric would likely not fit this situation despite it not being included on this CullFrey plot
#Poisson
EQ_Poi<-fitdist(var, "pois", method = 'mme')
png("../Counts/IRIS/IRIS_5to6_500days_Occ_fittopoisson.png", 800,450)
plot(EQ_Poi)
dev.off()

#Negative binomial
EQ_negativeb<-fitdist(var, "nbinom", method = 'mme')
png("../Counts/IRIS/IRIS_5to6_500days_Occ_fittonegativebinomial.png", 800,450)
plot(EQ_negativeb)
dev.off()


summary(EQ_Poi)[7:8]
# $aic
# [1] 2789.69
# 
# $bic
# [1] 2793.904


summary(EQ_negativeb)[7:8]
# $aic
# [1] 981.8237
# 
# $bic
# [1] 990.2529


EQ_negativeb$estimate

EQ_Poi$estimate



#6-7 500 days



arr<-read.csv('IRIS_6to7_500days.csv', header=FALSE)
A<-rep(1,length(arr))
for (i in 1:length(arr)){A[i]<-as.numeric(arr[2,i])}

A0<-A[order(A)] #Needed to add this to arrange the events in ascending order in order to actually 
inter<-rep(1,length(arr)-1)
for (i in 1:length(A0)-1){inter[i]<-A0[i+1]-A0[i]}



#make an empty dataset of 0s, each 0 pertaining to the day of 100 days

count6<-rep(0,500)
DAY<-24*60^2
for(i in 1:length(count3)){
  count6[i] = length(A0[A0<=DAY*(i+1) & A0>=DAY*i])
}
var=count6

#Fit diagnostic plots and histogram


png("../Counts/IRIS/IRIS_count_500days_6to7_hist.png", 1280,720)
ggplot(as.data.frame(var), aes(var)) + geom_histogram(aes(y=..density..),,color="blue", fill="black",bins = 10)+
  scale_color_brewer(palette="Dark2")+theme_fivethirtyeight()+
  ggtitle("Count data for the past 500 days | Magn 5-6 IRIS")
dev.off()


png("../Counts/IRIS/IRIS_6to7_500days_Occ_CullenFrey.png", 1280,720)
descdist(data=var, discrete = TRUE, boot=50, boot.col="red")
dev.off()


#looks like all of the possible distr s for discrete seem possible on the plot
#Distrs like geometric would likely not fit this situation despite it not being included on this CullFrey plot
#Poisson
EQ_Poi<-fitdist(var, "pois", method = 'mme')
png("../Counts/IRIS/IRIS_6to7_500days_Occ_fittopoisson.png", 800,450)
plot(EQ_Poi)
dev.off()

#Negative binomial
EQ_negativeb<-fitdist(var, "nbinom", method = 'mme')
png("../Counts/IRIS/IRIS_6to7_500days_Occ_fittonegativebinomial.png", 800,450)
plot(EQ_negativeb)
dev.off()







dispersion_test(var)

# Dispersion test of count data:
#   500 data points.
# Mean: 0.084
# Variance: 0.2133707
# Probability of being drawn from Poisson distribution: 0



#Now let us compare the residuals

residual_Pois(var,EQ_Poi,500)
#[1] 148.632

residual_nbinom(var,EQ_negativeb,500)
#[1] 210.686


#nbinom looks like larger residual here!

#Now let us compare the AIC and BIC criterion


summary(EQ_Poi)[7:8]
# $aic
# [1] 330.4485
# 
# $bic
# [1] 334.6631


summary(EQ_negativeb)[7:8]
# $aic
# [1] 249.6396
# 
# $bic
# [1] 258.0688

#negative binomial is clearly a better pick here



#Parameters

EQ_Poi$estimate
# lambda 
# 0.084   

#Implying 0.084 earthquakes of 6-7 per day in this 500 day period

#What would negative binomial have as parameters>?

EQ_negativeb$estimate
# size         mu 
# 0.05472143 0.08400000 



