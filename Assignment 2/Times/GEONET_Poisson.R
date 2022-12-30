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


##100 days ago | 4-5

arr<-read.csv('GEONET_4to5_100days.csv', header=FALSE)
A<-rep(1,length(arr))
for (i in 1:length(arr)){A[i]<-as.numeric(arr[2,i])}

A0<-A[order(A)] #Needed to add this to arrange the events in ascending order in order to actually 
inter<-rep(1,length(arr)-1)
for (i in 1:length(A0)-1){inter[i]<-A0[i+1]-A0[i]}



#make an empty dataset of 0s, each 0 pertaining to the day of 100 days

count<-rep(0,100)
DAY<-24*60^2
for(i in 1:length(count)){
  count[i] = length(A0[A0<=DAY*(i+1) & A0>=DAY*i])
}


png("../Counts/GEONET_count_100days_4to5_hist.png", 1280,720)
ggplot(as.data.frame(count), aes(count)) + geom_histogram(aes(y=..density..),,color="blue", fill="black",bins = 35)+
  scale_color_brewer(palette="Dark2")+theme_fivethirtyeight()+
  ggtitle("Count data for the past 100 days | Magn 4-5 GEONET")
dev.off()


png("../Counts/GEONET_4to5_100days_Occ_CullenFrey.png", 1280,720)
descdist(data=count, discrete = TRUE, boot=50, boot.col="red")
dev.off()


#looks like all of the possible distr s for discrete seem possible on the plot
#Distrs like geometric would likely not fit this situation despite it not being included on this CullFrey plot
#Poisson
EQ_Poi<-fitdist(count, "pois", method = 'mme')
png("../Counts/GEONET_4to5_100days_Occ_fittopoisson.png", 800,450)
plot(EQ_Poi)
dev.off()

#Negative binomial
EQ_negativeb<-fitdist(count, "nbinom", method = 'mme')
png("../Counts/GEONET_4to5_100days_Occ_fittonegativebinomial.png", 800,450)
plot(EQ_negativeb)
dev.off()




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

dispersion_test(count)
#Dispersion test of count data:
# 100 data points.
# Mean: 3.9
# Variance: 5.222222
# Probability of being drawn from Poisson distribution: 0.027

#2
source('https://dl.dropboxusercontent.com/s/yj7yc07s5fgkirz/CheckPoisson.R?dl=0')
CheckPoisson(count,0,max(count), mean(count))

# [1] "mean: 3.9"
# [1] "variance: 5.22222222222222"
# Int Freq       PoisF      ResidF Prop       PoisD      ResidD
# 1    0    4  2.02419114  1.97580886    4  2.02419114  1.97580886
# 2    1    6  7.89434546 -1.89434546    6  7.89434546 -1.89434546
# 3    2   14 15.39397365 -1.39397365   14 15.39397365 -1.39397365
# 4    3   26 20.01216575  5.98783425   26 20.01216575  5.98783425
# 5    4   15 19.51186161 -4.51186161   15 19.51186161 -4.51186161
# 6    5   21 15.21925205  5.78074795   21 15.21925205  5.78074795
# 7    6    4  9.89251383 -5.89251383    4  9.89251383 -5.89251383
# 8    7    2  5.51154342 -3.51154342    2  5.51154342 -3.51154342
# 9    8    4  2.68687742  1.31312258    4  2.68687742  1.31312258
# 10   9    2  1.16431355  0.83568645    2  1.16431355  0.83568645
# 11  10    0  0.45408228 -0.45408228    0  0.45408228 -0.45408228
# 12  11    1  0.16099281  0.83900719    1  0.16099281  0.83900719
# 13  12    0  0.05232266 -0.05232266    0  0.05232266 -0.05232266
# 14  13    0  0.01569680 -0.01569680    0  0.01569680 -0.01569680
# 15  14    1  0.00437268  0.99562732    1  0.00437268  0.99562732


summary(EQ_Poi)[7:8]
# $aic
# [1] 437.6
# 
# $bic
# [1] 440.2052



residual_Pois<-function(data, model, n=5){
  RES=0
  for (i in 1:n){    
    sim<-rpois(length(data), model$estimate)
    res<-sum((data-sim)^2)
    RES = RES+res}
  return(RES/n)
}

residual_Pois(count,EQ_Poi,500)
#[1] 907.164






#Note that the variance exceeds the mean, which might imply that we should be trying negative binomial instead
#The negative binomial distribution is deemed a more "acceptable" function to count data than Poisson by the fact that
#negative binomial distribution allowes for a dispersion parameter that changes, which is held constant for Poisson



#Checking for nbinomial
COUNT<-as.data.frame(count)
summary(EQ_negativeb)[7:8]
# $aic
# [1] 436.0836
# 
# $bic
# [1] 441.2939



residual_nbinom<-function(data, model,n=5){
  RES = 0
  for(i in 1:n){  
    sim<-rnbinom(length(data), size = model$estimate[1], mu = model$estimate[2])
    res<-sum((data-sim)^2)
    RES = RES+res}

  return(RES/n)
}

residual_nbinom(count,EQ_negativeb,500)
#[1] 1028.266


#Residuals seem to slightly favour Poisson. However, AIC and BIC criterion show no significant preference for either model
#Let's go with Poisson

EQ_Poi$estimate
#lambda 
#3.9 

#Implying 3.9 earthquakes of 4-5 per day in this 100 day period

#What would negative binomial have as parameters>?

EQ_negativeb$estimate
# size       mu 
# 11.97638  3.90000 














#100 days 5-6 magnitude


arr<-read.csv('GEONET_5to6_100days.csv', header=FALSE)
A<-rep(1,length(arr))
for (i in 1:length(arr)){A[i]<-as.numeric(arr[2,i])}

A0<-A[order(A)] #Needed to add this to arrange the events in ascending order in order to actually 
inter<-rep(1,length(arr)-1)
for (i in 1:length(A0)-1){inter[i]<-A0[i+1]-A0[i]}



#make an empty dataset of 0s, each 0 pertaining to the day of 100 days

count2<-rep(0,100)
DAY<-24*60^2
for(i in 1:length(count2)){
  count2[i] = length(A0[A0<=DAY*(i+1) & A0>=DAY*i])
}


#Fit diagnostic plots and histogram


png("../Counts/GEONET_count_100days_5to6_hist.png", 1280,720)
ggplot(as.data.frame(count2), aes(count2)) + geom_histogram(aes(y=..density..),,color="blue", fill="black",bins = 10)+
  scale_color_brewer(palette="Dark2")+theme_fivethirtyeight()+
  ggtitle("Count data for the past 100 days | Magn 5-6 GEONET")
dev.off()


png("../Counts/GEONET_5to6_100days_Occ_CullenFrey.png", 1280,720)
descdist(data=count2, discrete = TRUE, boot=50, boot.col="red")
dev.off()


#looks like all of the possible distr s for discrete seem possible on the plot
#Distrs like geometric would likely not fit this situation despite it not being included on this CullFrey plot
#Poisson
EQ_Poi<-fitdist(count2, "pois", method = 'mme')
png("../Counts/GEONET_5to6_100days_Occ_fittopoisson.png", 800,450)
plot(EQ_Poi)
dev.off()

#Negative binomial
EQ_negativeb<-fitdist(count2, "nbinom", method = 'mme')
png("../Counts/GEONET_5to6_100days_Occ_fittonegativebinomial.png", 800,450)
plot(EQ_negativeb)
dev.off()


#Both plots look pretty convincing fits
#Let us try to examine the dispersion

dispersion_test(count2)

# Dispersion test of count data:
#   100 data points.
# Mean: 1.59
# Variance: 1.900909
# Probability of being drawn from Poisson distribution: 0.18

#Given that this is experimental data, this is a pretty encouraging result
#However, this could be chalked up to sample size of 100



#Now let us compare the residuals

residual_Pois(count2,EQ_Poi,500)
#[1] 343.552

residual_nbinom(count2,EQ_negativeb,500)
#[1] 377.236


#Residuals look smaller for poisson by a decently significant amount

#Now let us compare the AIC and BIC criterion


summary(EQ_Poi)[7:8]
# $aic
# [1] 329.0615
# 
# $bic
# [1] 331.6667


summary(EQ_negativeb)[7:8]
# $aic
# [1] 329.5035
# 
# $bic
# [1] 334.7138


#Poisson appears to be the better pick



#Parameters

EQ_Poi$estimate
# lambda 
# 1.59 

#Implying 1.5 earthquakes of 5-6 per day in this 100 day period

#What would negative binomial have as parameters>?

EQ_negativeb$estimate
# size       mu 
# 8.660843 1.590000 


















#6-7 100 days



arr<-read.csv('GEONET_6to7_100days.csv', header=FALSE)
A<-rep(1,length(arr))
for (i in 1:length(arr)){A[i]<-as.numeric(arr[2,i])}

A0<-A[order(A)] #Needed to add this to arrange the events in ascending order in order to actually 
inter<-rep(1,length(arr)-1)
for (i in 1:length(A0)-1){inter[i]<-A0[i+1]-A0[i]}



#make an empty dataset of 0s, each 0 pertaining to the day of 100 days

count3<-rep(0,100)
DAY<-24*60^2
for(i in 1:length(count3)){
  count3[i] = length(A0[A0<=DAY*(i+1) & A0>=DAY*i])
}


#Fit diagnostic plots and histogram


png("../Counts/GEONET_count_100days_6to7_hist.png", 1280,720)
ggplot(as.data.frame(count3), aes(count3)) + geom_histogram(aes(y=..density..),,color="blue", fill="black",bins = 10)+
  scale_color_brewer(palette="Dark2")+theme_fivethirtyeight()+
  ggtitle("Count data for the past 100 days | Magn 6-7 GEONET")
dev.off()


png("../Counts/GEONET_6to7_100days_Occ_CullenFrey.png", 1280,720)
descdist(data=count3, discrete = TRUE, boot=50, boot.col="red")
dev.off()


#looks like all of the possible distr s for discrete seem possible on the plot
#Distrs like geometric would likely not fit this situation despite it not being included on this CullFrey plot
#Poisson
EQ_Poi<-fitdist(count3, "pois", method = 'mme')
png("../Counts/GEONET_6to7_100days_Occ_fittopoisson.png", 800,450)
plot(EQ_Poi)
dev.off()

#Negative binomial
EQ_negativeb<-fitdist(count3, "nbinom", method = 'mme')
png("../Counts/GEONET_6to7_100days_Occ_fittonegativebinomial.png", 800,450)
plot(EQ_negativeb)
dev.off()







dispersion_test(count3)

# Dispersion test of count data:
#   100 data points.
# Mean: 0.28
# Variance: 0.3652525
# Probability of being drawn from Poisson distribution: 0.045

#Given that this is experimental data, this is a pretty encouraging result
#However, this could be chalked up to sample size of 100



#Now let us compare the residuals

residual_Pois(count3,EQ_Poi,500)
#[1] 63.872

residual_nbinom(count3,EQ_negativeb,500)
#[1] 72.266


#Residuals look smaller for poisson by a decently significant amount

#Now let us compare the AIC and BIC criterion


summary(EQ_Poi)[7:8]
# $aic
# [1] 138.4148
# 
# $bic
# [1] 141.0199


summary(EQ_negativeb)[7:8]
# $aic
# [1] 138.5167
# 
# $bic
# [1] 143.7271


#Poisson appears to be the better pick



#Parameters

EQ_Poi$estimate
# lambda 
# 0.28 

#Implying .28 earthquakes of 6-7 per day in this 100 day period

#What would negative binomial have as parameters>?

EQ_negativeb$estimate
# size        mu 
# 0.9607843 0.2800000 












##What if we take data for 500 days?



#4-5 500 days



arr<-read.csv('GEONET_4to5_500days.csv', header=FALSE)
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


png("../Counts/GEONET_count_500days_4to5_hist.png", 1280,720)
ggplot(as.data.frame(var), aes(var)) + geom_histogram(aes(y=..density..),,color="blue", fill="black",bins = 10)+
  scale_color_brewer(palette="Dark2")+theme_fivethirtyeight()+
  ggtitle("Count data for the past 500 days | Magn 4-5 GEONET")
dev.off()


png("../Counts/GEONET_4to5_500days_Occ_CullenFrey.png", 800,450)
descdist(data=var, discrete = TRUE, boot=50, boot.col="red")
dev.off()


#looks like all of the possible distr s for discrete seem possible on the plot
#Distrs like geometric would likely not fit this situation despite it not being included on this CullFrey plot
#Poisson
EQ_Poi<-fitdist(var, "pois", method = 'mme')
png("../Counts/GEONET_4to5_500days_Occ_fittopoisson.png", 800,450)
plot(EQ_Poi)
dev.off()

#Negative binomial
EQ_negativeb<-fitdist(var, "nbinom", method = 'mme')
png("../Counts/GEONET_4to5_500days_Occ_fittonegativebinomial.png", 800,450)
plot(EQ_negativeb)
dev.off()







dispersion_test(var)

# Dispersion test of count data:
#   500 data points.
# Mean: 1.01
# Variance: 27.6011
# Probability of being drawn from Poisson distribution: 0


#Very likely due to inflation of 0 event days



#Now let us compare the residuals

residual_Pois(var,EQ_Poi,500)
#[1] 14280.02

residual_nbinom(var,EQ_negativeb,500)
#[1] 27714.02


#nbinom looks like larger residual here!

#Now let us compare the AIC and BIC criterion


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

#negative binomial is clearly a better pick here



#Parameters

EQ_Poi$estimate
# lambda 
# 1.01  

#Implying 1 earthquakes of 4-5 per day in this 500 day period

#What would negative binomial have as parameters>?

EQ_negativeb$estimate
# size         mu 
# 0.03844226 1.01000000 








#5-6 500 days



arr<-read.csv('GEONET_5to6_500days.csv', header=FALSE)
A<-rep(1,length(arr))
for (i in 1:length(arr)){A[i]<-as.numeric(arr[2,i])}

A0<-A[order(A)] #Needed to add this to arrange the events in ascending order in order to actually 
inter<-rep(1,length(arr)-1)
for (i in 1:length(A0)-1){inter[i]<-A0[i+1]-A0[i]}



#make an empty dataset of 0s, each 0 pertaining to the day of 100 days

count5<-rep(0,500)
DAY<-24*60^2
for(i in 1:length(count3)){
  count5[i] = length(A0[A0<=DAY*(i+1) & A0>=DAY*i])
}
var=count5

#Fit diagnostic plots and histogram


png("../Counts/GEONET_count_500days_5to6_hist.png", 1280,720)
ggplot(as.data.frame(var), aes(var)) + geom_histogram(aes(y=..density..),,color="blue", fill="black",bins = 10)+
  scale_color_brewer(palette="Dark2")+theme_fivethirtyeight()+
  ggtitle("Count data for the past 500 days | Magn 5-6 GEONET")
dev.off()


png("../Counts/GEONET_5to6_500days_Occ_CullenFrey.png", 1280,720)
descdist(data=var, discrete = TRUE, boot=50, boot.col="red")
dev.off()


#looks like all of the possible distr s for discrete seem possible on the plot
#Distrs like geometric would likely not fit this situation despite it not being included on this CullFrey plot
#Poisson
EQ_Poi<-fitdist(var, "pois", method = 'mme')
png("../Counts/GEONET_5to6_500days_Occ_fittopoisson.png", 800,450)
plot(EQ_Poi)
dev.off()

#Negative binomial
EQ_negativeb<-fitdist(var, "nbinom", method = 'mme')
png("../Counts/GEONET_5to6_500days_Occ_fittonegativebinomial.png", 800,450)
plot(EQ_negativeb)
dev.off()







dispersion_test(var)

# Dispersion test of count data:
#   500 data points.
# Mean: 0.386
# Variance: 2.465936
# Probability of being drawn from Poisson distribution: 0



#Now let us compare the residuals

residual_Pois(var,EQ_Poi,500)
#[1] 1419.812

residual_nbinom(var,EQ_negativeb,500)
#[1] 2477.572


#nbinom looks like larger residual here!

#Now let us compare the AIC and BIC criterion


summary(EQ_Poi)[7:8]
# $aic
# [1] 1096.484
# 
# $bic
# [1] 1100.698


summary(EQ_negativeb)[7:8]
# $aic
# [1] 668.3687
# 
# $bic
# [1] 676.7979

#negative binomial is clearly a better pick here



#Parameters

EQ_Poi$estimate
# lambda 
# 0.386  

#Implying .36 earthquakes of 5-6 per day in this 500 day period

#What would negative binomial have as parameters>?

EQ_negativeb$estimate
# size         mu 
# 0.07180516 0.38600000 












#6-7 500 days



arr<-read.csv('GEONET_6to7_500days.csv', header=FALSE)
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


png("../Counts/GEONET_count_500days_6to7_hist.png", 1280,720)
ggplot(as.data.frame(var), aes(var)) + geom_histogram(aes(y=..density..),,color="blue", fill="black",bins = 10)+
  scale_color_brewer(palette="Dark2")+theme_fivethirtyeight()+
  ggtitle("Count data for the past 500 days | Magn 5-6 GEONET")
dev.off()


png("../Counts/GEONET_6to7_500days_Occ_CullenFrey.png", 1280,720)
descdist(data=var, discrete = TRUE, boot=50, boot.col="red")
dev.off()


#looks like all of the possible distr s for discrete seem possible on the plot
#Distrs like geometric would likely not fit this situation despite it not being included on this CullFrey plot
#Poisson
EQ_Poi<-fitdist(var, "pois", method = 'mme')
png("../Counts/GEONET_6to7_500days_Occ_fittopoisson.png", 800,450)
plot(EQ_Poi)
dev.off()

#Negative binomial
EQ_negativeb<-fitdist(var, "nbinom", method = 'mme')
png("../Counts/GEONET_6to7_500days_Occ_fittonegativebinomial.png", 800,450)
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









