# Earthquake Data Distributions 
 
Earthquake occurrences were generally indicative of Poisson distributions when considering a relatively low number of days (100 days) and got superseded by the negative binomial distribution due to the inflated presence of days with 0 events for larger periods (500 days).

This readme document will contain very brief description of some of the results that are fully available for perusal in this repository.

## Inter-arrival times

### Interarrival times against exponential and gamma distributions | 100 day sample

We examine, amongst a number of features, the time between earthquakes. This is done separately for earthquakes for earthquake ranges of 1 (eg 3-4, 4-5 etc). This is done because earthquake arrival times are a significant contender for a typical Poisson process for most studies. While this varies greatly for different scenarios, this was highly corroborated by the fact that the distribution of inter-arrival times strongly followed either an exponential or gamma distribution.  This was significant becaues a feature of homogenous Poisson Processes is that the distribution of inter-arrival times follows an exponential distribution.

Let us have a quick look at a Cullen and Frey plot for inter-arrival times of Earthquakes for our 100 day sample (we did 100,250 and 500 day samples for this study)

#### Cullen and Frey plot | 100 days
![GEONET_4to5_100days_Times_CullenFrey](https://user-images.githubusercontent.com/100022747/210140996-a8b15097-14a8-455e-9ef7-4dc26b19677a.png)
![GEONET_5to6_100days_Times_CullenFrey](https://user-images.githubusercontent.com/100022747/210141161-3176b4c2-ac5d-4f5f-bc98-a7ae8a80b251.png)
![GEONET_6to7_100days_Times_CullenFrey](https://user-images.githubusercontent.com/100022747/210141162-e24df1a8-469a-429b-bf34-4da5195998a0.png)


#### GEONET Inter-arrival time Distributions | 100 days

Wonder what their distributions look like based off of magnitude? Here are some plots for 4-5,5-6 and 6-7 for this 100 day period for the GEONET catalog of recorded earthquakes!

![GEONET_4to5_100daysTimes](https://user-images.githubusercontent.com/100022747/210141079-07d50068-0528-4768-9569-ebc6281e901e.png)
![GEONET_5to6_100daysTimes](https://user-images.githubusercontent.com/100022747/210141080-de1818e8-1102-4ae0-879d-ae44e6fa3567.png)
![GEONET_6to7_100daysTimes](https://user-images.githubusercontent.com/100022747/210141081-c75bf470-7cba-4e4e-bc86-f12e8839c1c8.png)

#### GEONET Inter-arrival time Distributions | 100 days

Below is a facetted plot showing how the corresponding IRIS dataset (instead of GEONET's above) distributions' shapes change with different magnitude ranges.

![Interarrival times for earthquakes in a 100day record](https://user-images.githubusercontent.com/100022747/210140775-76c955f9-fa0f-4cb4-b5d7-a5c305dd54fc.png)

#### IRIS Inter-arrival time Distributions | 100 days

![IRIS_count_100days_InterarrivalTimes_FACET_Magnitude](https://user-images.githubusercontent.com/100022747/210141234-dc9bbfbe-29c6-476a-a133-5fa6dc47bef0.png)



### Interarrival times against exponential and gamma distributions | 500 day sample

Let us look at the same plots but for a 500 day catalog for both GEONET and IRIS!


#### Cullen and Frey plot | 500 days

![GEONET_4to5_500days_Times_CullenFrey](https://user-images.githubusercontent.com/100022747/210141176-cfcbd591-99f6-4e0c-845c-96dac6dfe518.png)
![GEONET_5to6_500days_Times_CullenFrey](https://user-images.githubusercontent.com/100022747/210141178-569d517f-6a9e-42f9-8740-04c75608d533.png)
![GEONET_6to7_500days_Times_CullenFrey](https://user-images.githubusercontent.com/100022747/210141180-0bed4bae-8531-40aa-9548-2b1abf6e5083.png)

#### GEONET Inter-arrival time Distributions | 500 days
![GEONET_6to7_500daysTimes](https://user-images.githubusercontent.com/100022747/210141192-2a5ae89a-602e-4c98-a7de-376a236988dd.png)
![GEONET_4to5_500daysTimes](https://user-images.githubusercontent.com/100022747/210141193-18867b18-daa4-4750-80d1-f40b43757496.png)
![GEONET_5to6_500daysTimes](https://user-images.githubusercontent.com/100022747/210141194-de4084c2-e944-407c-98da-7dd675ff3f6d.png)

#### GEONET Inter-arrival time Distributions | 500 days


![GEONET_count_500days_InterarrivalTimes_FACET_Magnitude](https://user-images.githubusercontent.com/100022747/210141241-331d3ca3-7b1c-4145-8bb5-691fc9c08680.png)

#### IRIS Inter-arrival time Distributions | 500 days



![IRIS_count_500days_InterarrivalTimes_FACET_Magnitude](https://user-images.githubusercontent.com/100022747/210141248-5f7d908a-3d7d-4266-8845-9611a6f4e6bb.png)


A more nuanced or fine grained attempt at classifying based off of magnitude instead of our 1.0 magnitude ranges may be appropriate for a future experimentation.
