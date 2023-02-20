library(ggplot2)
library(scales)

#What we're interested in: 
#Diel behavior - when fish are detected, does it differ where trees were added
  #Kruskal-Wallis test 
#Movement among sites - how many sites were fish detected at, does it differ where trees were added
#Frequency at sites - how often were fish detected and for how long, did it differ where trees were added


####Histograms for diel####

alltime <- san_pit_figs$time
allmonth <- san_pit_figs$month
alldate <- san_pit_figs$date 

#Both of these need to be converted to numeric 
hist(alltime)
hist(alldate)

hist(allmonth)

san_pit_figs$time <- strptime(san_pit_figs$time, format="%H:%M:%S")
san_pit_figs$hours <-  as.numeric(format(san_pit_figs$time, format="%H"))
hist(san_pit_figs$hours)

#All fish, detections by hour per year 
ggplot(data=san_pit_figs) + geom_histogram(aes(x=hours), bins=24) +  
  facet_wrap(~ year) +
  ggtitle("All fish detections by hour per year")
#Using fill
ggplot(data=san_pit_figs) + geom_bar(aes(x=hours, fill=species)) +  
  facet_wrap(~ year) +
  ggtitle("All fish detections by hour per year")
#All years, detections by hour per species 
ggplot(data=subset(san_pit_figs, !is.na(species))) + geom_histogram(aes(x=hours), bins=24) +  
  facet_wrap(~ species) +
  ggtitle("All fish detections by hour per species")
#Detections per hour by species+year
ggplot(data=subset(san_pit_figs, !is.na(species))) + geom_histogram(aes(x=hours), bins=24) +  
  facet_wrap(~species ~year) +
  ylim(0,500) +
  ggtitle("Detections per hour by species+year")

#Now lets do it per site
#CWH 
ggplot(data=subset(san_pit_figs,san_pit_figs$area=="1",!is.na(species))) + geom_histogram(aes(x=hours), bins=24) +  
  facet_wrap(~species ~year) +
  ylim(0,500) +
  ggtitle("Detections per hour by species+year in CWH")


#Non-CWH
ggplot(data=subset(san_pit_figs,san_pit_figs$area=="2",!is.na(species))) + geom_histogram(aes(x=hours), bins=24) +  
  facet_wrap(~species ~year) +
  ylim(0,500) +
  ggtitle("Detections per hour by species+year no-CWH")

#Using fill
ggplot(data=san_pit_figs) + geom_bar(aes(x=hours, fill=as.character(area))) +  
  facet_wrap(~ year) +
  ggtitle("All fish detections by hour per year")

ggplot(data=subset(san_pit_figs, !is.na(species))) + geom_bar(aes(x=hours, fill=tag)) +  
  facet_wrap(~species ~year) +
  ylim(0,500) +
  ggtitle("Detections per hour by species+year")

####Histograms for site####

#Number of detections at each site per year 
ggplot(data=subset(san_pit_figs, !is.na(site))) + geom_histogram(aes(x=site), bins=10) +
  facet_wrap(~year) +
  scale_x_continuous(breaks =seq(0,10,1)) +
  ggtitle("Detections at each site per year")
#Number of detections at each site per species
ggplot(data=subset(san_pit_figs, !is.na(species), !is.na(site))) + geom_histogram(aes(x=site), bins=10) +
  facet_wrap(~species) +
  scale_x_continuous(breaks =seq(0,10,1)) +
  ggtitle("Detections at each site per species")
#Number of detectons at each site per species+year
ggplot(data=subset(san_pit_figs, !is.na(species), !is.na(site))) + geom_histogram(aes(x=site), bins=10) +
  facet_wrap(~species ~year) +
  scale_x_continuous(breaks =seq(0,10,1)) +
  ggtitle("Detections at each site per species+year")
#Number of detections per site per month
ggplot(data=subset(san_pit_figs, !is.na(month), !is.na(site))) + geom_histogram(aes(x=site), bins=10) +facet_wrap(~month) 
#Number of detections per site per month +year
ggplot(data=subset(san_pit_figs, !is.na(month), !is.na(site))) + geom_histogram(aes(x=site), bins=10) +facet_wrap(~month ~year) 

#Number of detections per year at CWH (1) and no-CWH (2)
ggplot(data=subset(san_pit_figs, !is.na(area))) + geom_bar(aes(x=area)) +facet_wrap(~year) 
#Number of detections per month+year at CWH (1) and no-CWH (2)
ggplot(data=subset(san_pit_figs, !is.na(area))) + geom_bar(aes(x=area)) +facet_wrap(~year~month) 


#Goofin around with fills 
#Detections at each site per year with species
  ggplot(data=subset(san_pit_figs, !is.na(site))) + geom_bar(aes(x=site, fill= species)) +
  facet_wrap(~year) +
  scale_x_continuous(breaks =seq(0,10,1)) +
  ggtitle("Detections at each site per year")
  
#See what individuals are contributing the most 
ggplot(data=subset(san_pit_figs, !is.na(species), !is.na(site))) + geom_bar(aes(x=site, fill=tag)) +
    facet_wrap(~species) +
    scale_x_continuous(breaks =seq(0,10,1)) +
    ggtitle("Detections at each site per species")

ggplot(data=subset(san_pit_figs, !is.na(species), !is.na(site))) + geom_bar(aes(x=site, fill=tag)) +
  facet_wrap(~species ~year) +
  scale_x_continuous(breaks =seq(0,10,1)) +
  ggtitle("Detections at each site per species+year")


####Histograms for hits####
#All hits, very low above 1 hit
#10 hits = about 1.8 seconds 
#12000 out of 180000 instances are 1 hit
#80 instances of >1000 hits in 18000 samples
#Max is 36000 hits -> 108 minutes
ggplot(data=subset(san_pit_figs, !is.na(species), !is.na(hits))) + geom_bar(aes(x=hits, fill=species)) +
  ggtitle("Hits by each unique detection") +
  xlim(2,25)

#Hits per species+year
ggplot(data=subset(san_pit_figs, !is.na(species), !is.na(hits))) + geom_histogram(aes(x=hits), bins=10) +facet_wrap(~species ~year) 

#Year, species fill, limited 
ggplot(data=subset(san_pit_figs, !is.na(species), !is.na(hits))) + geom_bar(aes(x=hits, fill=species)) +
  facet_wrap(~year) +
  xlim(2,25)


       