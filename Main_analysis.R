# Main analysis of KBS Ladybeetle 2020 data, Coccinella 
# septempunctata and Harmonia axyridis niche partitioning

#bring data into R

LB<-read.csv(file="data/KBS_Haxy_C7_1989-2020.csv", header=T,
             na.strings=c(NA))


##### enough of all that, what about real data?
summary(LB)

#clean data
#first, we fix dates, make them ISO'ed
library(lubridate)

LB$newdate<-mdy(LB$DATE)#parses the date format used for the forest plots
LB$newdate<-mdy_hm(LB$DATE)#parses the date format used in the main plots

#well, crap, neither command gets all of the dates to parse correctly
#we have an issue because data from the main site exported with time stamps but 
#the forest site did not. Ugh.


#Christie's solution: brute force removal of timestamps
LB$DATE<-gsub(" 0:00", "", LB$DATE)#remove time stamp strings

LB$newdate<-mdy(LB$DATE)#parses the date format now used by all observations

LB$DOY<-yday(LB$newdate)
LB$week<-isoweek(LB$newdate)

summary(LB)#bingo! looks like it worked!

#let's take a look at ladybeetles by treatment
#we need to aggregate the data by rep first, because subsamples are zero-biased
library(dplyr)

lb_rep<-aggregate(data=LB, SumOfADULTS~ Year+week+TREAT+HABITAT+REPLICATE+SPID, FUN=sum)
lb_rep_N<-aggregate(data=LB, SumOfADULTS~ Year+week+TREAT+HABITAT+REPLICATE+SPID, FUN=length)
#change variable name to reflect that it's number of traps
lb_rep_N<-rename(lb_rep_N, TRAPS=SumOfADULTS)
#merge trap data into lb_rep data frame

lb_weekly<-merge(lb_rep, lb_rep_N)
#cull data prior to Harmonia's arrival in 1994
lb_weekly1994<-lb_weekly[which(lb_weekly$Year>=1994),]

#let's figure out how to re-catagorize our treatments. Remember T1-4 are annual, 5-7 are perennial, and the rest are forest
lb_weekly1994$TREAT_CAT<-if(lb_weekly1994$TREAT==0)

#remember to cull the data at a standard time point  
    
  

library(ggplot2)

lb_boxplot<-ggplot(lb_rep, aes(x=TREAT, y=SumOfADULTS, fill=SPID))+
  geom_boxplot()
lb_boxplot

#let's try re-aggregating our data at a yearly resolution
lb_yearly_captures<-aggregate(data=lb_weekly1994, SumOfADULTS~ Year+TREAT+HABITAT+REPLICATE+SPID, FUN=sum)
lb_yearly_N<-aggregate(data=lb_weekly1994, TRAPS~ Year+TREAT+HABITAT+REPLICATE+SPID, FUN=sum)

#merge yearly captures with sampling intensity data
lb_yearly<-merge(lb_yearly_captures, lb_yearly_N)
#compute a new variable- average number of beetles per trap
lb_yearly$pertrap<-lb_yearly$SumOfADULTS/lb_yearly$TRAPS

#let's repeat the boxplot but with yearly data
lb_yearly_boxplot<-ggplot(lb_yearly, aes(x=TREAT, y=pertrap, fill=SPID))+
  geom_boxplot()
lb_yearly_boxplot

#let's look at the populations over time instead
lb_yearly_plot<-ggplot(lb_yearly, aes(x=Year, y=SumOfADULTS, fill=SPID, color=SPID))+
  geom_point(pch=21)+
  geom_smooth()
lb_yearly_plot

###################################
#Begin weather data processing



#download weather data from KBS weather station
#download a local copy into the data folder and then pull it from there to load

# # how file was downloaded- commented out so it's not run and re-downloaded each time
# # Specify URL where file is stored
# url <- "http://lter.kbs.msu.edu/datatables/7.csv"
# # Specify destination where file should be saved
# destfile <- "data/kbsweather.csv"
# # Apply download.file function in R
# download.file(url, destfile)


weather<-read.csv(file="data/kbsweather.csv",
                    header=T, sep=",", na.strings="", comment.char = '#')
#extract day of year, so we have a continuous variable running for each year.
#since we're in a temperate northern climate, this is convenient- not too 
#much insect action happening at the december-january transition, so we 
#can use the yearly break as a blocking variable for rowing season.
#it's convenient living where we do! 

weather$DOY<-yday(weather$date)
weather$week<-isoweek(weather$date)
#do a few simple plots to make sure the data makes sense -this is
#a good way to check that the importation was successful

plot(weather$DOY, weather$air_temp_mean)
plot(weather$DOY, weather$precipitation)

#let's cut out the data from before 1989 so we can process the weather data more quickly. Al so we'll cut off the weather
#data that's causing us problems- we don't need it anyway
weather<-subset(weather, weather$year>=1989& weather$year<=2020)


#lets also get rid of the variables we don't need:
weather$flag_precip<-NULL
weather$flag_air_temp_mean<-NULL
weather$flag_air_temp_max<-NULL
weather$flag_air_temp_min<-NULL

#also, these data are sorted in descending order. It's easier to think of this 
#stuff in ascending order, so let's sort the data by year and DOY

weather<-weather[order(weather$year, weather$DOY),]

#Let's examine the data to see how complete it is
summary(weather)

#let's pre-process these weather data so we get rid of missing values
# we can write a function to do this for us.
#if missing data is rare, it is probably safe to assume that missing
#temperatures are similar to the weather on the day before or after.
#for the sake of simplicity, let's replace a missing value with the 
#value for that variable for the day before

#first, define the function

replace.missing<-function(vec){
  #create a vector to put our new values into
  New = c()
  for (i in 1:(length(vec))){
    if (is.na(vec[i])){
      vec[i]<-mean(c(vec[i-1], vec[i+1]), na.rm=TRUE)
      #if the data is missing, sub in the value from the measurement before
      
    } else{
      #if the value is not missing, just pass it through to the result vector
      vec[i]<-vec[i]
    }
    New=c(New, vec[i])
  }
  if (any(is.na(New))){
    replace.missing(New)
  }
  return(New)
}
#now let's use our replace missing function to gap fill our weather data
weather$temp_mean_cleaned<-replace.missing(weather$air_temp_mean)
weather$temp_min_cleaned<-replace.missing(weather$air_temp_min)
weather$temp_max_cleaned<-replace.missing(weather$air_temp_max)

# calculate the degree day accumulation for the first half of the day dd1,
#assuming a sine wave structure of temperature over the day
#use a development threshold of 10C, well, because it's a nice number
#to work with
#we'll use the model presented in Allen 1976 which uses daily max and min temperatures
#and assumes temperature follows a sine wave

allen<-function(maxi, mini, thresh){
  #if threshold is not given, assume it's 10 Celcius
  if(missing(thresh)) {
    thresh<-10
  } else {
    thresh<-thresh
  }
  dd1<-c()
  dd2<-c()
  for (i in 1:length(maxi)){
    if (maxi[i]>= thresh & mini[i]<thresh) {
      #first half of day
      #amplitude of temperature difference
      alpha1<-(maxi[i]-mini[i])/2
      #average temperature
      avg1<-(maxi[i]+mini[i])/2
      #theta is time point when temperature crosses the threshold
      #assuming temperature is roughly following the sine curve
      theta1<-asin((thresh-avg1)/alpha1)
      #use these to calculate degree day accumulation over first half of day
      dd1.T<-(1/(2*pi))*((avg1-thresh)*(pi/2 - theta1)+alpha1*cos(theta1))
      dd1<-c(dd1, dd1.T)
      #second half of day
      #two possible cases, min temperature on day i+1 could be below thereshold or above
      #for below threshold:
      if (mini[i+1]<thresh){
        #amplitude of temperature difference
        alpha2<-(maxi[i]-mini[i+1])/2
        #average temperature
        avg2<-(maxi[i]+mini[i+1])/2
        #theta is time point when temperature crosses the threshold
        #assuming temperature is roughly following the sine curve
        theta2<-asin((thresh-avg2)/alpha2)
        #use these to calculate degree day accumulation over first half of day
        dd2.T<-(1/(2*pi))*((avg2-thresh)*(pi/2 - theta2)+alpha2*cos(theta2))
        dd2<-c(dd2, dd2.T)
      } else { #for above threshold
        #second half of day
        avg2<-(maxi[i]+mini[i+1])/2
        dd2.T<-(avg2-thresh)/2
        dd2<-c(dd2, dd2.T)
      }
      
    } else if (mini[i]>=thresh){
      #first half of day
      avg1<-(maxi[i]+mini[i])/2
      dd1.T<-(avg1-thresh)/2
      dd1<-c(dd1, dd1.T)
      #second half of day, as above, two possible cases
      if (mini[i+1]>=thresh){
        avg2<-(maxi[i]+mini[i+1])/2
        dd2.T<-(avg2-thresh)/2
        dd2<-c(dd2, dd2.T)
      } else{
        #amplitude of temperature difference
        alpha2<-(maxi[i]-mini[i+1])/2
        #average temperature
        avg2<-(maxi[i]+mini[i+1])/2
        #theta is time point when temperatur crosses the threshold
        #assuming temperature is roughly following the sine curve
        theta2<-asin((thresh-avg2)/alpha2)
        #use these to calculate degree day accumulation over first half of day
        dd2.T<-(1/(2*pi))*((avg2-thresh)*(pi/2 - theta2)+alpha2*cos(theta2))
        dd2<-c(dd2, dd2.T)
      }
      
    }
    else  {
      #if temperature doesn't get over threshold, no degree days accumulated
      #first half of day
      dd1<-c(dd1, 0)
      #second half of day
      dd2<-c(dd2, 0)
    }
    #total accumulation over the day is just first half of day plus second
    
  }
  
  return(dd1+dd2)
  
}


#do some checks to make sure the function is working properly

weather$dd<-allen(weather$temp_max_cleaned, weather$temp_min_cleaned, 10)



#plot to make sure nothing weird is happening- look for more degree days midyear,
#and NO negative values. Looks like we're WINNING!
plot(weather$DOY, weather$dd)

#now write a new function to calculate accumulated degree days


accum.allen<-function(maxi, mini, thresh, DOY, startday){
  #if startday is not given, assume it's day 1
  if(missing(startday)) {
    startday<-1
  } else {
    startday<-startday
  }
  dd<-allen(maxi, mini, thresh)
  dd.accum<-c()
  for (i in 1:length(dd)){
    #hmm, need a way to sum up over the year, starting anew for each year.
    #this should do it
    if (DOY[i]==1){
      dd.accum.day=0
    }
    #the accumulation on day i is the degree day accumulation before
    #plus the dd accumulated on that day
    dd.accum.day<-dd.accum.day+dd[i]
    
    #but if the degdays are accumulating before the startday, we want to forget them
    if (DOY[i]<startday){
      dd.accum.day=0
    }
    #add that day's accumulation to the vector
    dd.accum<-c(dd.accum, dd.accum.day)
  }
  return (dd.accum)
}

#same sort of checks. Run the function for our data
start<-1
weather$dd.accum<-accum.allen(weather$temp_max_cleaned, weather$temp_min_cleaned, 10, weather$DOY, start)
#and plot that thing to look for problems:
plot(weather$DOY, weather$dd.accum)
#looks good! victory!!!

#we have good reason to think precipitation may also be important for ladybeetles
#let's use the functions developed for the lampyrid analysis to aggregate some precipitation metrics

accum.precip<-function (precip, week){
  precip.acc<-c()
  counter<-week[1]
  accumulation<-0
  for (i in 1:length(precip)){
    if(week[i]==counter){
      accumulation<-accumulation + precip[i]
    }else{
      counter<-week[i]
      accumulation<-precip[i]
    }
    precip.acc<-c(precip.acc, accumulation)
  }
  return(precip.acc)
}

#run the precipitation accumulation function
weather$prec.accum<-accum.precip(weather$precipitation, weather$week)


#looks good! now let's count rainy days
#this is a simple thing, doesn't really need a function to encode for it, but what the heck
#might as well be consistent with how we've handled processing other weather data
#encoding rain days as 0/1 will allow us to simply sum up the number of rainy days for whatever time 
#period we like

rainy.days<-function (precip, week){
  rainy.days<-c()
  for (i in 1:length(precip)){
    if(precip[i]>0){
      raindays<-1
    }else{
      raindays<-0
    }
    rainy.days<-c(rainy.days, raindays)
  }
  return(rainy.days)
}

#and now the rain day counter
weather$rain.days<-rainy.days(weather$precipitation, weather$week)

#finally, we need to be able to compute the accumulated precipitation over the season from a given timepoint
#another function? I think SO! base this one on the degree day accumulation function 


accum.precip.time<-function(precip, DOY, startday){
  #if startday is not given, assume it's day 1
  if(missing(startday)) {
    startday<-1
  } else {
    startday<-startday
  }
  prec.accum<-c()
  for (i in 1:length(DOY)){
    #hmm, need a way to sum up over the year, starting anew for each year.
    #this should do it
    if (DOY[i]==1){
      prec.accum.day=0
    }
    #the accumulation on day i is the precip accumulation before
    #plus the precip accumulated on that day
    prec.accum.day<-prec.accum.day+precip[i]
    
    #but if the precip is accumulating before the startday, we want to forget them
    if (DOY[i]<startday){
      prec.accum.day=0
    }
    #add that day's accumulation to the vector
    prec.accum<-c(prec.accum, prec.accum.day)
  }
  return (prec.accum)
}

weather$prec.accum.0<-accum.precip.time(weather$precipitation, weather$DOY, start)
#and plot that thing to look for problems:
plot(weather$DOY, weather$prec.accum.0)

#now let's put together a weekly 'weather report'

weather1<-group_by(weather, year, week)

weather_weekly<-summarize(weather1,
                          mean.prec=mean(precipitation),
                          rain.days=sum(rain.days),
                          weekly.precip=max(prec.accum),
                          yearly.precip.accum=max(prec.accum.0),
                          max.rainfall=max(precipitation),
                          mean.temp=mean(temp_mean_cleaned),
                          min.temp=min(temp_min_cleaned),
                          max.temp=max(temp_max_cleaned),
                          weekly.dd=max(dd),
                          yearly.dd.accum=max(dd.accum),
                          )

#let's merge in the weather data to the ladybeetle data
#first rename the year column in one of the datasets
lb_weekly1994<-rename(lb_weekly1994, year=Year)
lb_all<-merge(lb_weekly1994, weather_weekly)

#let's do some quick plots to look at ladybeetles by various environmental parameters
lb_all$pertrap<-lb_all$SumOfADULTS/lb_all$TRAPS

#let's look at these data by week
lb_summary_week<-ggplot(lb_all, aes(x=week, y=pertrap, fill=year))+
  #geom_point(pch=21)+
  scale_fill_binned()+
  geom_smooth(aes(color=as.factor(year)))+
  facet_wrap(vars(SPID), nrow=2)

lb_summary_week

#ok, same thing but for degree day accumulation
lb_summary_dd<-ggplot(lb_all, aes(x=yearly.dd.accum, y=pertrap, fill=year))+
  #geom_point(pch=21)+
  scale_fill_binned()+
  geom_smooth(aes(color=as.factor(year)))+
  facet_wrap(vars(SPID), nrow=2)

lb_summary_dd

#let's look at trapping frequency and DD
lb_summary_traps<-ggplot(lb_all, aes(x=yearly.dd.accum, y=TRAPS, fill=year))+
  #geom_point(pch=21)+
  scale_fill_binned()+
  geom_smooth(aes(color=as.factor(year)))+
  facet_wrap(vars(SPID), nrow=2)

lb_summary_traps

#yikes! What are all those 10 trap observations? Christie to investigate!

#let's look at rain days
lb_summary_raindays<-ggplot(lb_all, aes(x=rain.days, y=pertrap, fill=year))+
  #geom_point(pch=21)+
  scale_fill_binned()+
  geom_smooth(aes(color=as.factor(year)))+
  facet_wrap(vars(SPID), nrow=2)

lb_summary_raindays

#let's look at mean temp
lb_summary_meantemp<-ggplot(lb_all, aes(x=mean.temp, y=pertrap, fill=year))+
  #geom_point(pch=21)+
  scale_fill_binned()+
  geom_smooth(aes(color=as.factor(year)))+
  facet_wrap(vars(SPID), nrow=2)

lb_summary_meantemp

#let's look at min temp
lb_summary_mintemp<-ggplot(lb_all, aes(x=min.temp, y=pertrap, fill=year))+
  #geom_point(pch=21)+
  scale_fill_binned()+
  geom_smooth(aes(color=as.factor(year)))+
  facet_wrap(vars(SPID), nrow=2)

lb_summary_mintemp

#let's look at max rainfall
lb_summary_maxrainfall<-ggplot(lb_all, aes(x=max.rainfall, y=pertrap, fill=year))+
  #geom_point(pch=21)+
  scale_fill_binned()+
  geom_smooth(aes(color=as.factor(year)))+
  facet_wrap(vars(SPID), nrow=2)

lb_summary_maxrainfall

#let's look at some ordination- we'll visualize and conduct analyses to describe how 
# the two species are using space, over time.
library(reshape2)
library(vegan)



#create a matrix of observations by community
#create parallel yearly and weekly analyses
landscape.year<-dcast(lb_all, year+REPLICATE+SPID~HABITAT,
                      value.var ="SumOfADULTS",  sum)

landscape.week<-dcast(lb_all, year+week+SPID~HABITAT,
                      value.var ="SumOfADULTS",  sum)
#because we have some rep by week combinations with zero observations, we must remove them prior to analysis
landscape.week.1<-landscape.week[rowSums(landscape.week[4:12])>1,]

#strip out the context- yes I know! this seems counter-intuitive and awful
#but vegan (and most community analysis packages) want your response variable as its own object

com.matrix.year<-landscape.year[,4:12]
com.matrix.week<-landscape.week.1[,4:12]

#skipping a few steps but skeletonizing

ord.year<-metaMDS(com.matrix.year, autotransform=TRUE)
ord.year

plot(ord.year, disp='sites', type='n')
points(ord.year, display="sites", select=which(landscape.year$SPID=="HAXY"), pch=19, col="orange")
points(ord.year, display="sites", select=which(landscape.year$SPID=="C7"), pch=15, col="red")

fit.year<-envfit(ord.year~year+REPLICATE, data=landscape.year, perm=999)
summary(fit.year)
fit.year

plot(fit.year)


#and now for week

ord.week<-metaMDS(com.matrix.week, autotransform=TRUE)
ord.week

plot(ord.week, disp='sites', type='n')
points(ord.week, display="sites", select=which(landscape.week.1$SPID=="HAXY"), pch=19, cex=0.5,col="orange")
points(ord.week, display="sites", select=which(landscape.week.1$SPID=="C7"), pch=15, cex=0.5, col="red")
#ordilabel(ord.week, display="species", cex=0.75, col="black")

#bring the relevant environmental data back into our enviromnetal frame
weekly.context<-merge(landscape.week.1, weather_weekly, all.x = T)

#is the spatiotemporal distribution of harmonia different from that of C7?
#we will do a permanova to check
specmod<-adonis(com.matrix.week~SPID, data=landscape.week.1, method="bray")
specmod

#we're performing a model selection, using backwards selection from all environmental variables
#we're using the P value and R square, and paying attention to which variables seem too colinear to include

fit.week<-envfit(ord.week~year+
                   yearly.precip.accum+yearly.dd.accum,
                 data=weekly.context, perm=999)
summary(fit.week)
fit.week

plot(fit.week)

#save to pdf
pdf("plots/NMDS_weekly.pdf", height=4, width=4)
plot(ord.week, disp='sites', type='n')
points(ord.week, display="sites", select=which(landscape.week.1$SPID=="HAXY"), pch=19, cex=0.5,col="orange")
points(ord.week, display="sites", select=which(landscape.week.1$SPID=="C7"), pch=15, cex=0.5, col="red")

plot(fit.week)
dev.off()

#let's create a quadratic variable that we can play with in our linear model

lb_all$yearly.dd.accum2<-lb_all$yearly.dd.accum^2


#now let's build a simple linear model using the quadratic structure to see what the fit looks like

lm_lb<-lm(SumOfADULTS~SPID*yearly.dd.accum2+SPID*yearly.dd.accum, data=lb_all)
summary(lm_lb)
AIC(lm_lb)

# eq<-function(x){-2.257e-07*x^2 +5.746e-04*x+1.217e+00}
# curve(eq, from=1, to=1000)

#let's build this as a GLM instead
library(MASS)

glm_lb<-glm(SumOfADULTS~SPID*yearly.dd.accum2+SPID*yearly.dd.accum, family=negative.binomial(0.3), data=lb_all)
summary(glm_lb)
AIC(glm_lb)

#let's directly plot the C7 curve
eq<-function(x){exp( -1.635e-06*x^2 + 1.468e-03*x+2.766e-01)}
curve(eq, from=1, to=1000)

#and for Harmonia
eq<-function(x){exp((-1.635e-06+2.174e-06)*x^2 + (1.468e-03-1.343e-03)*x+(2.766e-01-3.704e-01 ))}
curve(eq, from=1, to=1000)



# let's rough in our gam model
library(mgcv)
library(visreg)

gam_lb<-gam(SumOfADULTS~s(year, by=c(as.factor(HABITAT)), sp=0.5)+
                          s(yearly.dd.accum, by=as.factor(SPID), sp=0.5)+
              s(rain.days, by=as.factor(SPID), sp=0.5, k=3)+HABITAT+
              offset(log(TRAPS)), data=lb_all)
summary(gam_lb)
AIC(gam_lb)

#let's visualize this!
visreg(gam_lb, "yearly.dd.accum", "SPID", partial=FALSE, rug=FALSE, 
       overlay=TRUE)

visreg(gam_lb, "rain.days", "SPID", partial=FALSE, rug=FALSE, 
       overlay=TRUE)

visreg(gam_lb, "year", "HABITAT", partial=FALSE, rug=FALSE, 
       overlay=FALSE)



