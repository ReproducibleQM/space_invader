# Main analysis of KBS Ladybeetle 2020 data, Coccinella 
# septempunctata and Harmonia axyridis niche partitioning

#bring data into R

LB<-read.csv(file="data/KBS_Haxy_C7_1989-2020.csv", header=T,
             na.strings=c(NA))


##### let's take a look at this data
summary(LB)

#clean data
#first, we fix dates, make them ISO'ed
library(lubridate)
# 
# #not run
# LB$newdate<-mdy(LB$DATE)#parses the date format used for the forest plots
# LB$newdate<-mdy_hm(LB$DATE)#parses the date format used in the main plots

#well, crap, neither command gets all of the dates to parse correctly
#we have an issue because data from the main site exported with time stamps but 
#the forest site did not. Ugh.


#Christie's solution: brute force removal of timestamps
LB$DATE<-gsub(" 0:00", "", LB$DATE)#remove time stamp strings

LB$newdate<-mdy(LB$DATE)#parses the date format now used by all observations

LB$DOY<-yday(LB$newdate)
LB$week<-isoweek(LB$newdate)

summary(LB)#bingo! looks like it worked!


#let's reorder our habitats right here at the top
LB$HABITAT<-factor(LB$HABITAT, 
                   levels=c("maize", "soybeans","wheat", "alfalfa", "poplar", "ES", "Coniferous", "Deciduous", "Succesional"))
#and relabel them so they're in the same case
levels(LB$HABITAT)<-c("maize", "soybean","wheat", "alfalfa", "poplar", "ES", "coniferous", "deciduous", "succesional")


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

#let's figure out how to re-categorize our treatments. Remember T1-4 are annual, 5-7 are perennial, and the rest are forest
annuallist<-c("T1","T2", "T3", "T4" )
perlist<-c("T5", "T6", "T7")
lb_weekly1994$TREAT_CAT<-ifelse(lb_weekly1994$TREAT %in% annuallist, "Annual",
                                (ifelse(lb_weekly1994$TREAT %in% perlist, "Perennial", "Forest")))

#remember to cull the data at a standard time point 
#(we use DOY 222 in other studies which corresponds to week 32, 
# but this cuts out a major harmonia activity peak, so let's use first week of sept
# =week 35)

lb_weekly1994_culled<-lb_weekly1994[which(lb_weekly1994$week<=35),]
    
  

library(ggplot2)

lb_boxplot<-ggplot(lb_rep, aes(x=TREAT, y=SumOfADULTS, fill=SPID))+
  geom_boxplot()
lb_boxplot

#let's try re-aggregating our data at a yearly resolution
lb_yearly_captures<-aggregate(data=lb_weekly1994_culled, SumOfADULTS~ Year+TREAT+HABITAT+REPLICATE+SPID, FUN=sum)
lb_yearly_N<-aggregate(data=lb_weekly1994_culled, TRAPS~ Year+TREAT+HABITAT+REPLICATE+SPID, FUN=sum)


#also, just so we know what we're comparing here, how many of each species did we catch?
lb_tots<-aggregate(data=lb_weekly1994_culled, SumOfADULTS~ SPID, FUN=sum)
lb_tots

#merge yearly captures with sampling intensity data
lb_yearly<-merge(lb_yearly_captures, lb_yearly_N)
#compute a new variable- average number of beetles per trap
lb_yearly$pertrap<-lb_yearly$SumOfADULTS/lb_yearly$TRAPS



#let's repeat the boxplot but with yearly data
lb_yearly_boxplot<-ggplot(lb_yearly, aes(x=HABITAT, y=pertrap, fill=SPID))+
  geom_boxplot()+
  scale_fill_manual(values=c("darkred", "darkorange"), labels=c("C7", "HA"))+
  labs(x="Plant community", y="Captures per trap", fill="Species")+
  theme_classic()+ theme(legend.position = "none", axis.text.x=element_text(angle=90))
lb_yearly_boxplot

#import ladybug icons
library(jpeg)
library(magick)
library(cowplot)
library(grid)


harm<-magick::image_read("data/ha_square.jpeg")
csept<-magick::image_read("data/c7_square.jpeg")
lbset<-c(csept, harm)

lbicon<-magick::image_append(image_scale(lbset, "100"), stack=TRUE)

#let's look at the populations over time instead
lb_yearly_plot<-ggplot(lb_yearly, aes(x=Year, y=SumOfADULTS, fill=SPID, shape=SPID, linetype=SPID, color=SPID))+
  geom_point(size=0.5, position="jitter", alpha=0.5)+
  geom_smooth()+
  scale_fill_manual(values=c("darkred", "darkorange"), labels=c("C7", "HA"), name="Species")+
  scale_color_manual(values=c("darkred", "darkorange"), labels=c("C7", "HA"), name="Species")+
  scale_shape_manual(values=c(4, 1), labels=c("C7", "HA"), name="Species")+
  scale_linetype_manual(values=c(1, 1), labels=c("C7", "HA"), name="Species")+
  labs(x="Year", y="Captures per trap")+
  theme_classic()+ theme(legend.position = c(0.92, 0.85),legend.background = element_rect(fill='transparent'))+
  annotation_custom(rasterGrob(lbicon), 2016.05, 2017.95, 153, 183)

lb_yearly_plot



rawtrends<-plot_grid(lb_yearly_plot, lb_yearly_boxplot,  ncol=1, rel_widths=c(1), labels=c('A', 'B'), 
                     align="v", axis="l")

rawtrends

pdf("plots/figurerawtrends.pdf", height=8, width=6, bg="white")
rawtrends
dev.off()

svg("plots/figurerawtrends.svg", height=8, width=6, bg="white")
rawtrends
dev.off()




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
lb_weekly1994_culled<-rename(lb_weekly1994_culled, year=Year)
lb_all<-merge(lb_weekly1994_culled, weather_weekly)

#while we're at this, let's make some yearly summary data that will allow us to
#characterize weather by year. Since it looks like seasonality plays a role in within-year 
#partitioning (spoilers!) let's get some accumulations at key points in the season- let's do
#week 25, 30, and 35 and get dd accum, precip accum for each year

keypoints<-c(20, 25, 30, 35)

weather_keypoints<-weather_weekly[which(weather_weekly$week  %in% keypoints),]

#cull out the non-accumulated data

weather_keypoints1<-weather_keypoints[,c(1:2, 6, 12)]

#now we need to recast each of the response columns as their own unique responses by week
#dd accum
library(reshape2)
dd.year<-dcast(weather_keypoints1, year~week,
                      value.var ="yearly.dd.accum",  sum)
colnames(dd.year)<-c("year", "dd20", "dd25", "dd30", "dd35")
#create metrics for DIFFERENCE from last time point too
dd.year$dd25.dif<-dd.year$dd25-dd.year$dd20
dd.year$dd30.dif<-dd.year$dd30-dd.year$dd25
dd.year$dd35.dif<-dd.year$dd35-dd.year$dd30

#precip
precip.year<-dcast(weather_keypoints1, year~week,
               value.var ="yearly.precip.accum",  sum)

colnames(precip.year)<-c("year", "precip20", "precip25", "precip30", "precip35")

#create metrics for DIFFERENCE from last time point too
precip.year$precip25.dif<-precip.year$precip25-precip.year$precip20
precip.year$precip30.dif<-precip.year$precip30-precip.year$precip25
precip.year$precip35.dif<-precip.year$precip35-precip.year$precip30


#ok, now we can merge this into a yearly weather summary matrix

weather_yearly<-merge(dd.year, precip.year)

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
#looks like there are a few rare occasions that the LB were sampled twice in one week (Monday, then Friday?)
#offset in models should account for the worst of that.

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
landscape.week.1<-landscape.week[rowSums(landscape.week[4:12])>2,]

#strip out the context- yes I know! this seems counter-intuitive and awful
#but vegan (and most community analysis packages) want your response variable as its own object

com.matrix.year<-landscape.year[,4:12]
com.matrix.week<-landscape.week.1[,4:12]

#set up ordination with year data

ord.year<-metaMDS(com.matrix.year, autotransform=TRUE)
ord.year

plot(ord.year, disp='sites', type='n')
points(ord.year, display="sites", select=which(landscape.year$SPID=="HAXY"), pch=15, col="orange")
points(ord.year, display="sites", select=which(landscape.year$SPID=="C7"), pch=19, col="red")
ordilabel(ord.year, display="species", cex=0.75, col="black")

#bring the relevant environmental data back into our enviromental frame
yearly.context<-merge(landscape.year, weather_yearly, all.x = T)


#is the spatiotemporal distribution of harmonia different from that of C7 over years?
#we will do a permanova to check
specmod.y<-adonis(com.matrix.year~SPID, data=landscape.year, method="bray")
specmod.y


fit.year<-envfit(ord.year~year+dd35.dif+
                   precip35.dif, data=yearly.context, perm=999)
summary(fit.year)
fit.year


plot(fit.year)

# #save to pdf
# pdf("plots/NMDS_yearly.pdf", height=6, width=6)
# plot(ord.year, disp='sites', type='n')
# points(ord.year, display="sites", select=which(landscape.year$SPID=="HAXY"), pch=19, cex=0.5,col="orange")
# points(ord.year, display="sites", select=which(landscape.year$SPID=="C7"), pch=15, cex=0.5, col="red")
# plot(fit.year)
# ordilabel(ord.year, display="species", cex=0.75, col="black")
# dev.off()



#and now for week

ord.week<-metaMDS(com.matrix.week, autotransform=TRUE)
ord.week

plot(ord.week, disp='sites', type='n')
points(ord.week, display="sites", select=which(landscape.week.1$SPID=="HAXY"), pch=15, cex=0.5,col="orange")
points(ord.week, display="sites", select=which(landscape.week.1$SPID=="C7"), pch=19, cex=0.5, col="red")
ordilabel(ord.week, display="species", cex=0.75, col="black")

#bring the relevant environmental data back into our environmental frame
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

# #save to pdf
# pdf("plots/NMDS_weekly.pdf", height=6, width=6)
# plot(ord.week, disp='sites', type='n')
# points(ord.week, display="sites", select=which(landscape.week.1$SPID=="HAXY"), pch=19, cex=0.5,col="orange")
# points(ord.week, display="sites", select=which(landscape.week.1$SPID=="C7"), pch=15, cex=0.5, col="red")
# ordilabel(ord.week, display="species", cex=0.75, col="black")
# plot(fit.week)
# dev.off()



#built a two-panel PDF
#guh, looks like with the base vegan plots it's still easiest to do the base R
#can we turn these plots into grobs? extract the data, remember we've transposed it so plant 
#community was across the top of the matrix

#yearly
year.scores.species<-as.data.frame(scores(ord.year, "site"))
year.scores.plant<-as.data.frame(scores(ord.year, "species"))
year.scores.plant$Community <- rownames(year.scores.plant)


arrow_factor<-ordiArrowMul(fit.year)
year.data.fit<-as.data.frame(scores(fit.year, display="vectors"))*arrow_factor
year.data.fit$vari<-rownames(year.data.fit)

#make the names on the vectors nicer
year.data.fit$vari<-gsub("precip35.dif","precip35", year.data.fit$vari)
year.data.fit$vari<-gsub("dd35.dif","dd35", year.data.fit$vari)
arrow_factor<-ordiArrowMul(fit.year)
fudgexy<-c(0.1, 0.14, -0.2)#jitter the vector labels a bit
fudgeyy<-c(-0.08, -0.06, 0.12)

yearnmds<-ggplot()+
  geom_point(data=year.scores.species,
             aes(x=NMDS1,y=NMDS2,shape=landscape.year$SPID,colour=landscape.year$SPID), size=1)+# add the point markers
  scale_colour_manual(values=c("C7" = "darkred", "HAXY" = "darkorange"), labels=c("C7", "HA")) +
  scale_shape_manual(values=c("C7" = 4, "HAXY" = 1), labels=c("C7", "HA"))+
  geom_segment(data=year.data.fit, aes(x=0, xend=NMDS1, y=0, yend=NMDS2), 
               arrow=arrow(length = unit(0.03, "npc")), size=0.8, color="blue")+
  geom_label(data=year.data.fit, aes(x=NMDS1+fudgexy, y=NMDS2+fudgeyy, label=vari),size= 5, color="blue", fill="white", alpha=0.7, label.size=NA)+
  geom_label(data=year.scores.plant,aes(x=NMDS1,y=NMDS2,label=Community),size=4,vjust=0, fill="white", alpha=0.9) +  # add the site labels
  coord_fixed()+
  theme_classic()+
  theme(legend.position = "none")

yearnmds

#weekly


week.scores.species<-as.data.frame(scores(ord.week, "site"))
week.scores.plant<-as.data.frame(scores(ord.week, "species"))
week.scores.plant$Community <- rownames(week.scores.plant)


arrow_factorw<-ordiArrowMul(fit.week)
week.data.fit<-as.data.frame(scores(fit.week, display="vectors"))*arrow_factorw
week.data.fit$vari<-rownames(week.data.fit)

#make the names on the vectors nicer
week.data.fit$vari<-gsub("yearly.precip.accum","precip", week.data.fit$vari)
week.data.fit$vari<-gsub("yearly.dd.accum","dd", week.data.fit$vari)
fudgex<-c(0.15, -0.285, 0)#jitter the vector labels a bit
fudgey<-c(0.15, -0.08, 0.17)#jitter the vector labels a bit

weeknmds<-ggplot()+
  geom_point(data=week.scores.species,
             aes(x=NMDS1,y=NMDS2,shape=landscape.week.1$SPID,colour=landscape.week.1$SPID), size=1)+# add the point markers
  scale_colour_manual(values=c("C7" = "darkred", "HAXY" = "darkorange"), labels=c("C7", "HA")) +
  scale_shape_manual(values=c("C7" = 4, "HAXY" = 1), labels=c("C7", "HA"))+
  geom_segment(data=week.data.fit, aes(x=0, xend=NMDS1, y=0, yend=NMDS2), 
               arrow=arrow(length = unit(0.03, "npc")), size=0.8, color="blue")+
  geom_label(data=week.data.fit, aes(x=NMDS1+fudgex, y=NMDS2+fudgey, label=vari),size= 5, color="blue", fill="white", alpha=0.7, label.size=NA)+
  geom_label(data=week.scores.plant,aes(x=NMDS1,y=NMDS2,label=Community),size=4,vjust=0, fill="white", alpha=0.9) +  # add the site labels
  coord_fixed()+
  theme_classic()+
  theme(legend.position = "none")

weeknmds

#ok, finally. Put it together
ggnmds<-plot_grid(yearnmds, weeknmds,  ncol=1, rel_widths=c(1), labels=c('A', 'B'), 
                     align="h", axis="l")

ggnmds



pdf("plots/figureNMSDs1.pdf", height=10, width=6)
ggnmds
dev.off()





# let's rough in our gam models. Just like with the multivariate analysis, we'll look at
#two different scales- within year dynamics and between year dynamics
library(mgcv)
library(visreg)
library(ggpubr)
library(Hmisc)

#pearson correlation of environmental parameters

round(cor(lb_all[10:19], method="pearson"), digits=2)
#start withe the drivers of within-year variation

library(psych)
pairs.panels(lb_all[c(10,11,13:19)], #drop 'weekly precipitation because it's = mean daily precip*7
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = FALSE # show correlation ellipses
)

pdf("plots/figurewithinyearpairsplot.pdf", height=6, width=8)
pairs.panels(lb_all[c(10,11,13:19)], #drop 'weekly precipitation because it's = mean daily precip*7
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = FALSE # show correlation ellipses
)
dev.off()

gam_lb<-gam(SumOfADULTS~s(yearly.dd.accum, by=as.factor(SPID), sp=1)+
              s(max.rainfall, by=as.factor(SPID), sp=1)+
              s(max.temp, by=as.factor(SPID), sp=1)+
              HABITAT*SPID+
              s(year, by=as.factor(SPID), sp=1)+
              offset(log(TRAPS)), method="REML", data=lb_all, family="quasipoisson")
summary(gam_lb)
anova.gam(gam_lb)

#check concurvity
concurvity(gam_lb)
#looks fine, sweet!
gam.check(gam_lb)

#let's visualize this!
#must detach psych- it conflicts with ggplot
detach(package:psych,unload=TRUE)

withinyear.dd<-visreg(gam_lb, "yearly.dd.accum", by="SPID", partial=FALSE, rug=FALSE, 
                      overlay=TRUE, scale="response", gg=TRUE,
                      line=list(lty=1))+
  scale_fill_manual(values=alpha(c("darkred", "darkorange"), 0.4), labels=c("C7", "HA"), name="Species")+
  scale_color_manual(values=c("darkred", "darkorange"), labels=c("C7", "HA"), name="Species")+
  labs(x="Degree day accumulation", y="")+
  theme_classic()+ theme(legend.position = "none")+
  coord_cartesian(ylim=c(0, 8))

withinyear.dd

withinyear.rain<-visreg(gam_lb, "max.rainfall", "SPID", partial=FALSE, rug=FALSE, 
       overlay=TRUE, scale="response", gg=TRUE, line=list(lty=1))+
  scale_fill_manual(values=alpha(c("darkred", "darkorange"), 0.4), labels=c("C7", "HA"), name="Species")+
  scale_color_manual(values=c("darkred", "darkorange"), labels=c("C7", "HA"), name="Species")+
  labs(x="Maximum daily rainfall within week (mm)", y="")+
  theme_classic()+ theme(legend.position = "none")+
  coord_cartesian(ylim=c(0, 8))

withinyear.rain

withinyear.temp<-visreg(gam_lb, "max.temp", "SPID", partial=FALSE, rug=FALSE, 
                        overlay=TRUE, scale="response", gg=TRUE, line=list(lty=1))+
  scale_fill_manual(values=alpha(c("darkred", "darkorange"), 0.4), labels=c("C7", "HA"), name="Species")+
  scale_color_manual(values=c("darkred", "darkorange"), labels=c("C7", "HA"), name="Species")+
  labs(x="Maximum temperature within week (C)", y="")+
  theme_classic()+ theme(legend.position = "none")+
  coord_cartesian(ylim=c(0, 8))

withinyear.temp



withinyear.habitat<-visreg(gam_lb, "HABITAT","SPID", partial=FALSE, rug=FALSE, 
       overlay=TRUE, scale="response", gg=TRUE,
       line=list(lty=1))+
  scale_fill_manual(values=alpha(c("darkred", "darkorange"), 0.4), labels=c("C7", "HA"), name="Species")+
  scale_color_manual(values=c("darkred", "darkorange"), labels=c("C7", "HA"), name="Species")+
  labs(x="Habitat", y="")+
  theme_classic()+ theme(legend.position = "none", axis.text.x=element_text(angle=90, vjust=0.5, hjust=1))+
  coord_cartesian(ylim=c(0, 8))

withinyear.habitat

withinyear.yearly<-visreg(gam_lb, "year", "SPID", partial=FALSE, rug=FALSE, 
       overlay=TRUE, scale="response", gg=TRUE,
       line=list(lty=1))+
  scale_fill_manual(values=alpha(c("darkred", "darkorange"), 0.4), labels=c("C7", "HA"), name="Species")+
  scale_color_manual(values=c("darkred", "darkorange"), labels=c("C7", "HA"), name="Species")+
  labs(x="Year\n\n\n", y="")+
  theme_classic()+ theme(legend.position = c(0.92, 0.85),legend.background = element_rect(fill='transparent'))+
  coord_cartesian(ylim=c(0, 8))

withinyear.yearly

#plot the withinyear model all together:

withinyear.modelplot<-plot_grid(withinyear.dd, withinyear.rain, withinyear.temp, withinyear.habitat, withinyear.yearly, 
                              ncol=3, rel_widths=c(1, 1, 1), rel_heights = c(0.85, 1), labels=c('A', 'B', 'C', 'D', 'E'))
withinyear.modelplot

#create overall y axis label
partresid<-text_grob(paste("        Partial residual captures"), color="black", size=12, rot=90)


#now replot with grob label
withinyear.plot<-plot_grid(partresid, withinyear.modelplot, ncol=2, rel_widths = c(1,11))

withinyear.plot

pdf("plots/figurewithinyeargam.pdf", height=5, width=10)
withinyear.plot
dev.off()



#we'll want to extract the data associated with activity peaks

#ok, I think we found the method we should use! here's the tutorial:
# https://fromthebottomoftheheap.net/2014/05/15/identifying-periods-of-change-with-gams/

#first we create a new dataframe that we can use our model to predict the values for

#create data for C7, holding everything constant but degree days
newData.C7 <- with(lb_all,
                  data.frame(yearly.dd.accum = seq(0, 1500, length = 300),
                             TRAPS=5, 
                             year=mean(year), 
                             max.rainfall=mean(max.rainfall), 
                             max.temp=mean(max.temp), 
                             SPID="C7", 
                             HABITAT="alfalfa"))

#make the same frame but for 1 more degday
newData.C7.1<- with(lb_all,
                     data.frame(yearly.dd.accum = seq(1, 1501, length = 300),
                                TRAPS=5, 
                                year=mean(year), 
                                max.rainfall=mean(max.rainfall),
                                max.temp=mean(max.temp), 
                                SPID="C7", 
                                HABITAT="alfalfa"))

#make predictions
predict.dd.C7<-predict(gam_lb, newData.C7, type="link")
predict.dd.C7.1<-predict(gam_lb, newData.C7.1, type="link")

dd.C7.der<-as.data.frame(cbind(newData.C7$yearly.dd.accum, predict.dd.C7, predict.dd.C7.1))
dd.C7.der$slope<-(dd.C7.der$predict.dd.C7.1-dd.C7.der$predict.dd.C7)/1

#then let's do the same thing with harmonia

newData.HAXY <- with(lb_all,
                   data.frame(yearly.dd.accum = seq(0, 1500, length = 300),
                              TRAPS=5, 
                              year=mean(year), 
                              max.rainfall=mean(max.rainfall), 
                              max.temp=mean(max.temp), 
                              SPID="HAXY", 
                              HABITAT="alfalfa"))

#make the same frame but for 1 more degday
newData.HAXY.1<- with(lb_all,
                    data.frame(yearly.dd.accum = seq(1, 1501, length = 300),
                               TRAPS=5, 
                               year=mean(year), 
                               max.rainfall=mean(max.rainfall),
                               max.temp=mean(max.temp), 
                               SPID="HAXY", 
                               HABITAT="alfalfa"))

#make predictions
predict.dd.HAXY<-predict(gam_lb, newData.HAXY, type="link")
predict.dd.HAXY.1<-predict(gam_lb, newData.HAXY.1, type="link")

dd.HAXY.der<-as.data.frame(cbind(newData.HAXY$yearly.dd.accum, predict.dd.HAXY, predict.dd.HAXY.1))
dd.HAXY.der$slope<-(dd.HAXY.der$predict.dd.HAXY.1-dd.HAXY.der$predict.dd.HAXY)/1

#ok, now let's predict the mean captures for each habitat. Let's do this at the C7 activity peak

newData.C7.habitat<- with(lb_all,
                          data.frame(yearly.dd.accum = 1250,
                                                TRAPS=5, 
                                                year=mean(year), 
                                                max.rainfall=mean(max.rainfall), 
                                                max.temp=mean(max.temp), 
                                                SPID="C7", 
                                                HABITAT="ES"))
predict(gam_lb, newData.C7.habitat, type="link")



#soybean 1.74
#wheat 1.93
#alfalfa 1.76
#ES 1.31
#poplar



newData.HAXY.habitat<- with(lb_all,
                          data.frame(yearly.dd.accum = 1250,
                                     TRAPS=5, 
                                     year=mean(year), 
                                     max.rainfall=mean(max.rainfall), 
                                     max.temp=mean(max.temp), 
                                     SPID="HAXY", 
                                     HABITAT="poplar"))
predict(gam_lb, newData.HAXY.habitat, type="link")

#alfalfa 0.65
#es 0.49
# Poplar


#now, what accounts for the year to-year variation in absolute numbers of both species? 
#let's do another gam, but with the yearly aggregated data and the summary weather metrics
lb_yearly<-rename(lb_yearly, year=Year)
#also because we're looking for drivers, let's re-arrange the data so one beetle can
#be considered as a driver for the other
lb_yearly_1<-dcast(lb_yearly, year+REPLICATE+TREAT+HABITAT~SPID,
                                   value.var ="SumOfADULTS",  sum)

lb_yearly_2<-merge(lb_yearly_1,unique(lb_yearly[,c(1:4,7)]), all.x=T)

lb_yearly_weather<-merge(lb_yearly_2, weather_yearly, all.x=T)



#another gam using this data aggregated by year but not by rep, and because the
#species seem to be responding to different factors, let's do one at a time. Because autocorrelation, let's do
#a forward selection

#first let's find out what's super-correlated

target_data<-lb_yearly_weather[,c(8,12:15,19:21)]

cor(target_data, method = "pearson")
#let's flag things that are >abs(0.3)

library(psych)
pairs.panels(target_data, 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = FALSE # show correlation ellipses
)


pdf("plots/figurebetweenyearpairsplot.pdf", height=6, width=8)
pairs.panels(target_data, 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = FALSE # show correlation ellipses
)
dev.off()

#must detach psych- it conflicts with ggplot
detach(package:psych,unload=TRUE)

#model selection criteria- add params one by one by highest F value.
#in subsequent steps, eliminate params with >abs(0.3) Pearson correlation with any parameters in model
#each time a parameter is added, check concurvity, if it exceeds 0.5 (observed) for any parameter, eliminate
#that parameter from consideration


gam_haxy_yearly<-gam(HAXY~#s(dd20, sp=1)+
                     #s(dd25.dif, sp=1)+
                     s(dd30.dif, sp=1)+
                     #s(dd35.dif, sp=1)+
                     s(precip20, sp=1)+
                     #s(precip25.dif, sp=1)+
                     #s(precip30.dif, sp=1)+
                     s(precip35.dif, sp=1)+
                       HABITAT+
                     offset(log(TRAPS)), data=lb_yearly_weather, family="quasipoisson", method="REML")
summary(gam_haxy_yearly)
anova.gam(gam_haxy_yearly)

#because the model has a lot of variables that are probably a bit autocorrelated,
#check concurvity to see if it needs simplification- aim to get observed <0.5 for all pairwise comparisons of variables


concurvity(gam_haxy_yearly)

gam.check(gam_haxy_yearly)

#for plotting purposes, let's also pull the average value of each weather measurement

avg.dd20<-mean(weather_yearly$dd20)
avg.dd25<-mean(weather_yearly$dd25.dif)
avg.dd30<-mean(weather_yearly$dd30.dif)
avg.dd35<-mean(weather_yearly$dd35.dif)
avg.precip20<-mean(weather_yearly$precip20)
avg.precip25<-mean(weather_yearly$precip25.dif)
avg.precip30<-mean(weather_yearly$precip30.dif)
avg.precip35<-mean(weather_yearly$precip35.dif)

avg.haxy<-mean(lb_yearly_weather$HAXY)
avg.c7<-mean(lb_yearly_weather$C7)



haxy.dd30<-visreg(gam_haxy_yearly, "dd30.dif", partial=F, rug=FALSE, 
                  overlay=TRUE,scale="response", gg=T, ylab="", 
                  xlab=NULL, line=list(col="darkorange", lty=1),
                  fill=list(fill="darkorange", alpha=0.4))+
  coord_cartesian(ylim=c(0, 35))+
  theme_classic()+
  geom_vline(xintercept=avg.dd30, linetype="dashed", color="blue", size=1)
haxy.dd30




haxy.precip20<-visreg(gam_haxy_yearly, "precip20", partial=F, rug=FALSE, 
                      overlay=TRUE,scale="response", gg=T, ylab="", 
                      xlab=NULL, line=list(col="darkorange", lty=1),
                      fill=list(fill="darkorange", alpha=0.4))+
  coord_cartesian(ylim=c(0, 35))+
  theme_classic()+
  geom_vline(xintercept=avg.precip20, linetype="dashed", color="blue", size=1)
haxy.precip20


haxy.precip35<-visreg(gam_haxy_yearly, "precip35.dif", partial=F, rug=FALSE, 
                      overlay=TRUE,scale="response", gg=T, ylab="", 
                      xlab=NULL, line=list(col="darkorange", lty=1),
                      fill=list(fill="darkorange", alpha=0.4))+
  coord_cartesian(ylim=c(0, 35))+
  theme_classic()+
  geom_vline(xintercept=avg.precip35, linetype="dashed", color="blue", size=1)
haxy.precip35


##### now C7

gam_c7_yearly<-gam(C7~ #s(dd20, sp=1)+
                       s(dd25.dif, sp=1)+
                       #s(dd30.dif, sp=1)+
                       s(dd35.dif, sp=1)+
                       #s(precip20, sp=1)+ 
                       #s(precip25.dif, sp=1)+
                       s(precip30.dif, sp=1)+
                       #s(precip35.dif, sp=1)+
                     HABITAT+
                       offset(log(TRAPS)), data=lb_yearly_weather, family="quasipoisson", method="REML")
summary(gam_c7_yearly)

anova.gam(gam_c7_yearly)
#because the model has a lot of variables that are probably a bit autocorrelated,
#check concurvity to see if it needs simplification- aim to get observed <0.5 for all values


concurvity(gam_c7_yearly)

gam.check(gam_c7_yearly)





C7.dd25<-visreg(gam_c7_yearly, "dd25.dif", partial=F, rug=FALSE,
                overlay=TRUE,scale="response",gg=T, ylab="",
                xlab=NULL,line=list(col="darkred", lty=1),
                fill=list(fill=rgb(209,153,153, maxColorValue = 255)))+
  coord_cartesian(ylim=c(0, 130))+
  theme_classic()+
  geom_vline(xintercept=avg.dd25, linetype="dashed", color="blue", size=1)

C7.dd25


C7.dd35<-visreg(gam_c7_yearly, "dd35.dif", partial=F, rug=FALSE,
                overlay=TRUE,scale="response",gg=T, ylab="",
                xlab=NULL,line=list(col="darkred", lty=1),
                fill=list(fill=rgb(209,153,153, maxColorValue = 255)))+
  coord_cartesian(ylim=c(0, 130), xlim=c(310, 460))+
  theme_classic()+
  geom_vline(xintercept=avg.dd35, linetype="dashed", color="blue", size=1)

C7.dd35


C7.precip30<-visreg(gam_c7_yearly, "precip30.dif", partial=F, rug=FALSE,
                    overlay=TRUE,scale="response", gg=T, ylab="",
                    xlab=NULL,line=list(col="darkred", lty=1),
                    fill=list(fill=rgb(209,153,153, maxColorValue = 255)))+
  coord_cartesian(ylim=c(0, 130))+
  theme_classic()+
  geom_vline(xintercept=avg.precip30, linetype="dashed", color="blue", size=1)

C7.precip30



blankspace<-text_grob(paste(""), color="black")
harmonia<-text_grob(paste("Harmonia axyridis"), color="black", face="italic", size=13)
coccinella<-text_grob(paste("Coccinella septempunctata"), color="black", face="italic", size=13)
Competitor<-text_grob(paste("Captures of\ncompetitor"), color="black", size=11)
dd20<-text_grob(paste("Degree days\n  at 20 weeks"), color="black", size=11)
dd25L<-text_grob(paste("Degree days\n  at 25 weeks"), color="black", size=11)
dd25<-text_grob(paste("\n  at 25 weeks"), color="black", size=11)
dd30<-text_grob(paste("\n  at 30 weeks"), color="black", size=11)
dd35<-text_grob(paste("\n  at 35 weeks"), color="black", size=11)
precip20<-text_grob(paste("Precipitation\nat 20 weeks"), color="black", size=11)


#create a giant flippin' plot with all the panels

between_years<-plot_grid(blankspace, harmonia, coccinella,
                         dd25L, blankspace, C7.dd25,
                         dd30, haxy.dd30, blankspace,
                         dd35, blankspace, C7.dd35, 
                         precip20, haxy.precip20, blankspace,
                         dd30, blankspace, C7.precip30,
                         dd35, haxy.precip35, blankspace, 
                         ncol=3, rel_widths=c(1,4,4),rel_heights = c(0.3,1,1,1,1,1,1,1), align="v", axis="l")

between_years

pdf("plots/figurebetweenyeargam.pdf", height=8, width=8)
between_years
dev.off()



