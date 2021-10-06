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

summary(LB)#bingo! looks like it worked!

###################################
#Begin weather data processing



#download weather data from KBS weather station
weather<-read.table(file="http://lter.kbs.msu.edu/datatables/7.csv",
                    header=T, sep=",", na.strings="")
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

weather$dd.accum<-accum.allen(weather$air_temp_max_clean, weather$air_temp_min_clean, 10, weather$DOY, start)
#and plot that thing to look for problems:
plot(weather$DOY, weather$dd.accum)
#looks good! victory!!!

#let's also compute degree day accumulation from the beginning of year- we may need to see how the winter affected 
#the lampyrids if we can't explain all the variation
weather$dd.accum0<-accum.allen(weather$air_temp_max_clean, weather$air_temp_min_clean, 10, weather$DOY, 1)

