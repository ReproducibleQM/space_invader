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
#mikes trying to connect to the interwebs
