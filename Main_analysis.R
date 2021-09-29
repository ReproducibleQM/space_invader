# Main analysis of KBS Ladybeetle 2020 data, Coccinella 
# septempunctata and Harmonia axyridis niche partitioning

#bring data into R

LB<-read.csv(file="data/KBS_Haxy_C7_1989-2020.csv", header=T,
             na.strings=c(NA))


sum.of.squares <- function(x,y) {#paste in demo function that calculates sum of squares
  x^2 + y^2 #add square of x to square of y
}

sum.of.squares(3,4)#demo sum of squares function

iszero<-function(number){#create a function that determines if a value is zero
  if(number==0){
    print("yes")#if the number is zero, print yes
  }else{
    print("no")#otherwise print no
  }
}

iszero("cherry pie")#demonstrate iszero function

vec<-c(1,2,3,0)#define a test vector for demonstrating a function on a loop

for(i in 1:length(vec)){#demonstrate a simple loop 
  iszero(vec[i])
}

summary(LB)

#clean data
#fix dates, make them ISO'ed
library(lubridate)
LB$newdate<-mdy(LB$DATE)#parses the date format used for the forest plots
LB$newdate<-mdy_hm(LB$DATE)#parses the date format used in the main plots



###################################
#UNADAPTED PASTED CODE BELOW WILL NOT WORK

#extract year
lampyrid$year<-year(lampyrid$newdate)
#extract day of year. DOY is very useful for a phenology type analyses
#because you don't have to deal with day-of-month numbers starting over 
#in the middle of a phenological event.
lampyrid$DOY<-yday(lampyrid$newdate)
#use ISO week, so we start counting on Monday, not Jan 1, COOL! Our sampling usually 
#takes place Wed-Friday, so if we use week of year stating on Jan 1, there is a good chance that
#samples taken within a sampling week would get grouped incorrectly when we go to do the analysis.
lampyrid$week<-isoweek(lampyrid$newdate)

#let's look for the data problems we found we used OpenRefine and see if
#we can impliment our cleaning operations here- that way we have a complete
#record of EVERYTHING that happened to these data. Recall there were issues 
#with TREAT_DESC
#let's look at these columns individually and fix errors as we find them
#and we should also check for weirdness in our numeric values

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
#a good way to check that the importation was sucessful

plot(weather$DOY, weather$air_temp_mean)
plot(weather$DOY, weather$precipitation)

#because we don't have lampyrid records before 2004, let's cut out the data
#from before 2003 so we can process the weaqther data more quickly. Also our
#lampyrid data stops at the end of 2015 and for some reason the new
#weather station data breaks our code. DANGIT. so we'll cut off the weather
#data that's causing us problems- we don't need it anyway
weather<-subset(weather, weather$year>2003& weather$year<2016)


#lets also get rid of the vairables we don't need:
weather$flag_precip<-NULL
weather$flag_air_temp_mean<-NULL
weather$flag_air_temp_max<-NULL
weather$flag_air_temp_min<-NULL

#also, these data are sorted in decending order. It's easier to think of this 
#stuff in ascending order, so let's sort the data by year and DOY

weather<-weather[order(weather$year, weather$DOY),]

#lets's pre-process these weather data so we get rid of missing values
# we can write a function to do this for us.
#if missing data is rare, it is probably safe to assume that missing
#temperatures are similar to the weather on the day before or after.
#for the sake of simplicity, let's replace a missing value with the 
#value for that variable for the day before
