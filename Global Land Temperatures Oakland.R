
# Global Land Temperatures for Oakland


# Load the preprocessed libraries as needed.


library(ggplot2)
library(zoo)
library(randomForest)
library(data.table)



# Let's load the temperature data by the city, Let's say Oakland

GlobalLandTemperaturesByCity <- read.csv('./GlobalLandTemperaturesByCity.csv')
GlobalLandTemperaturesByCity <- fread("./GlobalLandTemperaturesByCity.csv")



sunPosition <- function(year, month, day, hour=12, min=0, sec=0,lat=37.80, long=122.27) 
{
  twopi <- 2 * pi
  deg2rad <- pi / 180
  
  # Get day of the year, e.g. Feb 1 = 32, Mar 1 = 61 on leap years
  month.days <- c(0,31,28,31,30,31,30,31,31,30,31,30)
  day <- day + cumsum(month.days)[month]
  leapdays <- year %% 4 == 0 & (year %% 400 == 0 | year %% 100 != 0) & 
    day >= 60 & !(month==2 & day==60)
  day[leapdays] <- day[leapdays] + 1
  
  # Get Julian date - 2400000
  hour <- hour + min / 60 + sec / 3600 # hour plus fraction
  delta <- year - 1949
  leap <- trunc(delta / 4) # former leapyears
  jd <- 32916.5 + delta * 365 + leap + day + hour / 24
  
  # The input to the Atronomer's almanach is the difference between
  # the Julian date and JD 2451545.0 (noon, 1 January 2000)
  time <- jd - 51545.
  
  # Ecliptic coordinates
  
  # Mean longitude
  mnlong <- 280.460 + .9856474 * time
  mnlong <- mnlong %% 360
  mnlong[mnlong < 0] <- mnlong[mnlong < 0] + 360
  
  # Mean anomaly
  mnanom <- 357.528 + .9856003 * time
  mnanom <- mnanom %% 360
  mnanom[mnanom < 0] <- mnanom[mnanom < 0] + 360
  mnanom <- mnanom * deg2rad
  
  # Ecliptic longitude and obliquity of ecliptic
  eclong <- mnlong + 1.915 * sin(mnanom) + 0.020 * sin(2 * mnanom)
  eclong <- eclong %% 360
  eclong[eclong < 0] <- eclong[eclong < 0] + 360
  oblqec <- 23.439 - 0.0000004 * time
  eclong <- eclong * deg2rad
  oblqec <- oblqec * deg2rad
  
  # Celestial coordinates
  # Right ascension and declination
  num <- cos(oblqec) * sin(eclong)
  den <- cos(eclong)
  ra <- atan(num / den)
  ra[den < 0] <- ra[den < 0] + pi
  ra[den >= 0 & num < 0] <- ra[den >= 0 & num < 0] + twopi
  dec <- asin(sin(oblqec) * sin(eclong))
  
  # Local coordinates
  # Greenwich mean sidereal time
  gmst <- 6.697375 + .0657098242 * time + hour
  gmst <- gmst %% 24
  gmst[gmst < 0] <- gmst[gmst < 0] + 24.
  
  # Local mean sidereal time
  lmst <- gmst + long / 15.
  lmst <- lmst %% 24.
  lmst[lmst < 0] <- lmst[lmst < 0] + 24.
  lmst <- lmst * 15. * deg2rad
  
  # Hour angle
  ha <- lmst - ra
  ha[ha < -pi] <- ha[ha < -pi] + twopi
  ha[ha > pi] <- ha[ha > pi] - twopi
  
  # Latitude to radians
  lat <- lat * deg2rad
  
  # Azimuth and elevation
  el <- asin(sin(dec) * sin(lat) + cos(dec) * cos(lat) * cos(ha))
  az <- asin(-cos(dec) * sin(ha) / cos(el))
  
  # For logic and names, see Spencer, J.W. 1989. Solar Energy. 42(4):353
  cosAzPos <- (0 <= sin(dec) - sin(el) * sin(lat))
  sinAzNeg <- (sin(az) < 0)
  az[cosAzPos & sinAzNeg] <- az[cosAzPos & sinAzNeg] + twopi
  az[!cosAzPos] <- pi - az[!cosAzPos]
  
  # if (0 < sin(dec) - sin(el) * sin(lat)) {
  #     if(sin(az) < 0) az <- az + twopi
  # } else {
  #     az <- pi - az
  # }
  
  
  el <- el / deg2rad
  az <- az / deg2rad
  lat <- lat / deg2rad
  
  return(list(elevation=el, azimuth=az))
}



## Let's Try to Use the Oakland Data to get some insights.


oakland<-na.omit(subset(GlobalLandTemperaturesByCity,City=="Oakland"))
oakland$dt<-as.Date(oakland$dt,"%Y-%m-%d")
oakland$lat<-as.numeric(gsub("N|E|S|W", "",oakland$Latitude))*ifelse(grepl("S",oakland$Latitude),-1,1)
oakland$long<-as.numeric(gsub("N|E|S|W", "",oakland$Longitude))*ifelse(grepl("W",oakland$Longitude),-1,1)
oakland$Month<-as.numeric(format(oakland$dt,"%m"))
oakland$Month.String<-format(oakland$dt,"%B")
oakland$Year<-as.numeric(format(oakland$dt,"%Y"))
oakland$elevation<-with(oakland,sunPosition(as.numeric(format(dt,"%Y")),as.numeric(format(dt,"%m")),1,12,0,0,lat,long)$elevation)
oakland$azimuth<-with(oakland,sunPosition(as.numeric(format(dt,"%Y")),as.numeric(format(dt,"%m")),1,12,0,0,lat,long)$azimuth)



# Graphing the Temperatures:


ggplot(oakland,aes(x=dt,y=AverageTemperature,color=reorder(Month.String,-AverageTemperature,mean)))+
  geom_point()+geom_smooth()+ggtitle("Average Temperatures by\nMonth in Oakland")+
  xlab("Year")+ylab("Average Temperature")+labs(color='Month')
rm(mean)




# Temperaure Uncertainty


ggplot(oakland,aes(x=dt,y=AverageTemperatureUncertainty))+
  geom_point(shape=1)+geom_smooth()+ggtitle("Average Temperature Uncertainty\nOver Time In Oakland")+
  xlab("Year")+ylab("Average Temperature Uncertainty")




# Temperature Uncertainty 100 Years Ago
Demonstrating the accuracy for the last 100 years ago.

ggplot(oakland[Year>1916,],aes(x=dt,y=AverageTemperatureUncertainty))+
  geom_point(shape=1)+geom_smooth()+ggtitle("Temperature Uncertainty Average\nPast 100 Years In Oakland")+
  xlab("Year")+ylab("Temperature Uncertainty Average")




# Density Plot

In this section, the graph demostrates the distribution on the temperature uncertainty for the last 100 years in Oakland.

ggplot(oakland[Year>1916,], aes(x=AverageTemperatureUncertainty)) + geom_density()+
  ggtitle("Density Plot of Temperature Uncertainty Average\npast 100 years in Oakland")



# Random Forest


rf<-randomForest(subset(oakland,select=c(Year,elevation,azimuth,AverageTemperatureUncertainty)),oakland$AverageTemperature)
varImpPlot(rf,main="Variable Importance in Determining\nOakland Average Temperatures")


