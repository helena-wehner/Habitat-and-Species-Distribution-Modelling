##################################################################################################

### SSF
### Buffalo example (J. Schwalb-Willmann)

##################################################################################################

buffalo
head(buffalo)
plot(buffalo)

# buffalo data ist im package adehabitatHS enthalten !!! --> movement data

par(mfrow=c(1,1))

# anhand des Bsp. aus dem Paper eine SSF nun mit den Büffel Daten probieren
setwd("C:/Users/Lenovo/Desktop/")
# loading packages
library(raster)
library(lubridate)
library(amt)
library(tidyverse)
library(move)
library(adehabitatHS)

# loading shapefile/movement data
data(buffalo) # from adehabitatHS package

plot(buffalo$habitat)
plot(buffalo$traj)

head(buffalo$traj)

plot(buffalo) # large move stack 

tail(buffalo)
head(buffalo)

### SSF for only one individual

# movement data from only one individual
buffalo.df <- as(buffalo, "data.frame")
toni <- buffalo.df[buffalo.df$trackId=="Toni",]
plot(x=toni$coords.x1, y = toni$coords.x2)

# toni Shapefile


# second way to do so
cilla <- filter(buffalo.df, trackId=="Cilla")
head(cilla)

# creating move object
cilla.m <- move(x=cilla$coords.x1, y=cilla$coords.x2, time = as.POSIXct(cilla$timestamps, format= "%Y-%m-%d %H:%M:%S", tz="UTC"), animal="cilla", data=cilla)
plot(cilla.m)

# creating a track (basic building block of amt package)
toni <-  filter(!is.na("coords.x1"))
cilla.t <- as_track(cilla.m, .t=as.POSIXct(cilla$timestamps, format="%Y-%m-%d %H:%M:%S"))
str(cilla.t)
plot(cilla.t)
names(cilla.t)
head(cilla.t)

# not neeeded
head(cilla)
cilla.df <- data.frame(x=cilla$coords.x1, y=cilla$coords.x2, t=as.POSIXct(cilla$timestamps, format="%Y-%m-%d %H:%M:%S"))
head(cilla.df)

obs.time <- as.POSIXct(cilla$timestamps, format="%Y-%m-%d %H:%M:%S")
cilla.track <- track(x=cilla$coords.x1, y=cilla$coords.x2, t=obs.time)
head(cilla.track)

# summarize sampling rate
amt::summarize_sampling_rate(cilla.track)

stps <- amt::track_resample(cilla.track, rate = minutes(10), tolerance = seconds(60))%>%
  filter_min_n_burst(min_n=3)%>%
  steps_by_burst()%>%
  time_of_day(include.crepuscule=F)



##################################
##################################
##################################
### SSF NBI Toscana ##############
library(amt)

# first: getting data of one NBI at the wintering site/Toscana
# Rudini (handraised 2019)

nbi <- readOGR("GPS_data_Helena/2019_Q4.shp")
head(nbi)

name <- c("Rudini","Cupi","Calimero","Atreju","Hansi","Hitschi","Hicks","Ohnezahn","Alfonso","Vincino","Paolo","Marcello","Albus","Hedwig","Ginny",
          "Luise","Bernadette","Linus","Rupert","El Comandante","Sir Angus","Gero","Shaun","Asterix","Obelix","Cesare","Kazoo","Spock","Scotty","Yoda")

juvenile_hand <- list()

for (i in 1:length(name)){
  juvenile_hand[[i]] <- subset(nbi,bird_name == name[i],)
}

head(juvenile_hand)

rudini <- juvenile_hand[[1]]
head(rudini)

rudini.df <- data.frame(x=rudini$lon, y=rudini$lat, t=rudini$timestamp)
head(rudini.df)

# is the data ordered?
rudini.df <- rudini.df[order(rudini.df$t),]

# movement data in amt package format
rud.tr <- amt::track(tbl = rudini.df, x = rudini.df$x, y=rudini.df$y, t=as.POSIXct(rudini.df$t, format="%Y-%m-%d %H:%M:%S"))
head(rud.tr)

summarize_sampling_rate(rud.tr)

stps <- amt::track_resample(rud.tr, rate = minutes(20), tolerance = seconds(60))%>%
  filter_min_n_burst(min_n=3)%>%
  steps_by_burst()%>%
  time_of_day(include.crepuscule=F)







