#install.packages("ctmm")
# install.packages("rgeos")
# install.packages("dismo")
# install.packages("tmaptools")
# install.packages("tmap")tmaptools
# install.packages("")
library(ctmm)
library(dplyr)
library(dismo)
library("rgeos")
library(tmaptools)
library(tmap)
library(sf)

DATUM <- "+proj=longlat +datum=WGS84"
data(buffalo) 
cilla<-buffalo[[1]]
plot(cilla)

test.data <- read.csv("Data/testOneDayData.csv")
test.data <- removedups(test.data)
 test.data.tele <- as.telemetry(test.data)


tag.17054 <- test.data %>% 
  select(timestamp) 


#%>%
  filter(tag.local.identifier=="17054")  
  

 
as.telemetry()


cilla <- data %>% 
  filter(tag.local.identifier==170541)

colnames(cilla)[10] <-"x"
colnames(cilla)[11] <-"y" 
plot(cilla)
class(cilla)

test <- read.csv("Data/2018&2019AllRaw.csv")
max(test$external.temperature,na.rm = T)
min(test$external.temperature,na.rm = T)
mins <- which(test$external.temperature<1)
mins <- test[which(test$external.temperature<1),]
p <- ggplot(test, aes(x=external.temperature)) +
geom_histogram(binwidth=0.5)
p
maxs <- test[which(test$external.temperature>40),]

moveBankToKamadata(movebank.df = maxs,sqliteName = "Output/SQlite/maxes.sqlite")
#----------------------------------
#2. Points to polygons
#count up points in polygons
#take the mean of a value of points in polygons, and count the points too
#Note: on my desktop Windows 10 i7 (32 GB ram), it took just over 2 min
#for a point in polygon with 5 million points in 5000 polygons
#----------------------------------

set.seed(8675309)

num_poly <- 10
num_points <- 20

#assume that this spdf is a layer of admin boundaries
pts <- cbind(-110 + runif(num_poly),44 + runif(num_poly))

#this is a vector spatial data class/object in R
v1 <- voronoi(pts)#dismo
proj4string(v1) <- CRS("+proj=utm +zone=12 +datum=WGS84") #rgdal

#assume that this spdf is a layer of forest area
pts <- as.data.frame(cbind(-110 + runif(num_points),44 + runif(num_points)))

names(pts) <- c("x","y")
pts$v <- rnorm(num_points)
pts$cnt <- 1

#create a spatialpointsdataframe
coordinates(pts) <- ~x+y

#this is a vector spatial data class/object in R
proj4string(pts) <- CRS("+proj=utm +zone=12 +datum=WGS84") #rgdal

#this overlay returns the polygon index of points; returns data.frame with rows
#from polygon spdf corresponding to points
v1@data$v <- over(v1,pts,returnList = FALSE,fn=mean)[,1]
v1@data$cnt <- over(v1,pts,returnList = FALSE,fn=sum)[,2]

plot(v1,border="black")
text(gCentroid(v1,byid=TRUE),labels=v1@data$cnt,col="gray",cex=2)
points(pts,pch=20,col="red",cex=0.75)


