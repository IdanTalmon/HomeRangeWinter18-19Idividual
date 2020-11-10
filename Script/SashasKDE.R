# Load libraries
library(adehabitatHR)
library(rgdal)
library(sp)
require(recurse)
require(scales)
library(leaflet) # required for interactive map display
library(rgeos)
library(mapview)
library(webshot)
library(ggplot2)
library(rhr)
library(plyr)
library(maptools)
# ------The edges of the grid for kernel -------------------------------------------------------------
yBound <- c(762000,810000)
xBound <- c(250000,270000)
#-- now create the grid
# The method to do it taken from here:
# https://stackoverflow.com/questions/41683905/grid-too-small-for-kernelud-getverticeshr-adehabitathr-home-range-estimation

x <- seq(xBound[1], xBound[2], by=100) # resolution is the pixel size you desire
y <- seq(yBound[1], yBound[2], by=100)
xy <- expand.grid(x=x,y=y)
coordinates(xy) <- ~x+y
gridded(xy) <- TRUE
class(xy)
# projections
itm <- "+init=epsg:2039 +proj=tmerc +lat_0=31.73439361111111 +lon_0=35.20451694444445 +k=1.0000067 +x_0=219529.584 +y_0=626907.39 +ellps=GRS80 +towgs84=-48,55,52,0,0,0,0 +units=m +no_defs"
wgs84<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

# load data
CraneData<-read.csv("KernelDataInBeforeMiddleAfterFinal.csv")
IDLst <- unique(CraneData$ID)

ur95 <- list()
ur75 <- list()
ur50 <- list()

GridSize<-as.data.frame(matrix(data=NA,nrow=length(IDLst),ncol=4))
colnames(GridSize)<-c("Tag","XGrid","YGrid","Area")

iso <- as.data.frame(matrix(data=NA,nrow=length(IDLst),ncol=9))
colnames(iso)<-c("Tag","Date","iso95","iso75","iso50",'Split','Migstatus',"NumberPoints","h")
iso$Date<-as.POSIXct(iso$Date)
iso$Split<-as.character(iso$Split)
for (i in 1:length(IDLst))
{
  tmp<-subset(CraneData,CraneData$ID==IDLst[i])
  
  # transform to date format
  tmp$DateTime_pox<- as.POSIXct(tmp$Date_Time,tz="","%d-%m-%Y %H:%M:%S")
  tmp <- tmp[diff.Date(tmp$DateTime_pox)!=0,]
  CraneData_ltraj<-as.ltraj(cbind(tmp$Local_Lon, tmp$Local_Lat), tmp$DateTime_pox, as.character(tmp$ID),
                            typeII = TRUE,proj4string = CRS(itm))
  df1 = data.frame(tmp$ID)
  locs1<-SpatialPointsDataFrame(cbind(tmp$Local_Lon, tmp$Local_Lat), df1, proj4string = CRS(itm))
  
  locs<-SpatialPoints(cbind(tmp$Local_Lon, tmp$Local_Lat), proj4string = CRS(itm))
  
  #reference bandwidth
  Q<-0.5*(sd(locs@coords[,1])+sd(locs@coords[,2]))
  h<- Q*(length(locs@coords[,1]))^(-1/6)
  
  #-- check for banwidth using rhr package----
  # (1) ----Reference benwudth (Silverman's rule of thumb (Srt) KDE ocording to Noonan et. al 2018,Ecological Monographs)
  tempLonLat <- tmp[,6:7]
  h <- rhrHref(tempLonLat, rescale = "none")
  #rhrHlscv
  
  # Run the kernelUD
  ud <- kernelUD(locs, h=h[1]*0.7, grid=xy)
  
  #NewLst[[i]] <- IDLst[i]
  r1 <- try(getverticeshr(ud, 95))
  r2 <- try(getverticeshr(ud, 75))
  r3 <- try(getverticeshr(ud, 50))
  
  ur95[[i]] <- r1
  ur75[[i]] <- r2
  ur50[[i]]<- r3
  iso$Tag[i] <- as.character(IDLst[i])
  iso$Date[i] <-  as.POSIXct(tmp$Date[1],tz="","%d-%m-%Y")
  
  iso$iso95[i]<-ur95[[i]]@data$area
  iso$iso75[i]<-ur75[[i]]@data$area
  iso$iso50[i]<-ur50[[i]]@data$area
  iso$Split[i]<- as.character(tmp$Split[1])
  iso$Migstatus[i]<- as.character(tmp$MigrationSatatus[1])
  iso$NumberPoints[i]<-nrow(tempLonLat)
  iso$h[i]<- h[1]
}

# transform:
urll50<-spTransform(ur50[[Ind]], wgs84)
