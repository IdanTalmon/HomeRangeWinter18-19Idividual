# https://stackoverflow.com/questions/41683905/grid-too-small-for-kernelud-getverticeshr-adehabitathr-home-range-estimation
library("toolsForAtlas")
library(maptools)
library(rgdal)
library(tictoc)
library(stringr)
library(lubridate)
library(ggplot2)
library(dplyr)

tic()
relevant.inds <-c(161161, 170541, 170591, 170801,	170831, 170931, 170961, 171021,
                  1819501,	1819511, 1819521,	1819531, 1819541, 1819551, 1819561, 1819611,16116, 17054,
                  17059, 17080,	17083, 17093, 17096, 17102, 181950,	181951,
                  181952,	181953, 181954, 181955, 181956, 181961)  #only inds that are that finished in 2019
did.not.use.feeding.station <- c()#170951, 170991)
exluded.inds<-c(170581,170961,17058,17096,did.not.use.feeding.station)#15021,16116, 17105, 181957,17096) # insufficiant data 

# rawdata16 <- read.csv("Data/2016AllRaw.csv")%>% 
#   dplyr::select(tag.local.identifier,timestamp,location.long,location.lat) %>% 
#   filter(!location.lat=="NA")
#   
# rawdata17 <- read.csv("Data/2017AllRaw.csv")%>% 
#   dplyr::select(tag.local.identifier,timestamp,location.long,location.lat)%>% 
#   filter(!location.lat=="NA")
#   
# rawdata16.17 <- rbind(rawdata16,rawdata17)
# rawdata16.17 <- rawdata16.17 %>% 
#   filter(!location.long==0)%>%
#   filter(tag.local.identifier %in% relevant.inds)%>% 
#   mutate(is_out = NA) %>% 
#   filter(!tag.local.identifier %in% exluded.inds)
#   
# rawdata<-read.csv("Data/Data4Idan.csv")

rawdata17.19<-read.csv("Data/InOutOfRoost2017-19.csv")
rawdata19.20<-read.csv("Data/InOutOfRoost2019-20.csv")
rawdata <- rbind(rawdata17.19,rawdata19.20)
MaxDistanceFromRoost <- rawdata
colnames(rawdata) <- c("tag.local.identifier","DO","timestamp","location.long","location.lat","is_out")
#filtering out artifact points (Longitude==0), dead individuals & keeping only Hula individuals
data<-rawdata %>% # laeve only relevant individuals
  dplyr::select(tag.local.identifier,timestamp,location.long,location.lat,is_out) %>% 
  filter(!location.long==0)%>% 
  filter(tag.local.identifier %in% relevant.inds) %>% 
  filter(!tag.local.identifier %in% exluded.inds)

#data$tag.local.identifier[data$tag.local.identifier==16116] <- 161161

data <- removedups(data)

data$date <- str_split_fixed(data$timestamp, " ",2)

data<-as_tibble(data)
tag.local.identifiers<-unique(data$tag.local.identifier)

#Pre sbusetting location numbers for each individual
summary.df<-data%>%
  group_by(tag.local.identifier)%>%
  summarise("loc.nums"=n())

barplot(summary.df$loc.nums~summary.df$tag.local.identifier)

# rounding times to help later filtration\subsampling
data$timestamp<-ymd_hms(data$timestamp)
time_bin.mins <- 5

data$rounded_time<-roundToTimeFrame(timestamp=data$timestamp,
                                    TimeFrame.min=time_bin.mins)

data<-data %>% 
  mutate(hour.only = strftime(rounded_time,format = "%H:%M:%S" ,tz = "GMT"))%>% droplevels()
data$hour<-hour(data$rounded_time)
data$minute<-minute(data$rounded_time)
data$date.only <- date(data$rounded_time)

#data <- loc.num.subsetting(data,minimal.dayly.th = 10)
RoostData <- data
moveBankToKamadata(movebank.df = RoostData,sqliteName = "Output/SQlite/In.out17-20.sqlite")
data <-  data  %>% 
filter(is_out==1)

# removedups(maxs)
moveBankToKamadata(movebank.df = RoostData,sqliteName = "Output/SQlite/In.out17-20.sqlite")


# summary.subsetted.df<-data%>%
#   group_by(tag.local.identifier, date.only)%>%
#   summarise("loc.nums"=n())
# barplot(summary.subsetted.df$loc.nums~summary.subsetted.df$tag.local.identifier+summary.subsetted.df$date.only)



toc()



