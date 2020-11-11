# https://stackoverflow.com/questions/41683905/grid-too-small-for-kernelud-getverticeshr-adehabitathr-home-range-estimation
library("toolsForAtlas")
library(maptools)
library(rgdal)
library(tictoc)
library(stringr)
library(lubridate)
library(ggplot2)
library(dplyr)

WinteringAreas <- read.csv("Data/Wintering_areas.csv")
did.not.use.feeding.station <- c()#170951, 170991)
not.israel.winterers <- c(16113,16114, 170511,170861,170881,17079,
                          170521, 170581, 170761, 170791,170811, 170821,
                         170891, 170911, 171031, 171041,171001, 4684)
exluded.inds.general<-c(170581,170961,17058,17096,did.not.use.feeding.station)#15021,16116, 17105, 181957,17096) # insufficiant data 

rawdata16 <- read.csv("Data/InOutOfRoost2016-17.csv")

rawdata17 <- read.csv("Data/2017raw.csv")
HulaWintrers17 <- WinteringAreas %>% 
  filter(year==2017) %>% 
 dplyr:: select(indevidual)
HulaWintrers17 <- HulaWintrers17$indevidual
exluded.inds17 <- c(exluded.inds.general)
data17 <- rawdata17%>%
  filter(individual %in% HulaWintrers17)%>% 
  filter(!individual %in% exluded.inds17)

rawdata18 <- read.csv("Data/2018raw.csv")
HulaWintrers18 <- WinteringAreas %>% 
  filter(year==2018) %>% 
  dplyr::select(indevidual)
HulaWintrers18 <- HulaWintrers18$indevidual
exluded.inds18 <- c(exluded.inds.general)
data18 <- rawdata18%>%
  filter(individual %in% HulaWintrers18)%>% 
  filter(!individual %in% exluded.inds18)

rawdata19 <- read.csv("Data/2019raw.csv")
HulaWintrers19 <- WinteringAreas %>% 
  filter(year==2019) %>% 
  dplyr:: select(indevidual)
HulaWintrers19 <- HulaWintrers19$indevidual
exluded.inds19 <- c(exluded.inds.general)
data19 <- rawdata19%>%
  filter(individual %in% HulaWintrers19)%>% 
  filter(!individual %in% exluded.inds19)



#relevant for 2017-2019
# relevant.inds <-c(161161, 170541, 170591, 170801,	170831, 170931, 170961, 171021,
#                   1819501,	1819511, 1819521,	1819531, 1819541, 1819551, 1819561, 1819611,16116, 17054,
#                   17059, 17080,	17083, 17093, 17096, 17102, 181950,	181951,
#                   181952,	181953, 181954, 181955, 181956, 181961)  #only inds that are that finished in 2019


rawdata <- rbind(data17,data18,data19)
moveBankToKamadata(movebank.df = RoostData,sqliteName = "Output/SQlite/Raw.sqlite")
MaxDistanceFromRoost <- rawdata
colnames(rawdata) <- c("tag.local.identifier","DO","timestamp","location.long","location.lat","is_out")
#filtering out artifact points (Longitude==0), dead individuals & keeping only Hula individuals
data<-rawdata %>% # laeve only relevant individuals
  dplyr::select(tag.local.identifier,timestamp,location.long,location.lat) %>% #,is_out) %>% 
  filter(!location.long==0)
# %>% 
#   filter(tag.local.identifier %in% relevant.inds) %>% 
#   filter(!tag.local.identifier %in% exluded.inds)

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
#data <-  data  %>% 
#filter(is_out==1)

# removedups(maxs)
moveBankToKamadata(movebank.df = RoostData,sqliteName = "Output/SQlite/In.out17-20.sqlite")


# summary.subsetted.df<-data%>%
#   group_by(tag.local.identifier, date.only)%>%
#   summarise("loc.nums"=n())
# barplot(summary.subsetted.df$loc.nums~summary.subsetted.df$tag.local.identifier+summary.subsetted.df$date.only)



toc()



