library(ARTool)
library(adehabitatLT)
library("toolsForAtlas")
library(maptools)
library(rgdal)
library(tictoc)
library(stringr)
library(lubridate)
library(ggplot2)
library(dplyr)
library(lmtest)
tic()
rawdata<-MaxDistanceFromRoost

relevant.inds <-c(161161, 170541, 170591, 170801,	170831, 170931, 170961, 171021,
                  1819501,	1819511, 1819521,	1819531, 1819541, 1819551, 1819561, 1819611)  #only inds that are that finished in 2019
did.not.use.feeding.station <- c(170951, 170991)
colnames(rawdata) <- c("tag.local.identifier","DO","timestamp","location.long","location.lat","is_out")
#filtering out artifact points (Longitude==0), dead individuals & keeping only Hula individuals
exluded.inds<-c(170581,161161,170961,did.not.use.feeding.station)#15021,16116, 17105, 181957,17096) # insufficiant data 
data<-rawdata %>% # laeve only relevant individuals
  dplyr::select(tag.local.identifier,timestamp,location.long,location.lat,is_out) %>% 
  filter(!location.long==0)%>% 
  filter(tag.local.identifier %in% relevant.inds) %>% 
  filter(!tag.local.identifier %in% exluded.inds) 
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
toc()

#---------------testing
i=j=di=1
# "2016-11-07", "2016-12-05",
# "2016-12-04", "2017-02-20",
start.dates <- c("2017-11-10", "2017-12-05","2018-10-20", "2018-12-05","2019-10-20", "2019-12-05")
end.dates <-  c("2017-12-04","2018-02-22","2018-12-04", "2019-02-22","2019-12-04","2020-02-22")

data <- convertSpatial.WGS842ITM(data,xyColNames = c("location.long","location.lat"))
data <- as.data.frame(data)
roosts.df <- c()
max.ds <- c()
for (j in 1:length(tag.local.identifiers)) {
ind.data <- data %>% 
  filter(tag.local.identifier==tag.local.identifiers[j])  
dates.temp <- unique(ind.data$date.only)
tic()
for (i in 1:length(dates.temp)) {
  roost.data <-subsetByTimeRangeM(data = ind.data,
                            start.date = dates.temp[i],
                            end.date = dates.temp[i+1],
                            start.hour =22,
                            end.hour = 23)
roosts.temp <- data.frame("Tag"=tag.local.identifiers[j])
roosts.temp$X <- median(roost.data$location.long)
roosts.temp$Y<- median(roost.data$location.lat)
# roosts.temp$Date <- dates.temp[i]
# roosts.df <- rbind(roosts.df,roosts.temp)

day.df <-subsetByTimeRangeM(data = ind.data,
                            start.date = dates.temp[i+1],
                            end.date = dates.temp[i+2],
                            start.hour =4,
                            end.hour = 16)

ind.ds <-c()
for (di in 1:nrow(day.df)) {
  ind.ds[di] <-  d(roosts.temp$X,roosts.temp$Y,day.df$location.long[di],day.df$location.lat[di])
    }
temp.max.ds <- data.frame("Tag"=tag.local.identifiers[j])
temp.max.ds$max.ds <- max(ind.ds)
temp.max.ds$date <- dates.temp[i]
max.ds <- rbind(max.ds,temp.max.ds)
}
toc()
               
}

max.ds <- max.ds %>% 
  filter(!is.na(max.ds))
max.ds$period[max.ds$date<"2018-12-05"] <- "1 2018 Pre"
max.ds$period[max.ds$date>="2018-12-05"& max.ds$date<"2019-02-22"] <- "2 2018 Post"
max.ds$period[max.ds$date>="2019-02-22"& max.ds$date<="2019-12-05"] <- "3 2019 Pre"
max.ds$period[max.ds$date>"2019-12-05"] <- "4 2019 Post"


max.ds$period <- as.factor(max.ds$period)
max.ds$Tag <- as.factor(max.ds$Tag)

max.ds.summary <- max.ds %>% 
  group_by(period)%>%
  summarise("means"=mean(max.ds))

boxplot(max.ds$max.ds~max.ds$period, ylab = "", xlab = "")#,ylim=c(0,11000))
title(ylab="Mean individual dayily maximal displacement (m)", line=2.5, cex.lab=1.2)
title(xlab=expression("Period"), line=3, cex.lab=1.2)
points(max.ds.summary$means,pch=8,col="red",cex=1.5)


kruskal.test(max.ds$max.ds~max.ds$period)
pairwise.wilcox.test(max.ds$max.ds, g = max.ds$period)

lmMod <- lm(max.ds ~ period, data=max.ds)
par(mfrow=c(2,2)) # init 4 charts in 1 panel
plot(lmMod)
par(mfrow=c(1,1))
lmtest::bptest(lmMod)
#the test has a p-value less than 0.05, therefore we can reject the null hypothesis that the variance of the residuals is constant and infer that heteroscedasticity is indeed present


m <- art(max.ds ~ period + (1 | Tag), data=max.ds)
summary(m)
anova(m)

p <- ggplot(max.ds, aes(x=max.ds,fill=period)) + 
  geom_histogram(binwidth=150)
p
 