library(dplyr)
library(tictoc)
library("toolsForAtlas")
tic()
file.to.be.made <- read.csv("Data/2018&2019AllRaw.csv")
toc()
file.to.be.made <- file.to.be.made %>% # laeve only relevant individuals
  dplyr::select(tag.local.identifier,timestamp,location.long,location.lat) %>% 
  filter(!location.long==0)
file.to.be.made$timestamp<-ymd_hms(file.to.be.made$timestamp)
time_bin.mins <- 5
file.to.be.made$rounded_time<-roundToTimeFrame(timestamp=file.to.be.made$timestamp,
                                    TimeFrame.min=time_bin.mins)
file.to.be.made<-file.to.be.made %>% 
  mutate(hour.only = strftime(rounded_time,format = "%H:%M:%S" ,tz = "GMT"))%>% droplevels()
file.to.be.made$hour<-hour(file.to.be.made$rounded_time)
file.to.be.made$minute<-minute(file.to.be.made$rounded_time)
file.to.be.made$date.only <- date(file.to.be.made$rounded_time)



file.to.be.made18 <- subsetByTimeRangeM(file.to.be.made,
                                      start.date ="2018-10-21",
                                      end.date = "2019-02-22",
                                      start.hour =1,
                                      end.hour =23 )


file.to.be.made19 <- subsetByTimeRangeM(file.to.be.made,
                                        start.date ="2019-10-21",
                                        end.date = "2020-02-22",
                                        start.hour =1,
                                        end.hour =23 )
file.to.be.madeALL <- rbind(file.to.be.made18,file.to.be.made19)
  
moveBankToKamadata(file.to.be.madeALL,"Output/SQlite/2018&2019AllRaw.sqlite")
  