#install.packages("ctmm")
# install.packages("rgeos")
# install.packages("dismo")
# install.packages("tmaptools")
# install.packages("tmap")tmaptools
# install.packages("")
#install.packages("openxlsx")
library(ctmm)
library(dplyr)
library(dismo)
library("rgeos")
library(tmaptools)
library(tmap)
library(sf)
library(openxlsx)
start.dates <- c("2016-11-07", "2016-12-05","2017-11-10", "2017-12-05","2018-10-20", "2018-12-05","2019-10-20", "2019-12-05")
end.dates <-  c("2016-12-04", "2017-02-20","2017-12-04","2018-02-22","2018-12-04", "2019-02-22","2019-12-04","2020-02-22")



moveBankToKamadata(movebank.df = RoostData, "Output/SQlite/RoostData.sqlite")
RoostData <- convertSpatial.WGS842ITM(RoostData,xyColNames = c("location.long","location.lat"))
RoostData <- as.data.frame(RoostData)
RoostData <- subsetByTimeRangeM(RoostData,
                                start.date ="2016-10-20",
                                end.date = "2020-02-22",
                                start.hour = 22,
                                end.hour = 23)



RoostingSites = st_read("Data/RoostingSites.shp") 

qtm(RoostingSites)

 ITM.CRS <- st_crs(RoostingSites)


#------------------fit this loop to needs
roosts.df <- c()
tic()
for (j in 1:length(tag.local.identifiers)) {
  ind.data <- RoostData %>%
    filter(tag.local.identifier==tag.local.identifiers[j])
  dates.temp <- unique(ind.data$date.only)
    for (i in 1:length(dates.temp)) {
    roost.data <-subsetByTimeRangeM(data = ind.data,
                                    start.date = dates.temp[i],
                                    end.date = dates.temp[i+1],
                                    start.hour =22,
                                    end.hour = 23)
    roosts.temp <- data.frame("Tag"=tag.local.identifiers[j])
    roosts.temp$X <- median(roost.data$location.long)
    roosts.temp$Y<- median(roost.data$location.lat)
    roosts.temp$Date <- dates.temp[i]
    roosts.df <- rbind(roosts.df,roosts.temp)
  }
}
toc()

roosts.df <- subset(x = roosts.df,!is.na(roosts.df$X))
point.geo <- st_as_sf(roosts.df,
                      coords = c(x="X", y= "Y"),
                      crs= ITM.CRS)
res <- st_join(point.geo,RoostingSites,join = st_within)
res$ProjectNam[is.na(res$ProjectNam)] <- "Other"



# priod.names <- c("1 2016/17 Pre","2 2016/17 Post","3 2017/18 Pre",
#                  "4 2017/18 Post","5 2018/19 Pre","6 2018/19 Post",
#                  "7 2019/20 Pre","8 2019/20 Post")
priod.names <- c("H 2016/17 Pre","G 2016/17 Post","F 2017/18 Pre",
                 "E 2017/18 Post","D 2018/19 Pre","C 2018/19 Post",
                 "B 2019/20 Pre","A 2019/20 Post")


res$period[res$Date<end.dates[1]] <- priod.names[1]

res$period[res$Date>=start.dates[2]& res$Date<=end.dates[2]] <- priod.names[2]
res$period[res$Date>=start.dates[3]& res$Date<=end.dates[3]] <- priod.names[3]
res$period[res$Date>=start.dates[4]& res$Date<=end.dates[4]] <- priod.names[4]
res$period[res$Date>=start.dates[5]& res$Date<=end.dates[5]] <- priod.names[5]
res$period[res$Date>=start.dates[6]& res$Date<=end.dates[6]] <- priod.names[6]
res$period[res$Date>=start.dates[7]& res$Date<=end.dates[7]] <- priod.names[7]
res$period[res$Date>=start.dates[8]] <- priod.names[8]
res <- subset(x = res,!is.na(res$period))

tm_shape(RoostingSites)+
  tm_fill()+
  tm_shape(res)+
  tm_bubbles(col="red",size = 0.3)

h <- ggplot(res, aes(x=ProjectNam,fill=period)) + 
    geom_histogram(stat="count")
h
+
  scale_y_discrete(limits=c("2019/20 Post", "2019/20 Pre",
                            "2018/19 Post", "2018/19 Pre", "2017/18 Post",
                            "2017/18 Pre", "2016/17 Post","2016/17 Pre"))
Roost.summary <- res %>% 
  group_by(ProjectNam,period) %>% 
  summarise("count"=n())

Roost.summary <- Roost.summary %>% 
  mutate(Loc.Period = paste(ProjectNam,period))
Roost.summary <- as.data.frame(Roost.summary)
Roost.summary <- Roost.summary %>% 
 dplyr::select(-geometry)
write.csv2(Roost.summary,)
write.xlsx(x = Roost.summary,file = "Roost.summary",asTable = TRUE,".xlsx")
ggplot(Roost.summary, aes(x=Loc.Period,y="", fill=period)) +
  geom_bar(stat="identity", width=1)+
  coord_polar("x", start = 0) 

Roost.summary.p <- c()
for (i in 1:length(priod.names)) {
  temp.p<- Roost.summary %>%
  filter(period==priod.names[i])
  temp.p<-temp.p %>% 
  mutate(percentage=100*(count/sum(Roost.summary.p$count)))
  Roost.summary.p <- rbind(Roost.summary.p,temp.p)
  
}

ggplot(Roost.summary.p, aes(x=ProjectNam, y=count,fill=period)) +
  geom_bar(stat="identity", width=1)+
  coord_polar("x", start = 0) 


