
library(dplyr)
library(lubridate)

roundToTimeFrame<-function(timestamp,              
                           TimeFrame.min=60){
  num.dateTime<-as.numeric(timestamp)
  rounded.dateTime<-round(num.dateTime/(60*TimeFrame.min))*(TimeFrame.min*60)
  rtrn.dateTime<-as.POSIXct(rounded.dateTime, tz="UTC",origin="1970-01-01")
  return(rtrn.dateTime)
}


#---------------------my subsetByTimeRange-----------------

subsetByTimeRangeM<-function(data,
                             start.date = "2019-12-01",
                             end.date = "2020-02-01",
                             start.hour = 1,
                             end.hour = 23){
  ###subseting by date Range
  epoch.start.date<-as.POSIXct(start.date, tz="UTC")
  epoch.end.date<-as.POSIXct(end.date, tz="UTC")
  data.subset<-data %>%
    filter(rounded_time >= epoch.start.date & rounded_time < epoch.end.date) 
  
  data.subset <- data.subset %>% 
    filter(between(hour, start.hour, end.hour))
  return(data.subset)
}


#-----------------------Daily\weekly HR cor per ind--------------

# the function assumes that the data contains only relevant 
# individuals (including a sufficient amount of fixes) and hours (when inds are out of the roost)

ind.period.corHR.fun<-function(data, 
                               ind,
                               start.date = "2019-12-01",
                               end.date = "2020-02-01")#,
  # start.hour = 1,
  # end.hour = 23)
{
  ###subseting by date Range
  
  epoch.start.date<-as.POSIXct(start.date, tz="UTC")
  epoch.end.date<-as.POSIXct(end.date, tz="UTC")
  data.subset<-data %>%
    filter(tag.local.identifier == ind )%>%
    filter(rounded_time >= epoch.start.date & rounded_time < epoch.end.date)#%>% 
  #filter(between(hour, start.hour, end.hour))
  return(data.subset)
}


#--------------moveBank To Kamadata-------------------

moveBankToKamadata<-function(movebank.df,
                             sqliteName){
  atlas.df<-convert.GPS2atlas.df(movebank.df,
                                 gps.X = "location.long",
                                 gps.Y = "location.lat",
                                 gps.TIME = "timestamp",
                                 tryTimeFormats = "%Y-%m-%d %H:%M:%S")
  atlas.df$TAG<-movebank.df$tag.local.identifier
  # --- workaround to duplications problems -----
  dup.ix<-which(duplicated(atlas.df[,c("TAG","TIME")]))
  print(sprintf("moveBankToKamadata(): found %d duplications of TAG & TIME",
                length(dup.ix)))
  atlas.df<-atlas.df[-dup.ix,]
  # -------------------------------------------------
  
  exportForKamadata(sqliteName = sqliteName,
                    loc.df = atlas.df,
                    overwrite = TRUE)  
}

#-----------------Subsetting to get a simillar numbers of location\individual

loc.num.subsetting <- function(data,
                               minimal.dayly.th=35){
  #  only days with enough loclizations 
  summary.df<-data%>%
    group_by(tag.local.identifier,date.only)%>%
    summarise("loc.nums"=n()) %>%
    filter(loc.nums>=minimal.dayly.th)
  min.loc.num <- min(summary.df$loc.nums)
  summary.df<-summary.df %>% mutate("interval"=floor(loc.nums/min.loc.num))
  
  subset.daily.loc=c()
  for (i in 1:nrow(summary.df)) {
    id.subset.ix<-seq(from=1, 
                      to=summary.df$loc.nums[i],
                      by=summary.df$interval[i])
    daily.loc<-data%>%
      filter((date.only==summary.df$date.only[i]) & (tag.local.identifier==summary.df$tag.local.identifier[i]))%>%
      arrange(timestamp)  
    
    subset.daily.loc<-rbind(subset.daily.loc, daily.loc[id.subset.ix,])
  }
  return(subset.daily.loc)
}

#----------------Periodic HR summary

periodicHRsummary <- function(data, 
                              start.date,
                              end.date,
                              date.interval=8,
                              percent=50){
  YEAR<-year(start.date)
  # start.dates <- seq(as.Date(start.date),as.Date(end.date),by = date.interval)
  #end.dates <- seq(as.Date(start.date)+(date.interval-1),as.Date(end.date)+(date.interval-1),by = date.interval)
  start.dates <- c("2018-10-20", "2018-12-05","2019-10-20", "2019-12-05")
  end.dates <-  c("2018-12-04", "2019-02-22","2019-12-04","2020-02-22")
  HR.summary<-c()
  ud <- list()
  HRZs <-list()
  for (i in 1:(length(start.dates))) {
    #for (i in 9:(length(start.dates)-2)) {
    tic()
    HR.subset <- subsetByTimeRangeM(data = data,
                                    start.date = start.dates[i],
                                    end.date = end.dates[i])
    
    ITM.HR.Sub<-convertSpatial.WGS842ITM(HR.subset,xyColNames = c("location.long","location.lat"))
    print(sprintf("interval %d",
                  i))
    # print(sprintf("year %d interval %d ",
    #               YEAR, i))
    
    kud <- kernelUD(ITM.HR.Sub[,1],grid = 200, same4all = FALSE)
    # investigateKernel(kud, 400)
    ud <- getverticeshr(x = kud,percent=percent,unout="km2")
    HRZ <- as.data.frame(kernel.area(x = kud,percent=percent,unout="km2"))
    
    tmp.df<-data.frame("average"=rowMeans(HRZ),
                       "sum"=rowSums(HRZ),
                       "sd"=sd(HRZ))
    
    writePolyShape(ud, paste0("Output/SHP/UD",start.dates[i]))
    tmp.df$interval.id<-i 
    tmp.df$interval.start<-start.dates[i]
    tmp.df$interval.end<-end.dates[i]
    tmp.df$YEAR <- YEAR
    HR.summary<-rbind(HR.summary, tmp.df)
    HRZs[i]<- list(HRZ)
    toc()
  }
  #return(HR.summary)
  return(HRZs)
}

#------investigate Kernel bugs

investigateKernel<-function(kud_points, xyGrid){
  
  # 6. Kernel Density
  #kud_points <- kernelUD(points, grid = xyGrid)
  image(kud_points)
  
  # 7. Get the Volum
  vud_points <- getvolumeUD(kud_points)
  
  # 8. Get contour
  levels <- c(50)
  list <- vector(mode="list", length = 2)
  
  list[[1]] <- as.image.SpatialGridDataFrame(vud_points[[1]])
  list[[2]] <- as.image.SpatialGridDataFrame(vud_points[[2]])
  
  # 9. Plot
  par(mfrow = c(2, 1))
  image(vud_points[[1]])
  contour(list[[1]], add=TRUE, levels=levels)
  image(vud_points[[2]])
  contour(list[[2]], add=TRUE, levels=levels)
}