library(adehabitatLT)
library("toolsForAtlas")
library(maptools)
library(rgdal)
library(tictoc)
library(stringr)
library(lubridate)
library(ggplot2)
library(dplyr)

final.res.df <-c()
# data <- removedups(data)
years.periods<-data.frame(start.date=c("2018-10-21","2018-12-05", "2019-10-21","2019-12-05"),
                          end.date=c("2018-12-04","2019-02-22","2019-12-04", "2020-02-22"))
for (B in 1:length(years.periods$start.date)) {
  


  
  locs <- convertSpatial.WGS842ITM(data,xyColNames = c("location.long","location.lat"))
  locs <- as.data.frame(locs)

  #-----max daily distance per crane 
  locs <- subsetByTimeRangeM(locs,start.date = years.periods$start.date[B], years.periods$end.date[B],start.hour = 4,end.hour = 16)
  da <- as.character(locs$timestamp)
  da <- as.POSIXct(strptime(as.character(da),"%Y-%m-%d %H:%M:%S" ))
  hula <- as.ltraj(xy = locs[,c("location.long","location.lat")], date = da, id = locs$tag.local.identifier)
  # cut bursts sub-burst (Burst=day)
  # any two locations more than 10 hr apart ar considered as two different bursts.
  foo <- function(dt) {
    return(dt> (3600*10))
  }
  hula2 <- cutltraj(hula, "foo(dt)", nextr = TRUE)
  toc()
  inds <- unique(id(hula2))
  mean.of.maxes <- c()
  for (j in 1:length(inds)) {
    maxs <- c()
    ind <- hula2[id=inds[j]]
    for (i in 1:length(ind)) {
      maxs[i]<- max(ind[[1]]$dist,na.rm = TRUE)
    }
    mean.of.maxes[j] <- mean(maxs)
    
    print(sprintf("Ind %s interval %d ",
                  id(ind[i]), j))
  }

 
   res.df <-  as.data.frame(cbind(mean.of.maxes,Date=rep(years.periods$start.date[B],length(mean.of.maxes))))

  final.res.df <- rbind(final.res.df,res.df)
}
final.res.df$mean.of.maxes <- as.numeric(final.res.df$mean.of.maxes)
final.res.df.summary <- final.res.df %>% 
  group_by(Date)%>%
  summarise("mean.of.maxes"=mean(mean.of.maxes))
  

boxplot(final.res.df$mean.of.maxes~final.res.df$Date, ylab = "", xlab = "")#,ylim=c(0,25))
title(ylab="Mean individual dayily maximal step size(m)", line=2.5, cex.lab=1.2)
title(xlab=expression("Period"), line=3, cex.lab=1.2)
points(final.res.df.summary,pch=8,col="red",cex=1.5)

final.res.df$Date <- as.factor(final.res.df$Date)
kruskal.test(final.res.df$mean.of.maxes~final.res.df$Date)
pairwise.wilcox.test(final.res.df$mean.of.maxes, g = final.res.df$Date)
