# https://stackoverflow.com/questions/41683905/grid-too-small-for-kernelud-getverticeshr-adehabitathr-home-range-estimation
library(adehabitatHR)
library("toolsForAtlas")
library(maptools)
library(rgdal)
library(tictoc)
library(stringr)
library(lubridate)
library(ggplot2)
library(dplyr)
#install.packages("ARTool")
#install.packages("emmeans")
library(emmeans)
library(ARTool)


#---------------------Ind tests 2018-2019 HR sizes for boxplots
#data <- subset(data, -(tag.local.identifier == 161161 && timestamp >= "2017-10-19" &&  timestamp <= "2017-12-8"))
start.dates <- c("2016-11-07", "2016-12-05","2017-11-10", "2017-12-05","2018-10-20", "2018-12-05","2019-10-20", "2019-12-05")
end.dates <-  c("2016-12-04", "2017-02-20","2017-12-04","2018-02-22","2018-12-04", "2019-02-22","2019-12-04","2020-02-22")
# HR.summary<-periodicHRsize(data, 
#                               start.dates = start.dates,
#                               end.dates=end.dates,
#                               percent=50)

# HR.summary16<-periodicHRsize(data, 
#                              start.dates = start.dates[1:2],
#                              end.dates=end.dates[1:2],
#                              percent=50)
HR.summary17<-periodicHRsize(data, 
                             start.dates = start.dates[3:4],
                             end.dates=end.dates[3:4],
                             percent=50)
HR.summary18<-periodicHRsize(data, 
                             start.dates = start.dates[5:6],
                             end.dates=end.dates[5:6],
                             percent=50)
HR.summary19<-periodicHRsize(data, 
                             start.dates = start.dates[7:8],
                             end.dates=end.dates[7:8],
                             percent=50)

#Pre.2016 <- as.data.frame(cbind(colnames(HR.summary16[[1]]),as.numeric(HR.summary16[[1]]),rep("1 2016/17 Pre",length(HR.summary16[[1]][1,]))))
#post.2016 <- as.data.frame(cbind(colnames(HR.summary16[[2]]),as.numeric(HR.summary16[[2]]),rep("2 2016/17 Post",length(HR.summary16[[2]][1,]))))
Pre.2017 <- as.data.frame(cbind(colnames(HR.summary17[[1]]),as.numeric(HR.summary17[[1]]),rep("3 2017/18 Pre",length(HR.summary17[[1]][1,]))))
post.2017 <- as.data.frame(cbind(colnames(HR.summary17[[2]]),as.numeric(HR.summary17[[2]]),rep("4 2017/18 Post",length(HR.summary17[[2]][1,]))))
Pre.2018 <- as.data.frame(cbind(colnames(HR.summary18[[1]]),as.numeric(HR.summary18[[1]]),rep("5 2018/19 Pre",length(HR.summary18[[1]][1,]))))
post.2018 <- as.data.frame(cbind(colnames(HR.summary18[[2]]),as.numeric(HR.summary18[[2]]),rep("6 2018/19 Post",length(HR.summary18[[2]][1,]))))
Pre.2019 <- as.data.frame(cbind(colnames(HR.summary19[[1]]),as.numeric(HR.summary19[[1]]),rep("7 2019/20 Pre",length(HR.summary19[[1]][1,]))))
post.2019 <- as.data.frame(cbind(colnames(HR.summary19[[2]]),as.numeric(HR.summary19[[2]]),rep("8 2019/20 Post",length(HR.summary19[[2]][1,]))))
#Pre.2016,post.2016
HR.summary.df<- rbind(Pre.2017, post.2017,Pre.2018,post.2018,Pre.2019,post.2019)
colnames(HR.summary.df) <- c("IND","HR.size", "Period")
HR.summary.df$HR.size <- as.numeric(HR.summary.df$HR.size)
average.HRs <- HR.summary.df %>% 
  group_by(Period)%>%
  summarise("average"=mean(HR.size))

var.HRs <- HR.summary.df %>% 
  group_by(Period)%>%
  summarise("var"=var(HR.size))

HR.summary.df$Period <- as.factor(HR.summary.df$Period)
boxplot(HR.summary.df$HR.size~HR.summary.df$Period, ylab = "", xlab = "")#,ylim=c(0,25))
title(ylab=expression("Mean HR core area " ~ (km^{2})), line=2, cex.lab=1.2)
title(xlab=expression("Period"), line=3, cex.lab=1.2)
points(average.HRs$average,pch=8,col="red",cex=1.5)


# Change point shapes by the levels of cyl
for (i in 1:length(Hula.local.identifiers)) {
  ind.data <- HR.summary.df %>% 
    filter(IND==paste0("X",Hula.local.identifiers[i]))
P<-   ggplot(ind.data, aes(x=Period, y=HR.size, group=1)) +
  labs(title =Hula.local.identifiers[i] )+ylim(0,90)+
    geom_line()+
  geom_point()
print(P)
}



HR.summary.df<- rbind(Pre.2016,post.2016,Pre.2017,post.2017,Pre.2018,post.2018,Pre.2019,post.2019)
colnames(HR.summary.df) <- c("IND","HR.size", "Period")
HR.summary.df$HR.size <- as.numeric(HR.summary.df$HR.size)
HR.summary.df$Period <- as.factor(HR.summary.df$Period)
average.HRs <- HR.summary.df %>% 
  group_by(Period)%>%
  summarise("average"=mean(HR.size))

var.HRs <- HR.summary.df %>% 
    group_by(Period)%>%
    summarise("var"=var(HR.size))

boxplot(HR.summary.df$HR.size~HR.summary.df$Period, ylab = "", xlab = "")#,ylim=c(0,25))
title(ylab=expression("Mean HR core area " ~ (km^{2})), line=2, cex.lab=1.2)
title(xlab=expression("Period"), line=3, cex.lab=1.2)
points(average.HRs$average,pch=8,col="red",cex=1.5)


m <- art(HR.size ~ Period + (1 | IND), data=HR.summary.df)
summary(m)
anova(m)

model.lm = artlm(m, "Period")

marginal = emmeans(model.lm,
                   ~ Period)

pairs(marginal,
      adjust = "tukey")



  
DFSHT <- HR.summary.df$HR.size[HR.summary.df$Period ==
levels(HR.summary.df$Period)[4]]
  shapiro.test(DFSHT)


boxplot(HR.summary.df$HR.size~HR.summary.df$Period, ylab = "", xlab = "")#,ylim=c(0,25))
title(ylab=expression("Mean HR core area " ~ (km^{2})), line=2, cex.lab=1.2)
title(xlab=expression("Period"), line=3, cex.lab=1.2)
points(average.HRs$average,pch=8,col="red",cex=1.5)

kruskal.test(HR.summary.df$HR.size~HR.summary.df$Period)
pairwise.wilcox.test(HR.summary.df$HR.size,HR.summary.df$Period)


p <- ggplot(HR.summary.df, aes(x=HR.size, fill=Period)) + 
  geom_histogram(binwidth=0.5)
p


#---------------------Old drafts-------------
# #---------------------Ind tests 2018-2019
# years.periods<-data.frame(start.date=c("2018-10-21", "2019-10-21"),
#                           end.date=c("2019-02-22", "2020-02-22"))
# date.interval <- 2
# HR.yearly.summary<-c()
# tic()
# for (i in 1:nrow(years.periods)){
#   start.date <-years.periods$start.date[i]
#   end.date <- years.periods$end.date[i]
#   HR.summary<-periodicHRsummary(data, 
#                     start.date=start.date,
#                     end.date=end.date,
#                     date.interval=date.interval,
#                     percent=50)
#   HR.yearly.summary<-rbind(HR.yearly.summary, HR.summary)
#   
# }

# HR18 <- HR.yearly.summary %>% 
#   filter(YEAR==2018)
# HR19 <- HR.yearly.summary %>% 
#   filter(YEAR==2019)
# 
# #---- Plotting
# p1 <- ggplot(data =HR19,aes(x=interval.id))+
#   geom_point(aes(y=HR18$sum,color= "Winter 2018-19"),size=2.7)+
#   geom_point(aes(y=HR19$sum,color="Winter 2019-20"),size=2.7)+
#   labs(x="Date", y=bquote("Core area " (km^2)))+
#   theme(axis.text=element_text(size=16),
#         axis.title=element_text(size=18,face="bold"))
# 
# #whith Sasha's comments
# p1+
#   geom_smooth(aes(x=interval.id, y=HR18$sum),linetype= "dashed", color="red", method='lm', formula= y~x)+
#   geom_smooth(aes(x=interval.id, y=HR19$sum), color="blue",linetype= "dashed", method='lm', formula= y~x)+
#   theme(panel.background = element_rect(fill = "white", colour = "black",
#                                         size = 0.9, linetype = "solid"))
