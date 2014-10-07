#########################################
####### TED data shaping R Script #######
#########################################
setwd("Cases/2014-09-26 TED/youtube_api/")

#####################################
#### YouTube API HTTP 500 & 503 error
#####################################
rm(list=ls())
library(data.table)
data1 <- fread("../analytics_date_video_1.csv", header=TRUE, sep=",")
data2 <- fread("../analytics_date_video_2.csv", header=TRUE, sep=",")
data3 <- fread("../analytics_date_video_3.csv", header=TRUE, sep=",")
data4 <- fread("../analytics_date_video_4.csv", header=TRUE, sep=",")
existVideo <- c(
  unique(data1$video_id),
  unique(data2$video_id),
  unique(data3$video_id),
  unique(data4$video_id)
)
write.csv(existVideo, file="existing_videos.csv", row.names=FALSE)



