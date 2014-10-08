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
data5 <- fread("../analytics_date_video_5.csv", header=TRUE, sep=",")
data6 <- fread("../analytics_date_video_6.csv", header=TRUE, sep=",")

# existVideo <- c(
#   unique(data1$video_id),
#   unique(data2$video_id),
#   unique(data3$video_id),
#   unique(data4$video_id),
#   unique(data5$video_id),
#   unique(data6$video_id)
# )
# write.csv(existVideo, file="existing_videos.csv", row.names=FALSE)

data <- do.call(rbind, list(data1, data2, data3, data4, data5, data6))
data$date <- as.Date(data$date)
save(data, file="metrics_by_video_date.RData")
write.csv(data, file="analytics_date_video.csv", row.names=FALSE)




