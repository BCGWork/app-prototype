#####################################
####### TED Data Manipulation #######
#####################################

#########################
#### HTTP 500 & 503 error
#########################
setwd("Cases/2014-09-26 TED/data/scripts/")
rm(list=ls())
library(data.table)
data1 <- fread("analytics_video_data_1.csv", header=TRUE, sep=",")
data2 <- fread("analytics_video_data_2.csv", header=TRUE, sep=",")
data3 <- fread("analytics_video_data_3.csv", header=TRUE, sep=",")
data <- do.call(rbind, list(data1, data2, data3))
write.csv(data, file="analytics_video_data.csv", row.names=FALSE)


##############################
#### Videos with top 80% views
##############################
setwd("Cases/2014-09-26 TED/data/api_data/")
rm(list=ls())
library(data.table)
profile <- fread("video_profile.csv", header=TRUE, sep=",", colClasses="character")
profile <- profile[, list(video_id, video_detail, upload_date=substr(upload_date, 1, 10))]
perf <- fread("analytics_video_data.csv", header=TRUE, sep=",")
perf <- perf[order(-rank(views)), list(video_id, views)]
perf[, view_pct:=views/sum(views)]
perf[, view_cum_pct:=cumsum(view_pct)]
video_list <- profile[video_id %in% perf[view_cum_pct<=0.8, video_id],
                      list(
                        video_id=paste0("v", video_id),
                        video_detail,
                        upload_date
                      )]
write.csv(video_list, file="top_80_video.csv", row.names=FALSE)


###############################
#### Top 10 country top 5 talks
###############################
setwd("Cases/2014-09-26 TED/data/scripts/dashboard/")
rm(list=ls())
library(data.table)
load("data/metrics_by_video_country.RData")
getData <- function(name) {
  temp <- data[country==name]
  temp <- temp[order(-rank(views))][1:5, list(video_id=paste0("v", video_id), country, views)]
  return(temp)
}
country_list <- c("US", "CA", "GB", "AU", "IN", "DE", "FR", "BR", "NL", "AR")
top_video <- lapply(country_list, getData)
all_top_video <- do.call(rbind, top_video)
all_country <- data[country %in% country_list, list(views=sum(views)), by=country]
write.csv(all_top_video, file="../top_videos_country.csv", row.names=FALSE)
write.csv(all_country, file="../all_country.csv", row.names=FALSE)


#######################
#### Temporal dimension
#######################
setwd("Cases/2014-09-26 TED/data/scripts/dashboard/")
rm(list=ls())
library(data.table)
load("data/metrics_by_video_date.RData")
overall <- date_data[,
                     list(
                       videos=length(video_id),
                       views=sum(views),
                       estimatedMinutesWatched=sum(estimatedMinutesWatched),
                       averageViewPercentage=mean(averageViewPercentage),
                       subscribersGained=sum(subscribersGained)
                     ),
                     keyby=date]
top_cat <- date_category[, list(views=sum(views)), by=youtube_category]
top_cat <- top_cat[order(-rank(views))][1:5]
top_cat_trend <- date_category[youtube_category %in% top_cat$youtube_category]
write.csv(overall, file="overall_trend.csv", row.names=FALSE)
write.csv(top_cat_trend, file="top_category_trend.csv", row.names=FALSE)




