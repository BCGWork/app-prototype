#################
#### load library
#################
library(data.table)

##############
#### load data
##############
## video profile
perf_data <- fread("data/video_profile_activity.csv", header=TRUE, colClasses="character")
perf_data <- perf_data[youtube_category!="#N/A"]
perf_data[, upload_date:=as.Date(perf_data$upload_date)]
perf_data[, views:=as.numeric(perf_data$views)]
perf_data[, days:=as.numeric(perf_data$days)]
perf_data[, comments:=as.numeric(perf_data$comments)]
perf_data[, favoritesAdded:=as.numeric(perf_data$favoritesAdded)]
perf_data[, favoritesRemoved:=as.numeric(perf_data$favoritesRemoved)]
perf_data[, likes:=as.numeric(perf_data$likes)]
perf_data[, dislikes:=as.numeric(perf_data$dislikes)]
perf_data[, shares:=as.numeric(perf_data$shares)]
perf_data[, estimatedMinutesWatched:=as.numeric(perf_data$estimatedMinutesWatched)]
perf_data[, averageViewDuration:=as.numeric(perf_data$averageViewDuration)]
perf_data[, averageViewPercentage:=as.numeric(perf_data$averageViewPercentage)]
perf_data[, subscribersGained:=as.numeric(perf_data$subscribersGained)]
perf_data[, subscribersLost:=as.numeric(perf_data$subscribersLost)]

## category_data
category_data <- perf_data[,
                      list(
                        videos = length(video_id),
                        views = sum(views),
                        days = sum(days),
                        comments = sum(comments),
                        favoritesAdded = sum(favoritesAdded),
                        favoritesRemoved = sum(favoritesRemoved),
                        likes = sum(likes),
                        dislikes = sum(dislikes),
                        shares = sum(shares),
                        estimatedMinutesWatched = sum(estimatedMinutesWatched),
                        averageViewDuration = mean(averageViewDuration),
                        averageViewPercentage = mean(averageViewPercentage),
                        subscribersGained = sum(subscribersGained),
                        subscribersLost = sum(subscribersLost),
                        viewsPerVideo = sum(views)/length(video_id),
                        minutesWatchedPerVideo = sum(estimatedMinutesWatched)/length(video_id),
                        favoritesPerVideo = sum(favoritesAdded)/length(video_id),
                        likesPerVideo = sum(likes)/length(video_id),
                        sharesPerVideo = sum(shares)/length(video_id),
                        viewsPerDay = sum(views)/sum(days),
                        minutesWatchedPerDay = sum(estimatedMinutesWatched)/sum(days),
                        favoritesPerDay = sum(favoritesAdded)/sum(days),
                        likesPerDay = sum(likes)/sum(days),
                        sharesPerDay = sum(shares)/sum(days)
                      ),
                      keyby=youtube_category]

## video by country
load("data/metrics_by_video_country.RData")
country_raw <- data
rm(data)
profile <- perf_data[, list(video_id, video_detail, youtube_category, upload_date, days)]
profile[, video_id:=substring(video_id, 2)]
setkey(country_raw, video_id)
setkey(profile, video_id)
country_data <- profile[country_raw]

## video by date
load("data/metrics_by_video_date.RData")
date_raw <- data
rm(data)
profile <- perf_data[, list(video_id, video_detail, youtube_category, upload_date)]
profile[, video_id:=substring(video_id, 2)]
setkey(date_raw, video_id)
setkey(profile, video_id)
date_data <- profile[date_raw]


