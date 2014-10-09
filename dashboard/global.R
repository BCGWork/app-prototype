library(data.table)

data <- fread("data/video_profile_activity.csv", header=TRUE, colClasses="character")
data <- data[youtube_category!="#N/A"]
data[, upload_date:=as.Date(data$upload_date)]
data[, views:=as.numeric(data$views)]
data[, days:=as.numeric(data$days)]
data[, comments:=as.numeric(data$comments)]
data[, favoritesAdded:=as.numeric(data$favoritesAdded)]
data[, favoritesRemoved:=as.numeric(data$favoritesRemoved)]
data[, likes:=as.numeric(data$likes)]
data[, dislikes:=as.numeric(data$dislikes)]
data[, shares:=as.numeric(data$shares)]
data[, estimatedMinutesWatched:=as.numeric(data$estimatedMinutesWatched)]
data[, averageViewDuration:=as.numeric(data$averageViewDuration)]
data[, averageViewPercentage:=as.numeric(data$averageViewPercentage)]
data[, subscribersGained:=as.numeric(data$subscribersGained)]
data[, subscribersLost:=as.numeric(data$subscribersLost)]

category_data <- data[, list(
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

