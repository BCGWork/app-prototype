#################
#### load library
#################
library(data.table)

##############
#### load data
##############
## cleaned video data
profile_data <- fread("data/clean_data_v4.csv", header=TRUE, sep=",")
profile_data <- profile_data[!is.na(video_id)]
profile_data[, starts_at:=as.Date(starts_at)]
profile_data[, ends_at:=as.Date(ends_at)]
profile_data[!is.na(starts_at), event_year:=year(starts_at)]
profile_data$overall_rating <- factor(profile_data$overall_rating, levels=c("A", "A-", "B+", "B", "B-", "C+", "C", "C-", "D", "F"))
profile_data$idea_rating <- factor(profile_data$idea_rating, levels=c("A", "A-", "B+", "B", "B-", "C+", "C", "C-", "D", "F"))
profile_data$presentation_rating <- factor(profile_data$presentation_rating, levels=c("A", "A-", "B+", "B", "B-", "C+", "C", "C-", "D", "F"))
profile_data$video_quality <- factor(profile_data$video_quality, levels=c("A", "A-", "B+", "B", "B-", "C+", "C", "C-", "D", "F"))

