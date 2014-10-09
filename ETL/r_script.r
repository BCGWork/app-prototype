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







