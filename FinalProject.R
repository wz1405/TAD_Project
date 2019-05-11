rm(list = ls())

libraries <- c("ldatuning", "topicmodels", "ggplot2", 
               "dplyr", "rjson", "quanteda", "lubridate", 
               "parallel", "doParallel", "tidytext", "stringi", 
               "tidyr", "xtable", "devtools", "utf8", "preText",
               "gutenbergr", "data.table", "stringi", "stringr",
               "xml2", "rvest", "tidyverse", "reshape2","httr",
               "ROAuth", "twitteR")
lapply(libraries, require, character.only = TRUE)

setwd("/Users/zhengwenjie/Documents/RData/") 




# read in the csv as of 421, 428, 510
MTA_Timeline_DF_421 <- read.csv(file="/Users/zhengwenjie/Documents/RData/AsOf421/mta_timeline_df.csv", header=TRUE, sep=",", stringsAsFactors = F)
MTA_Timeline_DF_428 <- read.csv(file="/Users/zhengwenjie/Documents/RData/AsOf428/mta_timeline_df_428.csv", header=TRUE, sep=",", stringsAsFactors = F)
MTA_Timeline_DF_510 <- read.csv(file="/Users/zhengwenjie/Documents/RData/AsOf510/mta_timeline_df_510.csv", header=TRUE, sep=",", stringsAsFactors = F)
glimpse(MTA_Timeline_DF_421)

NYCTSubway_Timeline_DF_421 <- read.csv(file="/Users/zhengwenjie/Documents/RData/AsOf421/NYCTSubway_timeline_df.csv", header=TRUE, sep=",", stringsAsFactors = F)
NYCTSubway_Timeline_DF_428 <- read.csv(file="/Users/zhengwenjie/Documents/RData/AsOf428/NYCTSubway_timeline_df_428.csv", header=TRUE, sep=",", stringsAsFactors = F)
NYCTSubway_Timeline_DF_510 <- read.csv(file="/Users/zhengwenjie/Documents/RData/AsOf510/NYCTSubway_timeline_df_510.csv", header=TRUE, sep=",", stringsAsFactors = F)
nrow(NYCTSubway_Timeline_DF_421)
nrow(NYCTSubway_Timeline_DF_428)
nrow(NYCTSubway_Timeline_DF_510)
glimpse(NYCTSubway_Timeline_DF_421)

NYCTBus_Timeline_DF_421 <- read.csv(file="/Users/zhengwenjie/Documents/RData/AsOf421/NYCTBus_timeline_df.csv", header=TRUE, sep=",", stringsAsFactors = F)
NYCTBus_Timeline_DF_428 <- read.csv(file="/Users/zhengwenjie/Documents/RData/AsOf428/NYCTBus_timeline_df_428.csv", header=TRUE, sep=",", stringsAsFactors = F)
NYCTBus_Timeline_DF_510 <- read.csv(file="/Users/zhengwenjie/Documents/RData/AsOf510/NYCTBus_timeline_df_510.csv", header=TRUE, sep=",", stringsAsFactors = F)
nrow(NYCTBus_Timeline_DF_421)
nrow(NYCTBus_Timeline_DF_428)
nrow(NYCTBus_Timeline_DF_510)

MTA_Hashtags_DF_421 <- read.csv(file="/Users/zhengwenjie/Documents/RData/AsOf421/mta_hashtags.csv", header=TRUE, sep=",", stringsAsFactors = F)
MTA_Hashtags_DF_428 <- read.csv(file="/Users/zhengwenjie/Documents/RData/AsOf428/tweetsDF_428.csv", header=TRUE, sep=",", stringsAsFactors = F)
MTA_Hashtags_DF_510 <- read.csv(file="/Users/zhengwenjie/Documents/RData/AsOf510/tweetsDF_510.csv", header=TRUE, sep=",", stringsAsFactors = F)
nrow(MTA_Hashtags_DF_421)
nrow(MTA_Hashtags_DF_428)
nrow(MTA_Hashtags_DF_510)


# merge data frames vertically with rbind()
mta_hashtags_merged <- rbind(MTA_Hashtags_DF_421,MTA_Hashtags_DF_428,MTA_Hashtags_DF_510)
# sort by date time
mta_hashtags_merged <- mta_hashtags_merged[order(mta_hashtags_merged$created , decreasing = TRUE ),]
dim(mta_hashtags_merged)
# subset
mta_hashtag_columns <- c("X", "text", "favoriteCount", "created", 
            "screenName", "retweetCount", "longitude", "latitude")
mta_hashtags_merged <- mta_hashtags_merged[mta_hashtag_columns]
# drop duplicated & overlapped data for some dates
# not working: mta_hashtags_merged_drop <- mta_hashtags_merged[!duplicated(mta_hashtags_merged$text), ]
mta_hashtags_merged <- mta_hashtags_merged[!(duplicated(mta_hashtags_merged[c("text","created")]) | duplicated(mta_hashtags_merged[c("text","created")], fromLast = F)), ]





#ggplot(aes(x = created, y = favoriteCount), data = MTA_Hashtags_DF_510) + geom_point()


# 


# Summary info 
colnames(MTA_Timeline_DF)  # column names
colnames(NYCTSubway_Timeline_DF)
colnames(NYCTBus_Timeline_DF)
colnames(MTA_Hashtags_DF)  # column names
# column names for timeline and hashtags are the same in twitter
  # dimmension
dim(MTA_Timeline_DF)  # data dimensions
dim(NYCTSubway_Timeline_DF)
dim(NYCTBus_Timeline_DF)
dim(MTA_Hashtags_DF)

sapply(MTA_Timeline_DF, class) # returns class for each variable (column)



head(MTA_Timeline_DF)  # display first lines of an object
head(polling_data, n = 10)  # same as above but specifying number of lines 
tail(polling_data)  # display last lines of an object
dim(MTA_Timeline_DF)  # data dimensions
nrow(polling_data)  # number of rows
ncol(polling_data)  # number of columns
colnames(MTA_Timeline_DF)  # column names
names(polling_data)  # also column names (more general command)
rownames(polling_data) # row names
class(polling_data)  # returns class of an R object
sapply(polling_data, class) # returns class for each variable (column)
str(polling_data)  # display structure of an R object (e.g. a dataframe)
glimpse(polling_data)

#userTimeline(user, n=20, maxID=NULL, sinceID=NULL, includeRts=FALSE,
#             excludeReplies=FALSE, ...)
#homeTimeline(n=25, maxID=NULL, sinceID=NULL, ...)
#mentions(n=25, maxID=NULL, sinceID=NULL, ...)
#retweetsOfMe(n=25, maxID=NULL, sinceID=NULL, ...)
#Warning message:
#  In statusBase(cmd, params, n, 3200, ...) :
#  statuses/user_timeline has a cap of 3200 statuses, clipping






