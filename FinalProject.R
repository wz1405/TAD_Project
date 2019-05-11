rm(list = ls())

libraries <- c("ldatuning", "topicmodels", "ggplot2", 
               "dplyr", "rjson", "quanteda", "lubridate", 
               "parallel", "doParallel", "tidytext", "stringi", 
               "tidyr", "xtable", "devtools", "utf8", "preText",
               "gutenbergr", "data.table", "stringi", "stringr",
               "xml2", "rvest", "tidyverse", "reshape2","httr",
               "ROAuth", "twitteR", "readtext", "tm", "SnowballC",
               "wordcloud", "RColorBrewer")
lapply(libraries, require, character.only = TRUE)

setwd("/Users/zhengwenjie/Documents/RData/") 




# read in the csv as of 421, 428, 510
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

mta_hashtags_merged$text <- tolower(mta_hashtags_merged$text)
mta_hashtags_merged$text <- removeWords(mta_hashtags_merged$text,stopwords('en'))
mta_hashtags_merged$text <- removePunctuation(mta_hashtags_merged$text)
mta_hashtags_merged$text <- gsub('[\r\n]', '', mta_hashtags_merged$text)

mta_hashtags_merged <- mta_hashtags_merged[mta_hashtag_columns]
# drop duplicated & overlapped data for some dates
# not working: mta_hashtags_merged_drop <- mta_hashtags_merged[!duplicated(mta_hashtags_merged$text), ]
mta_hashtags_merged <- mta_hashtags_merged[!(duplicated(mta_hashtags_merged[c("text","created")]) | duplicated(mta_hashtags_merged[c("text","created")], fromLast = F)), ]
# get rid of @mta_mood, posts affect the data and possibly results
mta_hashtags_merged <- mta_hashtags_merged[!(mta_hashtags_merged$screenName== 'mta_mood'),]

# sentiment analysis
# Get the data directory from readtable
positivewords <- read.table("/Users/zhengwenjie/Documents/RData/positive-words.txt")
negativewords <- read.table("/Users/zhengwenjie/Documents/RData/negative-words.txt")
# unlist and as.character make them bag of words
positivewords <- as.character(unlist(positivewords))
negativewords <- as.character(unlist(negativewords))

mta_hashtags_merged$text <- tolower(mta_hashtags_merged$text)
mta_hashtags_merged$text <- as.character(mta_hashtags_merged$text)

# calculate and report the occurence of postive & negative words in the review text
# subtract one with another to get the score
mta_hashtags_merged$text_positive <- rowSums(dfm(mta_hashtags_merged$text, select = positivewords))
mta_hashtags_merged$text_negative <- rowSums(dfm(mta_hashtags_merged$text, select = negativewords))
mta_hashtags_merged$sentiment_score <- mta_hashtags_merged$text_positive - mta_hashtags_merged$text_negative

# if is 0 and above, tag it positive and negative for less than 0
mta_hashtags_merged$sentiment_attitude[mta_hashtags_merged$sentiment_score > 0] <- 'positive'
mta_hashtags_merged$sentiment_attitude[mta_hashtags_merged$sentiment_score < 0] <- 'negative'
mta_hashtags_merged$sentiment_attitude[mta_hashtags_merged$sentiment_score == 0] <- 'neutral'
# number of occurence of positive & negative & the total row number in the column
pos_number <- nrow(mta_hashtags_merged[mta_hashtags_merged$sentiment_score > 0,])
neg_number <- nrow(mta_hashtags_merged[mta_hashtags_merged$sentiment_score < 0,])
neu_number <- nrow(mta_hashtags_merged[mta_hashtags_merged$sentiment_score == 0,])

total_rows <- nrow(mta_hashtags_merged)

# calculate and report the ratio
positive_ratio <- pos_number / total_rows
negative_ratio <- neg_number / total_rows
neutral_ratio <- neu_number / total_rows

cat(
  "positive_ratio: ", positive_ratio, "\n",
  "negative_ratio:",  negative_ratio, "\n",
  "neutral_ratio:",  neutral_ratio, "\n"
)
# histogram
hist(mta_hashtags_merged$sentiment_score,
     main="Histogram of Sentiment Score", 
     xlab="Sentiment Score",
     ylab="Histogram Frequency Distribution",
     border="black", 
     col="green")


