rm(list = ls())

libraries <- c("ldatuning", "topicmodels", "ggplot2", 
               "dplyr", "rjson", "quanteda", "lubridate", 
               "parallel", "doParallel", "tidytext", "stringi", 
               "tidyr", "xtable", "devtools", "utf8", "preText",
               "gutenbergr", "data.table", "stringi", "stringr",
               "xml2", "rvest", "tidyverse", "reshape2","httr",
               "ROAuth", "twitteR", "readtext", "tm", "SnowballC",
               "wordcloud", "RColorBrewer", "syuzhet")
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
mta_hashtags_merged <- mta_hashtags_merged[mta_hashtag_columns]

# drop duplicated & overlapped data for some dates
# not working: mta_hashtags_merged_drop <- mta_hashtags_merged[!duplicated(mta_hashtags_merged$text), ]
mta_hashtags_merged <- mta_hashtags_merged[!(duplicated(mta_hashtags_merged[c("text","created")]) | duplicated(mta_hashtags_merged[c("text","created")], fromLast = F)), ]
# get rid of @mta_mood, posts affect the data and possibly results
mta_hashtags_merged <- mta_hashtags_merged[!(mta_hashtags_merged$screenName== 'mta_mood'),]

# bigrams
head(textstat_collocations(mta_hashtags_merged$text))
textstat_collocations(mta_hashtags_merged$text) %>% arrange(-lambda) %>% slice(1:10)

# preprocessing & cleaning
mta_hashtags_merged$text <- tolower(mta_hashtags_merged$text)
mta_hashtags_merged$text <- removeWords(mta_hashtags_merged$text,stopwords('en'))
mta_hashtags_merged$text <- removePunctuation(mta_hashtags_merged$text)
mta_hashtags_merged$text <- gsub('[\r\n]', '', mta_hashtags_merged$text)
# removing 'rt', remove 'rt : ' characters do not work:
mta_hashtags_merged$text <- gsub("rt", " ", mta_hashtags_merged$text)
# remove @ strings: 
# question: remove @ or not? for now, remove, will better help in analyzing word cloud stuff
mta_hashtags_merged$text <- gsub("@\\w+", " ", mta_hashtags_merged$text)
# remove website
mta_hashtags_merged$text <- gsub("http\\w+", " ", mta_hashtags_merged$text)
mta_hashtags_merged$text <- gsub("[ |\t]{2,}", " ", mta_hashtags_merged$text)
mta_hashtags_merged$text <- gsub("^ ", " ", mta_hashtags_merged$text)
mta_hashtags_merged$text <- gsub(" $", " ", mta_hashtags_merged$text)
mta_hashtags_merged$text <- gsub("[^\x01-\x7F]", " ", mta_hashtags_merged$text)

# word cloud
wordcloud(mta_hashtags_merged$text,min.freq = 20,colors=brewer.pal(8, "Dark2"),random.color = TRUE,max.words = 200)

# average retweet and fav
mean(mta_hashtags_merged$favoriteCount)
mean(mta_hashtags_merged$retweetCount)


#-----------------------------
# 4 WEIGHTED DOCUMENT FEATURE MATRIX
#-----------------------------
# WHAT ARE WE WEIGHTING?

# Now we will create a DFM of all the SOTU speeches
mta_hashtag_dfm <- dfm(mta_hashtags_merged$text)
mta_hashtag_dfm[, 1:10]  # notice sparsity
topfeatures(mta_hashtag_dfm)
topfeatures(mta_hashtag_dfm[nrow(mta_hashtag_dfm),])

# 4.1 tfidf - Frequency weighting
weighted_mta_hashtag_dfm <- dfm_tfidf(mta_hashtag_dfm) # uses the absolute frequency of terms in each document
topfeatures(weighted_mta_hashtag_dfm)
topfeatures(weighted_mta_hashtag_dfm[nrow(weighted_mta_hashtag_dfm),])

# 4.2 tfidf - Relative frequency weighting
?dfm_tfidf
normalized_mta_hashtag <- dfm_tfidf(mta_hashtag_dfm, scheme_tf = "prop") # Uses feature proportions within documents: divdes each term by the total count of features in the document
topfeatures(normalized_mta_hashtag)
topfeatures(normalized_mta_hashtag[nrow(normalized_mta_hashtag),])

# Heap's Law
tokens <- tokens(mta_hashtags_merged$text) 
Tee <- sum(lengths(tokens))
mta_hashtag_dfm <- dfm(mta_hashtags_merged$text)
M <- nfeat(mta_hashtag_dfm)  # number of features = number of types
# Let's check using parameter values from MRS Ch. 5 for a corpus with more than 100,000 tokens
k <- 44
b <- .5
k * (Tee)^b
M

# Zipf's Law
plot(log10(1:100), log10(topfeatures(mta_hashtag_dfm, 100)),
     xlab = "log10(rank)", ylab = "log10(frequency)", main = "Zipf's Graph of MTA Related Hashtags")
# Fits a linear regression to check if slope is approx -1.0
regression <- lm(log10(topfeatures(mta_hashtag_dfm, 100)) ~ log10(1:100))
# Adds the fitted line from regression to the plot
abline(regression, col = "red")
# Returns the 95% confidence intervals for the regression coefficients
confint(regression)
# Provides R-squared, F-test, and cofficient estimates from regression
summary(regression)
# Zipf's law as a feature selection tool (e.g. http://www.jmlr.org/papers/volume3/forman03a/forman03a_full.pdf)
plot(1:100, topfeatures(mta_hashtag_dfm, 100),
     xlab = "rank", ylab = "frequency", main = "Top 100 Words in MTA Related Hashtags")

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

top_words <- textstat_frequency(dfm(mta_hashtags_merged$text), n = NULL, groups = NULL) %>% select(feature, frequency)
head(top_words,20)

# histogram
hist(mta_hashtags_merged$sentiment_score,
     main="Histogram of Sentiment Score", 
     xlab="Sentiment Score",
     ylab="Histogram Frequency Distribution",
     border="black", 
     col="green")


# emoji analysis


# nrc sentiment
# need to figure out how nrc sentiment works
mysentiment_mta<-get_nrc_sentiment((mta_hashtags_merged$text))
Sentimentscores_mta<-data.frame(colSums(mysentiment_mta[,]))
names(Sentimentscores_mta)<-"Score"
Sentimentscores_mta<-cbind("sentiment"=rownames(Sentimentscores_mta),Sentimentscores_mta)
rownames(Sentimentscores_mta)<-NULL
ggplot(data=Sentimentscores_mta,aes(x=sentiment,y=Score))+geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("scores")+ggtitle("Sentiments of people behind the tweets on MTA hashtags")+ theme(axis.text.x = element_text(angle = 45, hjust = 1))







# cta_hashtags
rm(list = ls())

libraries <- c("ldatuning", "topicmodels", "ggplot2", 
               "dplyr", "rjson", "quanteda", "lubridate", 
               "parallel", "doParallel", "tidytext", "stringi", 
               "tidyr", "xtable", "devtools", "utf8", "preText",
               "gutenbergr", "data.table", "stringi", "stringr",
               "xml2", "rvest", "tidyverse", "reshape2","httr",
               "ROAuth", "twitteR", "readtext", "tm", "SnowballC",
               "wordcloud", "RColorBrewer", "syuzhet")
lapply(libraries, require, character.only = TRUE)

setwd("/Users/zhengwenjie/Documents/RData/") 

# read in the csv as of 421, 428, 510
CTA_Hashtags_DF_421 <- read.csv(file="/Users/zhengwenjie/Documents/RData/AsOf421/tweets_cta.csv", header=TRUE, sep=",", stringsAsFactors = F)
CTA_Hashtags_DF_428 <- read.csv(file="/Users/zhengwenjie/Documents/RData/AsOf428/tweets_cta_428.csv", header=TRUE, sep=",", stringsAsFactors = F)
CTA_Hashtags_DF_510 <- read.csv(file="/Users/zhengwenjie/Documents/RData/AsOf510/tweets_cta_510.csv", header=TRUE, sep=",", stringsAsFactors = F)
nrow(CTA_Hashtags_DF_421)
nrow(CTA_Hashtags_DF_428)
nrow(CTA_Hashtags_DF_510)


# merge data frames vertically with rbind()
cta_hashtags_merged <- rbind(CTA_Hashtags_DF_421,CTA_Hashtags_DF_428,CTA_Hashtags_DF_510)
# sort by date time
cta_hashtags_merged <- cta_hashtags_merged[order(cta_hashtags_merged$created , decreasing = TRUE ),]
dim(cta_hashtags_merged)
# subset
cta_hashtag_columns <- c("X", "text", "favoriteCount", "created", 
                         "screenName", "retweetCount", "longitude", "latitude")
cta_hashtags_merged <- cta_hashtags_merged[cta_hashtag_columns]

# drop duplicated & overlapped data for some dates
# not working: mta_hashtags_merged_drop <- mta_hashtags_merged[!duplicated(mta_hashtags_merged$text), ]
cta_hashtags_merged <- cta_hashtags_merged[!(duplicated(cta_hashtags_merged[c("text","created")]) | duplicated(cta_hashtags_merged[c("text","created")], fromLast = F)), ]

# preprocessing & cleaning
cta_hashtags_merged$text <- tolower(cta_hashtags_merged$text)
cta_hashtags_merged$text <- removeWords(cta_hashtags_merged$text,stopwords('en'))
cta_hashtags_merged$text <- removePunctuation(cta_hashtags_merged$text)
cta_hashtags_merged$text <- gsub('[\r\n]', '', cta_hashtags_merged$text)
# removing 'rt', remove 'rt : ' characters do not work:
cta_hashtags_merged$text <- gsub("rt", " ", cta_hashtags_merged$text)
# remove @ strings: 
# question: remove @ or not? for now, remove, will better help in analyzing word cloud stuff
cta_hashtags_merged$text <- gsub("@\\w+", " ", cta_hashtags_merged$text)
# remove website
cta_hashtags_merged$text <- gsub("http\\w+", " ", cta_hashtags_merged$text)
cta_hashtags_merged$text <- gsub("[ |\t]{2,}", " ", cta_hashtags_merged$text)
cta_hashtags_merged$text <- gsub("^ ", " ", cta_hashtags_merged$text)
cta_hashtags_merged$text <- gsub(" $", " ", cta_hashtags_merged$text)
cta_hashtags_merged$text <- gsub("[^\x01-\x7F]", " ", cta_hashtags_merged$text)

# word cloud
wordcloud(cta_hashtags_merged$text,min.freq = 20,colors=brewer.pal(8, "Dark2"),random.color = TRUE,max.words = 200)

# average retweet and fav
mean(cta_hashtags_merged$favoriteCount)
mean(cta_hashtags_merged$retweetCount)

#-----------------------------
# 4 WEIGHTED DOCUMENT FEATURE MATRIX
#-----------------------------
# WHAT ARE WE WEIGHTING?

# Now we will create a DFM of all the SOTU speeches
cta_hashtag_dfm <- dfm(cta_hashtags_merged$text)
cta_hashtag_dfm[, 1:10]  # notice sparsity
topfeatures(cta_hashtag_dfm)
topfeatures(cta_hashtag_dfm[nrow(cta_hashtag_dfm),])

# 4.1 tfidf - Frequency weighting
weighted_cta_hashtag_dfm <- dfm_tfidf(cta_hashtag_dfm) # uses the absolute frequency of terms in each document
topfeatures(weighted_cta_hashtag_dfm)
topfeatures(weighted_cta_hashtag_dfm[nrow(weighted_cta_hashtag_dfm),])

# 4.2 tfidf - Relative frequency weighting
?dfm_tfidf
normalized_cta_hashtag <- dfm_tfidf(cta_hashtag_dfm, scheme_tf = "prop") # Uses feature proportions within documents: divdes each term by the total count of features in the document
topfeatures(normalized_cta_hashtag)
topfeatures(normalized_cta_hashtag[nrow(normalized_cta_hashtag),])

# Heap's Law
tokens <- tokens(cta_hashtags_merged$text) 
Tee <- sum(lengths(tokens))
cta_hashtag_dfm <- dfm(cta_hashtags_merged$text)
M <- nfeat(cta_hashtag_dfm)  # number of features = number of types
# Let's check using parameter values from MRS Ch. 5 for a corpus with more than 100,000 tokens
k <- 44
b <- .5
k * (Tee)^b
M

# Zipf's Law
plot(log10(1:100), log10(topfeatures(cta_hashtag_dfm, 100)),
     xlab = "log10(rank)", ylab = "log10(frequency)", main = "Zipf's Graph of MTA Related Hashtags")
# Fits a linear regression to check if slope is approx -1.0
regression <- lm(log10(topfeatures(cta_hashtag_dfm, 100)) ~ log10(1:100))
# Adds the fitted line from regression to the plot
abline(regression, col = "red")
# Returns the 95% confidence intervals for the regression coefficients
confint(regression)
# Provides R-squared, F-test, and cofficient estimates from regression
summary(regression)
# Zipf's law as a feature selection tool (e.g. http://www.jmlr.org/papers/volume3/forman03a/forman03a_full.pdf)
plot(1:100, topfeatures(cta_hashtag_dfm, 100),
     xlab = "rank", ylab = "frequency", main = "Top 100 Words in MTA Related Hashtags")

# sentiment analysis
# Get the data directory from readtable
positivewords <- read.table("/Users/zhengwenjie/Documents/RData/positive-words.txt")
negativewords <- read.table("/Users/zhengwenjie/Documents/RData/negative-words.txt")
# unlist and as.character make them bag of words
positivewords <- as.character(unlist(positivewords))
negativewords <- as.character(unlist(negativewords))

cta_hashtags_merged$text <- tolower(cta_hashtags_merged$text)
cta_hashtags_merged$text <- as.character(cta_hashtags_merged$text)

# calculate and report the occurence of postive & negative words in the review text
# subtract one with another to get the score
cta_hashtags_merged$text_positive <- rowSums(dfm(cta_hashtags_merged$text, select = positivewords))
cta_hashtags_merged$text_negative <- rowSums(dfm(cta_hashtags_merged$text, select = negativewords))
cta_hashtags_merged$sentiment_score <- cta_hashtags_merged$text_positive - cta_hashtags_merged$text_negative

# if is 0 and above, tag it positive and negative for less than 0
cta_hashtags_merged$sentiment_attitude[cta_hashtags_merged$sentiment_score > 0] <- 'positive'
cta_hashtags_merged$sentiment_attitude[cta_hashtags_merged$sentiment_score < 0] <- 'negative'
cta_hashtags_merged$sentiment_attitude[cta_hashtags_merged$sentiment_score == 0] <- 'neutral'
# number of occurence of positive & negative & the total row number in the column
pos_number <- nrow(cta_hashtags_merged[cta_hashtags_merged$sentiment_score > 0,])
neg_number <- nrow(cta_hashtags_merged[cta_hashtags_merged$sentiment_score < 0,])
neu_number <- nrow(cta_hashtags_merged[cta_hashtags_merged$sentiment_score == 0,])

total_rows <- nrow(cta_hashtags_merged)

# calculate and report the ratio
positive_ratio <- pos_number / total_rows
negative_ratio <- neg_number / total_rows
neutral_ratio <- neu_number / total_rows

cat(
  "positive_ratio: ", positive_ratio, "\n",
  "negative_ratio:",  negative_ratio, "\n",
  "neutral_ratio:",  neutral_ratio, "\n"
)

top_words <- textstat_frequency(dfm(cta_hashtags_merged$text), n = NULL, groups = NULL) %>% select(feature, frequency)
head(top_words,20)

# histogram
hist(cta_hashtags_merged$sentiment_score,
     main="Histogram of Sentiment Score", 
     xlab="Sentiment Score",
     ylab="Histogram Frequency Distribution",
     border="black", 
     col="green")


# emoji analysis


# nrc sentiment
# need to figure out how nrc sentiment works
mysentiment_cta<-get_nrc_sentiment((cta_hashtags_merged$text))
Sentimentscores_cta<-data.frame(colSums(mysentiment_cta[,]))
names(Sentimentscores_cta)<-"Score"
Sentimentscores_cta<-cbind("sentiment"=rownames(Sentimentscores_cta),Sentimentscores_cta)
rownames(Sentimentscores_cta)<-NULL
ggplot(data=Sentimentscores_cta,aes(x=sentiment,y=Score))+geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("scores")+ggtitle("Sentiments of people behind the tweets on CTA hashtags")+ theme(axis.text.x = element_text(angle = 45, hjust = 1))

# -----------------------------------------
# nyct subway timeline
# -----------------------------------------

rm(list = ls())

libraries <- c("ldatuning", "topicmodels", "ggplot2", 
               "dplyr", "rjson", "quanteda", "lubridate", 
               "parallel", "doParallel", "tidytext", "stringi", 
               "tidyr", "xtable", "devtools", "utf8", "preText",
               "gutenbergr", "data.table", "stringi", "stringr",
               "xml2", "rvest", "tidyverse", "reshape2","httr",
               "ROAuth", "twitteR", "readtext", "tm", "SnowballC",
               "wordcloud", "RColorBrewer", "syuzhet")
lapply(libraries, require, character.only = TRUE)

setwd("/Users/zhengwenjie/Documents/RData/") 

# create bag of words from nyc open data portal
subway_station_list <- read.csv(file="/Users/zhengwenjie/Documents/RData/DOITT_SUBWAY_STATION_01_13SEPT2010.csv", header=TRUE, sep=",", stringsAsFactors = F)
glimpse(subway_station_list)
# unlist() & asCharacter() &  toLower()
station_names <- unlist(subway_station_list$NAME)
station_names <- as.character(station_names)
station_names <- tolower(station_names)
station_names <- gsub('th', '', station_names)
station_names <- gsub('rd', ' ', station_names)
station_names <- gsub('nd', '', station_names)
station_names <- unique(station_names)
station_names <- gsub("-", " ", station_names)
station_names <- gsub("[[:punct:]]", "", station_names)
station_names <- gsub('   ', ' ', station_names)
station_names <- gsub('  ', ' ', station_names)
station_names <- removeWords(station_names,stopwords('en'))
station_names <- gsub('[\r\n]', ' ', station_names)
station_names <- gsub("@\\w+", " ", station_names)
station_names <- gsub("[ |\t]{2,}", " ", station_names)
station_names <- gsub("^ ", " ", station_names)
station_names <- gsub(" $", " ", station_names)
station_names <- gsub("[^\x01-\x7F]", " ", station_names)
station_names <- gsub("ave", "av", station_names)

# list of lines
line_names <- unlist(subway_station_list$LINE)
line_names <- as.character(line_names)
line_names <- tolower(line_names) 
line_names <- gsub("-", " ", line_names) %>% unlist() %>% unique()

# read in the csv as of 421, 428, 510
subway_DF_421 <- read.csv(file="/Users/zhengwenjie/Documents/RData/AsOf421/NYCTSubway_timeline_df.csv", header=TRUE, sep=",", stringsAsFactors = F)
subway_DF_428 <- read.csv(file="/Users/zhengwenjie/Documents/RData/AsOf428/NYCTSubway_timeline_df_428.csv", header=TRUE, sep=",", stringsAsFactors = F)
subway_DF_510 <- read.csv(file="/Users/zhengwenjie/Documents/RData/AsOf510/NYCTSubway_timeline_df_510.csv", header=TRUE, sep=",", stringsAsFactors = F)

# merge data frames vertically with rbind()
subway_timeline_merged <- rbind(subway_DF_421,subway_DF_428,subway_DF_510)
# sort by date time
subway_timeline_merged <- subway_timeline_merged[order(subway_timeline_merged$created , decreasing = TRUE ),]
dim(subway_timeline_merged)
# subset
subway_timeline_columns <- c("X", "text", "favoriteCount", "created", 
                         "screenName", "retweetCount", "longitude", "latitude")
subway_timeline_merged <- subway_timeline_merged[subway_timeline_columns]

# drop duplicated & overlapped data for some dates
# not working: mta_hashtags_merged_drop <- mta_hashtags_merged[!duplicated(mta_hashtags_merged$text), ]
subway_timeline_merged <- subway_timeline_merged[!(duplicated(subway_timeline_merged[c("text","created")]) | duplicated(subway_timeline_merged[c("text","created")], fromLast = F)), ]
dim(subway_timeline_merged)

# preprocessing & cleaning
subway_timeline_merged$text <- tolower(subway_timeline_merged$text)
subway_timeline_merged$text <- removeWords(subway_timeline_merged$text,stopwords('en'))
# subway_timeline_merged$text <- stemDocument(subway_timeline_merged$text)
# subway_timeline_merged$text <- removePunctuation(subway_timeline_merged$text)
# remove 'th' in station names
subway_timeline_merged$text <- gsub('http\\S+\\s*', "", subway_timeline_merged$text)
# gsub(" ?(f|ht)tp(s?)://(.*)[.][a-z]+", "", x)
subway_timeline_merged$text <- gsub("[[:punct:]]", " ", subway_timeline_merged$text)
subway_timeline_merged$text <- gsub("@\\w+", " ", subway_timeline_merged$text)
#subway_timeline_merged$text <- gsub('th', ' ', subway_timeline_merged$text)
#subway_timeline_merged$text <- gsub('nd', ' ', subway_timeline_merged$text)
#subway_timeline_merged$text <- gsub('rd', ' ', subway_timeline_merged$text)

#subway_timeline_merged$text <- gsub('-', ' ', subway_timeline_merged$text)
subway_timeline_merged$text <- gsub('[\r\n]', ' ', subway_timeline_merged$text)
# removing 'rt', remove 'rt : ' characters do not work:
# subway_timeline_merged$text <- gsub("rt", " ", subway_timeline_merged$text)
subway_timeline_merged$text <- gsub('   ', ' ', subway_timeline_merged$text)
subway_timeline_merged$text <- gsub('  ', ' ', subway_timeline_merged$text)
# remove @ strings: 
# question: remove @ or not? for now, remove, will better help in analyzing word cloud stuff
subway_timeline_merged$text <- gsub("@\\w+", " ", subway_timeline_merged$text)
# remove website
subway_timeline_merged$text <- gsub("[ |\t]{2,}", " ", subway_timeline_merged$text)
subway_timeline_merged$text <- gsub("^ ", " ", subway_timeline_merged$text)
subway_timeline_merged$text <- gsub(" $", " ", subway_timeline_merged$text)
subway_timeline_merged$text <- gsub("[^\x01-\x7F]", " ", subway_timeline_merged$text)
subway_timeline_merged$text <- gsub("avenue", "av", subway_timeline_merged$text)

# word cloud
wordcloud(subway_timeline_merged$text,min.freq = 50,colors=brewer.pal(8, "Dark2"),random.color = TRUE,max.words = 200)
# bigrams
head(textstat_collocations(subway_timeline_merged$text))
textstat_collocations(subway_timeline_merged$text) %>% arrange(-lambda) %>% slice(1:10)

# average retweet and fav
mean(subway_timeline_merged$favoriteCount)
mean(subway_timeline_merged$retweetCount)

# any stat
# subset by 
subway_issue <- subway_timeline_merged[grep("delay", subway_timeline_merged$text), ]
dim(subway_issue)
# top words
top_words_issue <- textstat_frequency(dfm(subway_issue$text), n = NULL, groups = NULL) %>% select(feature, frequency)
head(top_words_issue,20)

# select most affected lines by character lengths = 1
top_lines <- top_words_issue[(nchar(top_words_issue$feature)) == 1, ]
head(top_lines)
station <- subway_issue[grep(paste(station_names, collapse="|"), subway_issue$text),]
#line <- subway_issue[grep(paste(line_names, collapse="|"), subway_issue$text),]
top_station <- data.frame(station_names,freq=rowSums(!adist(station_names,station,partial = T)))
top_station <- top_station[order(-top_station$freq),]
head(top_station, 20)


# very hard to filter out the lines and stations affected. 
# lines can be selected now by choosing the one-character-long strings
# hard to select some stations ends in 'ave'
# using gsbut() function to remove the e in the string 'ave' in 'station_names' list.



# delays stat
# subset by delays
subway_delay <- subway_timeline_merged[grep("delays", subway_timeline_merged$text), ]
dim(subway_delay)
# top words
top_words_delay <- textstat_frequency(dfm(subway_delay$text), n = NULL, groups = NULL) %>% select(feature, frequency)
head(top_words_delay,20)

# delay by subway station
# check the station names in both dataset are in the same format
# grep each element in turn 
# https://stackoverflow.com/questions/38724690/r-filter-rows-that-contain-a-string-from-a-vector/38726850
station_delay <- subway_delay[grep(paste(station_names, collapse="|"), subway_delay$text),]
line_delay <- subway_delay[grep(paste(line_names, collapse="|"), subway_delay$text),]
# delay at stations
top_words_station_delay <- textstat_frequency(dfm(station_delay$text), n = NULL, groups = NULL) %>% select(feature, frequency)
head(top_words_station_delay, 40)
# bigrams
head(textstat_collocations(station_delay$text))
textstat_collocations(station_delay$text) %>% arrange(-lambda) %>% slice(1:10)

# str count station delay # https://stackoverflow.com/questions/49552174/count-the-frequency-of-strings-in-a-dataframe-r
station_count_delay <- data.frame(station_names,freq=rowSums(!adist(station_names,station_delay,partial = T)))
station_count_delay <- station_count_delay[order(-station_count_delay$freq),]
head(station_count_delay, 10)
# str count line delay # https://stackoverflow.com/questions/49552174/count-the-frequency-of-strings-in-a-dataframe-r
line_count_delay <- data.frame(line_names,freq=rowSums(!adist(line_names,line_delay,partial = T)))
line_count_delay <- line_count_delay[order(-line_count_delay$freq),]
head(line_count_delay)



# signal stat
# subset by delays
subway_signal <- subway_timeline_merged[grep("signal", subway_timeline_merged$text), ]
dim(subway_signal)
# top words
top_words_signal <- textstat_frequency(dfm(subway_signal$text), n = NULL, groups = NULL) %>% select(feature, frequency)
head(top_words_signal,20)

# signal problem by subway station
# check the station names in both dataset are in the same format
# grep each element in turn 
# https://stackoverflow.com/questions/38724690/r-filter-rows-that-contain-a-string-from-a-vector/38726850
station_signal <- subway_signal[grep(paste(station_names, collapse="|"), subway_signal$text),]
line_signal <- subway_signal[grep(paste(line_names, collapse="|"), subway_signal$text),]
station_count_signal <- data.frame(station_names,freq=rowSums(!adist(station_names,station_signal,partial = T)))
station_count_signal <- station_count_signal[order(-station_count_signal$freq),]
head(station_count_signal, 10)
line_count_signal <- data.frame(line_names,freq=rowSums(!adist(line_names,line_signal,partial = T)))
line_count_signal <- line_count_signal[order(-line_count_signal$freq),]
head(line_count_signal)
# signal at stations
#top_words_station_delay <- textstat_frequency(dfm(station_delay$text), n = NULL, groups = NULL) %>% select(feature, frequency)
#head(top_words_station_delay, 40)
# bigrams
#head(textstat_collocations(station_delay$text))
#textstat_collocations(station_delay$text) %>% arrange(-lambda) %>% slice(1:10)


# NYPD stat
# subset by NYPD
subway_nypd <- subway_timeline_merged[grep("nypd", subway_timeline_merged$text), ]
dim(subway_nypd)
# top words
top_words_nypd <- textstat_frequency(dfm(subway_nypd$text), n = NULL, groups = NULL) %>% select(feature, frequency)
head(top_words_nypd,20)

# nypd investigation by subway station
# check the station names in both dataset are in the same format
# grep each element in turn 
# https://stackoverflow.com/questions/38724690/r-filter-rows-that-contain-a-string-from-a-vector/38726850
station_nypd <- subway_nypd[grep(paste(station_names, collapse="|"), subway_nypd$text),]
line_nypd <- subway_nypd[grep(paste(line_names, collapse="|"), subway_nypd$text),]
station_count_nypd <- data.frame(station_names,freq=rowSums(!adist(station_names,station_nypd,partial = T)))
station_count_nypd <- station_count_nypd[order(-station_count_nypd$freq),]
head(station_count_nypd, 10)
line_count_nypd <- data.frame(line_names,freq=rowSums(!adist(line_names,line_nypd,partial = T)))
line_count_nypd <- line_count_nypd[order(-line_count_nypd$freq),]
head(line_count_nypd, 10)



# any stat
# subset by 
subway_issue <- subway_timeline_merged[grep("delay", subway_timeline_merged$text), ]
dim(subway_issue)
# top words
top_words_issue <- textstat_frequency(dfm(subway_issue$text), n = NULL, groups = NULL) %>% select(feature, frequency)
head(top_words_issue,20)

# select most affected lines by character lengths = 1
top_lines <- top_words_issue[(nchar(top_words_issue$feature)) == 1, ]
head(top_lines)
#mta_hashtags_merged <- mta_hashtags_merged[!(mta_hashtags_merged$screenName== 'mta_mood'),]

# nypd investigation by subway station
# check the station names in both dataset are in the same format
# grep each element in turn 
# https://stackoverflow.com/questions/38724690/r-filter-rows-that-contain-a-string-from-a-vector/38726850
station <- subway_issue[grep(paste(station_names, collapse="|"), subway_issue$text),]
line <- subway_issue[grep(paste(line_names, collapse="|"), subway_issue$text),]
station_count <- data.frame(station_names,freq=rowSums(!adist(station_names,station,partial = T)))
station_count <- station_count[order(-station_count$freq),]
head(station_count, 10)
line_count <- data.frame(line_names,freq=rowSums(!adist(line_names,line,partial = T)))

# table(c("Apples","Pears","Oranges","Apples","Apples","Pears"))[["Apples"]]
# str_count(s,coll("("))
# str_count(mydf$string, paste(Uniques, collapse='|'))

line_count <- line_count[order(-line_count$freq),]
head(line_count, 10)

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






