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
