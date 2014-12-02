setwd("~/Desktop/shared-chao")

require(sqldf)
require("RTextTools")
source("sentiment.R")
source("createTDM.R")

####################### For 'bustechPerks' dataset ###################################
busTechPerks = read.csv("data/bus_tech_perks_full.txt", sep = "\t")
success = read.csv("data/campaign_success.txt", sep = "\t")
comments = read.csv("data/campaign_comments.txt", sep="\t")

busTechPerks = sqldf("select * from busTechPerks inner join success on busTechPerks.campid = success.campid")
busTechPerks = busTechPerks[,-7]
busTechPerks$differential = busTechPerks$money_raised - busTechPerks$campaign_goal

for (i in seq(1:length(busTechPerks))) {
    busTechPerks[which(busTechPerks[i] == ""), i] = NA
}

####################### For the entire dataset #######################################
require(reshape2)

busTechPerks_TDM = createTDM(busTechPerks$perk_descr, "text")
#Words that are in at least 2000 documents (descriptions)
findFreqTerms(busTechPerks_TDM, lowfreq = 2000)
findFreqTerms(busTechPerks_TDM, lowfreq = 5000)

TDM.common = removeSparseTerms(busTechPerks_TDM, sparse = 0.90)
inspect(TDM.common[1:dim(TDM.common)[1], 1:10])

TDM.common = as.matrix(TDM.common)
TDM.common = melt(TDM.common, value.name = "count")
temp = TDM.common[1:11000,]

table(TDM.common$Terms, TDM.common$count)

require(ggplot2)

ggplot(temp, aes(x = Docs, y = Terms, fill = count)) +
    geom_tile(color = "white") +
    scale_fill_gradient(high="#FF0000" , low="#FFFFFF") +
    ylab("") +
    xlab("Campaigns") +
    ggtitle("Word Frequency Matrix For Campaign Descriptions") + 
    theme(panel.background = element_blank()) +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

busTechPerks_TDM = createTDM(busTechPerks$perk_titl, "text")
findFreqTerms(busTechPerks_TDM, lowfreq = 1000)

TDM.common = removeSparseTerms(busTechPerks_TDM, sparse = 0.98)
inspect(TDM.common[1:dim(TDM.common)[1], 1:10])

TDM.common = as.matrix(TDM.common)
TDM.common = melt(TDM.common, value.name = "count")

table(TDM.common$Terms, TDM.common$count)

temp = TDM.common[1:6000,]

ggplot(temp, aes(x = Docs, y = Terms, fill = count)) +
    geom_tile(color = "white") +
    scale_fill_gradient(high="#FF0000" , low="#FFFFFF") +
    ylab("") +
    xlab("Campaigns") +
    ggtitle("Word Frequency Matrix For Campaign Titles") + 
    theme(panel.background = element_blank()) +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())


####################### For the successful campaigns #######################################

successful_busTechPerks = busTechPerks[which(busTechPerks$successful == 1),]

successful_busTechPerks_TDM = createTDM(successful_busTechPerks$perk_descr, "text")
#Words that are in at least 2000 documents (descriptions)
findFreqTerms(successful_busTechPerks_TDM, lowfreq = 1000)

TDM.common = removeSparseTerms(successful_busTechPerks_TDM, sparse = 0.90)
numTerms = dim(TDM.common)[1]
inspect(TDM.common[1:numTerms, 1:10])

TDM.common = as.matrix(TDM.common)
TDM.common = melt(TDM.common, value.name = "count")
temp = TDM.common[1:10000,]

table(TDM.common$Terms, TDM.common$count)

require(ggplot2)

ggplot(temp, aes(x = Docs, y = Terms, fill = count)) +
    geom_tile(color = "white") +
    scale_fill_gradient(high="#FF0000" , low="#FFFFFF") +
    ylab("") +
    xlab("Campaigns") +
    ggtitle("Word Frequency Matrix For Successful Campaign Descriptions") + 
    theme(panel.background = element_blank()) +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())


successful_busTechPerks_TDM = createTDM(successful_busTechPerks$perk_titl, "text")
#Words that are in at least 2000 documents (descriptions)
findFreqTerms(successful_busTechPerks_TDM, lowfreq = 100)

TDM.common = removeSparseTerms(successful_busTechPerks_TDM, sparse = 0.98)
numTerms = dim(TDM.common)[1]
inspect(TDM.common[1:numTerms, 1:10])

TDM.common = as.matrix(TDM.common)
TDM.common = melt(TDM.common, value.name = "count")
temp = TDM.common[1:21000,]

table(TDM.common$Terms, TDM.common$count)

require(ggplot2)

ggplot(temp, aes(x = Docs, y = Terms, fill = count)) +
    geom_tile(color = "white") +
    scale_fill_gradient(high="#FF0000" , low="#FFFFFF") +
    ylab("") +
    xlab("Campaigns") +
    ggtitle("Word Frequency Matrix For Successful Campaign Titles") + 
    theme(panel.background = element_blank()) +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())



####################### For the unsuccessful campaigns #######################################
unsuccessful_busTechPerks = busTechPerks[which(busTechPerks$successful != 1),]

unsuccessful_busTechPerks_TDM = createTDM(unsuccessful_busTechPerks$perk_descr, "text")
#Words that are in at least 2000 documents (descriptions)
findFreqTerms(unsuccessful_busTechPerks_TDM, lowfreq = 10000)

TDM.common = removeSparseTerms(unsuccessful_busTechPerks_TDM, sparse = 0.93)
numTerms = dim(TDM.common)[1]
inspect(TDM.common[1:numTerms, 1:10])

TDM.common = as.matrix(TDM.common)
TDM.common = melt(TDM.common, value.name = "count")
temp = TDM.common[1:19000,]

table(TDM.common$Terms, TDM.common$count)

require(ggplot2)

ggplot(temp, aes(x = Docs, y = Terms, fill = count)) +
    geom_tile(color = "white") +
    scale_fill_gradient(high="#FF0000" , low="#FFFFFF") +
    ylab("") +
    xlab("Campaigns") +
    ggtitle("Word Frequency Matrix For Unsuccessful Campaign Descriptions") + 
    theme(panel.background = element_blank()) +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())


unsuccessful_busTechPerks_TDM = createTDM(unsuccessful_busTechPerks$perk_titl, "text")
#Words that are in at least 1000 documents (descriptions)
findFreqTerms(unsuccessful_busTechPerks_TDM, lowfreq = 1000)

TDM.common = removeSparseTerms(unsuccessful_busTechPerks_TDM, sparse = 0.99)
numTerms = dim(TDM.common)[1]
inspect(TDM.common[1:numTerms, 1:10])

TDM.common = as.matrix(TDM.common)
TDM.common = melt(TDM.common, value.name = "count")
temp = TDM.common[1:24000,]

table(TDM.common$Terms, TDM.common$count)

require(ggplot2)

ggplot(temp, aes(x = Docs, y = Terms, fill = count)) +
    geom_tile(color = "white") +
    scale_fill_gradient(high="#FF0000" , low="#FFFFFF") +
    ylab("") +
    xlab("Campaigns") +
    ggtitle("Word Frequency Matrix For Successful Campaign Titles") + 
    theme(panel.background = element_blank()) +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())



####################### Comments of successful busTechPerks ##################################

successful_busTechPerks_comments = sqldf("select * from comments inner join 
                                         (select campid from successful_busTechPerks) as a
                                         on comments.campid = a.campid")
successful_busTechPerks_comments = successful_busTechPerks_comments[,-9]
successful_busTechPerks_comments = successful_busTechPerks_comments[!duplicated(successful_busTechPerks_comments),]

successful_busTechPerks_comment_score = 
    score.sentiment(successful_busTechPerks_comments$comment, hu.liu.pos, hu.liu.neg, .progress="text")

successful_busTechPerks_sentiment_counts = table(successful_busTechPerks_comment_score$sentiment)

bp = barplot(unsuccessful_busTechPerks_sentiment_counts[order(unsuccessful_busTechPerks_sentiment_counts, c(4,1,2,3,5))]
             ,xlab = "sentiment", ylab = "frequency", main = "Frequency of Sentiment Scores for Successful BusTechPerks",
             col = "blue", ylim = c(0, 25000),
             args.legend = list(title = "Sentiment Score", x = "topleft", cex = .7))
text(bp, 0, round(successful_busTechPerks_sentiment_counts[order(unsuccessful_busTechPerks_sentiment_counts, c(4,1,2,3,5))], 1),cex=1,pos=3)


########### TDM ##########

successful_busTechPerks_comments_TDM = createTDM(successful_busTechPerks_comments$comment, "text")
#Words that are in at least 2000 documents (descriptions)
findFreqTerms(successful_busTechPerks_comments_TDM, lowfreq = 1000)

TDM.common = removeSparseTerms(successful_busTechPerks_comments_TDM, sparse = 0.95)
numTerms = dim(TDM.common)[1]
inspect(TDM.common[1:numTerms, 1:10])

TDM.common = as.matrix(TDM.common)
TDM.common = melt(TDM.common, value.name = "count")
temp = TDM.common[1:19000,]

table(TDM.common$Terms, TDM.common$count)

require(ggplot2)

ggplot(temp, aes(x = Docs, y = Terms, fill = count)) +
    geom_tile(color = "white") +
    scale_fill_gradient(high="#FF0000" , low="#FFFFFF") +
    ylab("") +
    xlab("Campaigns") +
    ggtitle("Word Frequency Matrix For Successful Campaign Comments") + 
    theme(panel.background = element_blank()) +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())


####################### Comments of unsuccessful busTechPerks ##################################
unsuccessful_busTechPerks_comments = sqldf("select * from comments inner join 
                                           (select campid from unsuccessful_busTechPerks) as a
                                           on comments.campid = a.campid")
unsuccessful_busTechPerks_comments = unsuccessful_busTechPerks_comments[,-9]
unsuccessful_busTechPerks_comments = unsuccessful_busTechPerks_comments[!duplicated(unsuccessful_busTechPerks_comments),]

unsuccessful_busTechPerks_comment_score = 
    score.sentiment(unsuccessful_busTechPerks_comments$comment, hu.liu.pos, hu.liu.neg, .progress="text")

unsuccessful_busTechPerks_sentiment_counts = table(unsuccessful_busTechPerks_comment_score$sentiment)

bp = barplot(unsuccessful_busTechPerks_sentiment_counts[order(unsuccessful_busTechPerks_sentiment_counts, c(4,1,2,3,5))]
             ,xlab = "sentiment", ylab = "frequency", main = "Frequency of Sentiment Scores for Unsuccessful BusTechPerks",
             col = "blue", ylim = c(0, 25000),
             args.legend = list(title = "Sentiment Score", x = "topleft", cex = .7))
text(bp, 0, round(unsuccessful_busTechPerks_sentiment_counts[order(unsuccessful_busTechPerks_sentiment_counts, c(4,1,2,3,5))], 1),cex=1,pos=3)


########### TDM ##########

unsuccessful_busTechPerks_comments_TDM = createTDM(unsuccessful_busTechPerks_comments$comment, "text")
#Words that are in at least 2000 documents (descriptions)
findFreqTerms(unsuccessful_busTechPerks_comments_TDM, lowfreq = 1000)

TDM.common = removeSparseTerms(unsuccessful_busTechPerks_comments_TDM, sparse = 0.95)
numTerms = dim(TDM.common)[1]
inspect(TDM.common[1:numTerms, 1:10])

TDM.common = as.matrix(TDM.common)
TDM.common = melt(TDM.common, value.name = "count")
temp = TDM.common[1:20000,]

table(TDM.common$Terms, TDM.common$count)

require(ggplot2)

ggplot(temp, aes(x = Docs, y = Terms, fill = count)) +
    geom_tile(color = "white") +
    scale_fill_gradient(high="#FF0000" , low="#FFFFFF") +
    ylab("") +
    xlab("Campaigns") +
    ggtitle("Word Frequency Matrix For Unsuccessful Campaign Comments") + 
    theme(panel.background = element_blank()) +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())


#######################################################################################


#Percentage of successful business
nrow(successful_busTechPerks)/(nrow(unsuccessful_busTechPerks) + nrow(successful_busTechPerks))

#Number of unique successful business campaigns
length(unique(successful_busTechPerks$campid))
unique_successful_busTechPerks_campid = unique(successful_busTechPerks$campid)
subset_successful_busTechPerks_campid = sample(unique_successful_busTechPerks_campid, size = 50, replace = FALSE)


#Number of unique successful business campaigns
length(unique(unsuccessful_busTechPerks$campid))
unique_unsuccessful_busTechPerks_campid = unique(unsuccessful_busTechPerks$campid)
subset_unsuccessful_busTechPerks_campid = sample(unique_unsuccessful_busTechPerks_campid, size = 100, replace = FALSE)

subset = c(subset_successful_busTechPerks_campid, subset_unsuccessful_busTechPerks_campid)
subset = data.frame(campid = subset)

subset_busTechPerks = sqldf("select * from busTechPerks inner join subset on
                            busTechPerks.campid = subset.campid inner join success on busTechPerks.campid = success.campid")
subset_busTechPerks = subset_busTechPerks[,c(-7,-8)]


# CREATE THE DOCUMENT-TERM MATRIX FOR CLASSIFICATION
start.time = Sys.time()
doc_matrix = create_matrix(subset_busTechPerks$perk_descr, language="english", removeNumbers=TRUE, stemWords=TRUE, removeSparseTerms=0.999)
end.time = Sys.time()
time.taken = end.time - start.time
time.taken

#state what is training set and what is test
trainingsize = 0.8*nrow(subset_busTechPerks)
container <- create_container(doc_matrix, subset_busTechPerks$successful, trainSize=1:trainingsize,testSize=(trainingsize+1):nrow(subset_busTechPerks), virgin=FALSE)
SVM = train_model(container, "SVM")
GLMNET <- train_model(container,"GLMNET")
MAXENT <- train_model(container,"MAXENT")
BOOSTING <- train_model(container,"BOOSTING")
RF <- train_model(container,"RF")

SVM_CLASSIFY <- classify_model(container, SVM)
GLMNET_CLASSIFY <- classify_model(container, GLMNET)
MAXENT_CLASSIFY <- classify_model(container, MAXENT)
BOOSTING_CLASSIFY <- classify_model(container, BOOSTING)
RF_CLASSIFY <- classify_model(container, RF)

analytics <- create_analytics(container, cbind(SVM_CLASSIFY, RF_CLASSIFY, BOOSTING_CLASSIFY, MAXENT_CLASSIFY))
summary(analytics)

# CREATE THE data.frame SUMMARIES
topic_summary <- analytics@label_summary
alg_summary <- analytics@algorithm_summary
ens_summary <-analytics@ensemble_summary
doc_summary <- analytics@document_summary

rm(bustechPerks)

#Sort posts by perk_id
#Do a rudimentay graph of the various clusters. See which is the most/least frequent perk_id
#Assume that perk_id is coded from basic(low) to complex(high)
#Identify the outliers

#Sort posts by prices
#Compare to perk_id. Would expect higher prices to correspond to a higher perk_id


#Use cosine similarity (Levenshtein library in python)
#Calculate a sentiment score for each comment.
#Look up NLP libraries for R, or change to python
#Iterate through comments calculating the sentiment for each
#Sort again by sentiment scores. See which perk_id or price gets the highest score
#Can hopefully identify the ones that are the most highly favored

#Do a word frequency count on the perk descriptions. Expect the word "free" to be the most common
#but what will be the next 10 most common words?