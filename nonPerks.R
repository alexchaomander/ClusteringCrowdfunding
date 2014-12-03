setwd("~/Desktop/shared-chao")

require(sqldf)
require("RTextTools")
source("sentiment.R")
source("createTDM.R")
source("createWordCloud.R")

####################### For 'nonPerks' dataset ###################################
nonPerks = read.csv("data/nonTech nonBus nonCommunity perks.txt", sep="\t")
success = read.csv("data/campaign_success.txt", sep = "\t")
comments = read.csv("data/campaign_comments.txt", sep="\t")


nonPerks = sqldf("select * from nonPerks inner join success on nonPerks.campid = success.campid")
nonPerks$perks_claimed = as.numeric(paste(nonPerks$perks_claimed))
nonPerks = sqldf("select * from nonPerks order by nonPerks.category")
nonPerks = nonPerks[,-13]
nonPerks$differential = nonPerks$money_raised - nonPerks$campaign_goal

for (i in seq(1:length(nonPerks))) {
    nonPerks[which(nonPerks[i] == ""), i] = NA
}

####################### For the entire dataset #######################################
require(reshape2)

nonPerks_TDM = createTDM(nonPerks$perk_descr, "text")
#Words that are in at least 2000 documents (descriptions)
findFreqTerms(nonPerks_TDM, lowfreq = 2000)
findFreqTerms(nonPerks_TDM, lowfreq = 5000)

TDM.common = removeSparseTerms(nonPerks_TDM, sparse = 0.90)
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

### For perk titles
nonPerks_TDM = createTDM(nonPerks$perk_titl, "text")
findFreqTerms(nonPerks_TDM, lowfreq = 1000)

TDM.common = removeSparseTerms(nonPerks_TDM, sparse = 0.98)
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

successful_nonPerks = nonPerks[which(nonPerks$successful == 1),]
# Comments of successful nonPerks 
successful_nonPerks_comments = sqldf("select * from comments inner join 
                                     (select campid from successful_nonPerks) as a
                                     on comments.campid = a.campid")
successful_nonPerks_comments = successful_nonPerks_comments[,-9]
successful_nonPerks_comments = successful_nonPerks_comments[!duplicated(successful_nonPerks_comments),]

successful_nonPerks_comment_score = 
    score.sentiment(successful_nonPerks_comments$comment, hu.liu.pos, hu.liu.neg, .progress="text")
successful_nonPerks_sentiment_counts = table(successful_nonPerks_comment_score$sentiment)

bp = barplot(successful_nonPerks_sentiment_counts[c(4,1,2,3,5)]
             ,xlab = "sentiment", ylab = "frequency", main = "Frequency of Sentiment Scores for Successful nonPerks",
             col = "blue", ylim = c(0, 25000),
             args.legend = list(title = "Sentiment Score", x = "topleft", cex = .7))
text(bp, 0, round(successful_nonPerks_sentiment_counts[c(4,1,2,3,5)], 1),cex=1,pos=3)


successful_nonPerks_comment_score$campid = successful_nonPerks_comments$campid
successful_nonPerks_comment_score$commentdate = successful_nonPerks_comments$commentdate

time = read.csv("dateData.txt", sep=",")
colnames(time) = c("timestamp", "date")

successful_nonPerks_comment_score = sqldf("select * from successful_nonPerks_comment_score inner join time
                                              on time.timestamp = successful_nonPerks_comment_score.commentdate")
successful_nonPerks_comment_score = successful_nonPerks_comment_score[-6]
successful_nonPerks_comment_score$standardDate = lapply(successful_nonPerks_comment_score$date, function(date) {
    result = as.Date(date, format = "%d %B %Y")
})

a = table(sort(successful_nonPerks_comment_score$date), successful_nonPerks_comment_score$sentiment)
#Ideally will try to take care of sorting but it is a pain in R
a
plot(a, las=2, col = "blue", main = "Sentiment Over Time for Successful Campaigns" , ylab="Sentiment",xlab = "Date")

# Word cloud for comments left on successful campaigns
createWordCloud(successful_nonPerks_comments$comment, "text")

########### TDM ##########

successful_nonPerks_comments_TDM = createTDM(successful_nonPerks_comments$comment, "text")
#Words that are in at least 2000 documents (descriptions)
findFreqTerms(successful_nonPerks_comments_TDM, lowfreq = 1000)

TDM.common = removeSparseTerms(successful_nonPerks_comments_TDM, sparse = 0.95)
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


####################### Comments of unsuccessful nonPerks ##################################
unsuccessful_nonPerks = nonPerks[which(nonPerks$successful != 1),]
unsuccessful_nonPerks_comments = sqldf("select * from comments inner join 
                                       (select campid from unsuccessful_nonPerks) as a
                                       on comments.campid = a.campid")
unsuccessful_nonPerks_comments = unsuccessful_nonPerks_comments[,-9]
unsuccessful_nonPerks_comments = unsuccessful_nonPerks_comments[!duplicated(unsuccessful_nonPerks_comments),]
unsuccessful_nonPerks_comment_score = 
    score.sentiment(unsuccessful_nonPerks_comments$comment, hu.liu.pos, hu.liu.neg, .progress="text")

unsuccessful_nonPerks_sentiment_counts = table(unsuccessful_nonPerks_comment_score$sentiment)

bp = barplot(unsuccessful_nonPerks_sentiment_counts[c(4,1,2,3,5)]
             ,xlab = "sentiment", ylab = "frequency", main = "Frequency of Sentiment Scores for Unsuccessful nonPerks",
             col = "red", ylim = c(0, 25000),
             args.legend = list(title = "Sentiment Score", x = "topleft", cex = .7))
text(bp, 0, round(unsuccessful_nonPerks_sentiment_counts[c(4,1,2,3,5)], 1),cex=1,pos=3)

unsuccessful_nonPerks_comment_score$campid = unsuccessful_nonPerks_comments$campid
unsuccessful_nonPerks_comment_score$commentdate = unsuccessful_nonPerks_comments$commentdate

time = read.csv("dateData.txt", sep=",")
colnames(time) = c("timestamp", "date")

unsuccessful_nonPerks_comment_score = sqldf("select * from unsuccessful_nonPerks_comment_score inner join time
                                                on time.timestamp = unsuccessful_nonPerks_comment_score.commentdate")
unsuccessful_nonPerks_comment_score = unsuccessful_nonPerks_comment_score[-6]
unsuccessful_nonPerks_comment_score$standardDate = lapply(unsuccessful_nonPerks_comment_score$date, function(date) {
    result = as.Date(date, format = "%d %B %Y")
})

a = table(sort(unsuccessful_nonPerks_comment_score$date), unsuccessful_nonPerks_comment_score$sentiment)
#Ideally will try to take care of sorting but it is a pain in R
a
plot(a, las=2, col = "red", main = "Sentiment over time for Unsuccessful Campaigns" , ylab="Sentiment",xlab = "Date")

# Word cloud for comments left on unsuccessful campaigns
createWordCloud(unsuccessful_nonPerks_comments$comment, "text")

########### TDM ##########

unsuccessful_nonPerks_comments_TDM = createTDM(unsuccessful_nonPerks_comments$comment, "text")
#Words that are in at least 2000 documents (descriptions)
findFreqTerms(unsuccessful_nonPerks_comments_TDM, lowfreq = 1000)

TDM.common = removeSparseTerms(unsuccessful_nonPerks_comments_TDM, sparse = 0.95)
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

#################################### CLASSIFICATION ##########################################

#Percentage of successful non tech/business
nrow(successful_nonPerks)/(nrow(unsuccessful_nonPerks) + nrow(successful_nonPerks))

#Number of unique successful community campaigns
length(unique(successful_nonPerks$campid))
unique_successful_nonPerks_campid = unique(successful_nonPerks$campid)
subset_successful_nonPerks_campid = sample(unique_successful_nonPerks_campid, size = 50, replace = FALSE)


#Number of unique successful business campaigns
length(unique(unsuccessful_nonPerks$campid))
unique_unsuccessful_nonPerks_campid = unique(unsuccessful_nonPerks$campid)
subset_unsuccessful_nonPerks_campid = sample(unique_unsuccessful_nonPerks_campid, size = 100, replace = FALSE)

subset = c(subset_successful_nonPerks_campid, subset_unsuccessful_nonPerks_campid)
subset = data.frame(campid = subset)

subset_nonPerks = sqldf("select * from nonPerks inner join subset on
                        nonPerks.campid = subset.campid inner join success on nonPerks.campid = success.campid")
subset_nonPerks = subset_nonPerks[,c(-13,-14)]


# CREATE THE DOCUMENT-TERM MATRIX
doc_matrix = create_matrix(subset_nonPerks$perk_descr, language="english", removeNumbers=TRUE, stemWords=TRUE, removeSparseTerms=0.999)

#Plotting k means clustering 
require(cluster)
kmeans5 <- kmeans(doc_matrix, 5)

#Merge cluster assignment back to keywords
kw_with_cluster <- as.data.frame(cbind(subset_nonPerks$perk_descr,
                                       subset_nonPerks$price, subset_nonPerks$differential, kmeans5$cluster))
names(kw_with_cluster) <- c("keyword", "price", "differential", "kmeans5")

#Make df for each cluster result, quickly "eyeball" results
cluster1 <- subset(kw_with_cluster, subset=kmeans5 == 1)
cluster2 <- subset(kw_with_cluster, subset=kmeans5 == 2)
cluster3 <- subset(kw_with_cluster, subset=kmeans5 == 3)
cluster4 <- subset(kw_with_cluster, subset=kmeans5 == 4)
cluster5 <- subset(kw_with_cluster, subset=kmeans5 == 5)

plot(kw_with_cluster, col = kmeans5$cluster)


# Instead of randomly choosing k, we test to see which is the best

#accumulator for cost results
cost_df <- data.frame()

#run kmeans for all clusters up to 100
for(i in 1:100){
    #Run kmeans for each level of i, allowing up to 100 iterations for convergence
    kmeans<- kmeans(x=doc_matrix, centers=i, iter.max=100)
    
    #Combine cluster number and cost together, write to df
    cost_df<- rbind(cost_df, cbind(i, kmeans$tot.withinss))
    
}
names(cost_df) <- c("cluster", "cost")

#Cost plot
ggplot(data=cost_df, aes(x=cluster, y=cost, group=1)) + 
    theme_bw(base_family="Garamond") + 
    geom_line(colour = "darkgreen") +
    theme(text = element_text(size=20)) +
    ggtitle("Reduction In Cost For Values of 'k'\n") +
    xlab("\nClusters") + 
    ylab("Within-Cluster Sum of Squares\n") +
    scale_x_continuous(breaks=seq(from=0, to=100, by= 10))


#state what is training set and what is test
trainingsize = 0.8*nrow(subset_nonPerks)
container <- create_container(doc_matrix, subset_nonPerks$successful, trainSize=1:trainingsize,testSize=(trainingsize+1):nrow(subset_nonPerks), virgin=FALSE)
SVM = train_model(container, "SVM")
GLMNET <- train_model(container,"GLMNET")
MAXENT <- train_model(container,"MAXENT")
BOOSTING <- train_model(container,"BOOSTING")
BAGGING <- train_model(container,"BAGGING")
RF <- train_model(container,"RF")
#NNET <- train_model(container,"NNET")


SVM_CLASSIFY <- classify_model(container, SVM)
GLMNET_CLASSIFY <- classify_model(container, GLMNET)
MAXENT_CLASSIFY <- classify_model(container, MAXENT)
BOOSTING_CLASSIFY <- classify_model(container, BOOSTING)
RF_CLASSIFY <- classify_model(container, RF)
#NNET_CLASSIFY <- classify_model(container, NNET)

analytics <- create_analytics(container, cbind(SVM_CLASSIFY, RF_CLASSIFY, BOOSTING_CLASSIFY, MAXENT_CLASSIFY))
summary(analytics)

# CREATE THE data.frame SUMMARIES
topic_summary <- analytics@label_summary
alg_summary <- analytics@algorithm_summary
ens_summary <-analytics@ensemble_summary
doc_summary <- analytics@document_summary

rm(nonPerks)