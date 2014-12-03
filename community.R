setwd("~/Desktop/shared-chao")

require(sqldf)
require("RTextTools")
source("sentiment.R")
source("createTDM.R")
source("createWordCloud.R")

####################### For 'community' dataset ###################################
community = read.csv("data/Community perks.txt", sep="\t")
success = read.csv("data/campaign_success.txt", sep = "\t")
comments = read.csv("data/campaign_comments.txt", sep="\t")

community = sqldf("select * from community inner join success on community.campid = success.campid")
community = sqldf("select * from community order by community.category")
community = community[,-13]
community$differential = community$money_raised - community$campaign_goal

for (i in seq(1:length(community))) {
    community[which(community[i] == ""), i] = NA
}
####################### For the entire dataset #######################################
require(reshape2)

community_TDM = createTDM(community$perk_descr, "text")
#Words that are in at least 2000 documents (descriptions)
findFreqTerms(community_TDM, lowfreq = 2000)
findFreqTerms(community_TDM, lowfreq = 5000)

TDM.common = removeSparseTerms(community_TDM, sparse = 0.95)
inspect(TDM.common[1:dim(TDM.common)[1], 1:10])

TDM.common = as.matrix(TDM.common)
plot(hclust(dist(TDM.common)), xlab="Words" , main = "Cluster Dendogram for Community Perk Descriptions")


TDM.common = melt(TDM.common, value.name = "count")
temp = TDM.common[1:2400,]

table(TDM.common$Terms, TDM.common$count)

require(ggplot2)

ggplot(temp, aes(x = Docs, y = Terms, fill = count)) +
    geom_tile(color = "white") +
    scale_fill_gradient(high="#FF0000" , low="#FFFFFF") +
    ylab("") +
    xlab("Campaigns") +
    ggtitle("Word Frequency Matrix For Community Campaign Descriptions") + 
    theme(panel.background = element_blank()) +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

### For perk titles
community_TDM = createTDM(community$perk_titl, "text")
findFreqTerms(community_TDM, lowfreq = 1000)

TDM.common = removeSparseTerms(community_TDM, sparse = 0.99)
inspect(TDM.common[1:dim(TDM.common)[1], 1:10])

TDM.common = as.matrix(TDM.common)
plot(hclust(dist(TDM.common)), xlab="Words" , main = "Cluster Dendogram for Community Perk Titles")

TDM.common = melt(TDM.common, value.name = "count")

table(TDM.common$Terms, TDM.common$count)

temp = TDM.common[1:16000,]

ggplot(temp, aes(x = Docs, y = Terms, fill = count)) +
    geom_tile(color = "white") +
    scale_fill_gradient(high="#FF0000" , low="#FFFFFF") +
    ylab("") +
    xlab("Campaigns") +
    ggtitle("Word Frequency Matrix For Campaign Titles") + 
    theme(panel.background = element_blank()) +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

####################### For the successful campaigns #######################################
successful_Community = community[which(community$successful == 1),]

successful_Community_TDM = createTDM(successful_Community$perk_descr, "text")
#Words that are in at least 2000 documents (descriptions)
findFreqTerms(successful_Community_TDM, lowfreq = 1000)

TDM.common = removeSparseTerms(successful_Community_TDM, sparse = 0.94)
numTerms = dim(TDM.common)[1]
inspect(TDM.common[1:numTerms, 1:10])

TDM.common = as.matrix(TDM.common)
# Cluster Dendogram of Successful CommunityWords Perk Descriptions
plot(hclust(dist(TDM.common)), xlab="Words" , main = "Cluster Dendogram for Successful Community Perk Descriptions")


TDM.common = melt(TDM.common, value.name = "count")
temp = TDM.common[1:6000,]

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

# Create a word cloud for CommunityPerk descriptions
createWordCloud(successful_Community$perk_descr, "text")


### For CommunityPerk perk titles
successful_Community_TDM = createTDM(successful_Community$perk_titl, "text")
#Words that are in at least 2000 documents (descriptions)
findFreqTerms(successful_Community_TDM, lowfreq = 100)

TDM.common = removeSparseTerms(successful_Community_TDM, sparse = 0.99)
numTerms = dim(TDM.common)[1]
inspect(TDM.common[1:numTerms, 1:10])

TDM.common = as.matrix(TDM.common)

# Cluster Dendogram of Successful CommunityWords Perk Titles
plot(hclust(dist(TDM.common)), xlab="Words", main = "Cluster Dendogram for Successful Community Perk Titles")

TDM.common = melt(TDM.common, value.name = "count")
temp = TDM.common[1:17000,]

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

#Create word cloud for successful Community titles
createWordCloud(successful_Community$perk_titl, "text")


####################### Comments of successful Community ##################################

successful_Community_comments = sqldf("select * from comments inner join 
                                      (select campid from successful_Community) as a
                                      on comments.campid = a.campid")
successful_Community_comments = successful_Community_comments[,-9]
successful_Community_comments = successful_Community_comments[!duplicated(successful_Community_comments),]

successful_Community_comment_score = 
    score.sentiment(successful_Community_comments$comment, hu.liu.pos, hu.liu.neg, .progress="text")

successful_Community_sentiment_counts = table(successful_Community_comment_score$sentiment)

bp = barplot(successful_Community_sentiment_counts[c(4,1,2,3,5)]
             ,xlab = "sentiment", ylab = "frequency", main = "Frequency of Sentiment Scores for Successful Community Perks",
             col = "blue", ylim = c(0, 25000),
             args.legend = list(title = "Sentiment Score", x = "topleft", cex = .7))
text(bp, 0, round(successful_Community_sentiment_counts[c(4,1,2,3,5)], 1),cex=1,pos=3)


successful_Community_comment_score$campid = successful_Community_comments$campid
successful_Community_comment_score$commentdate = successful_Community_comments$commentdate

time = read.csv("date.txt", sep=",")
colnames(time) = c("timestamp", "date")

successful_Community_comment_score = sqldf("select * from successful_Community_comment_score inner join time
                                              on time.timestamp = successful_Community_comment_score.commentdate")
successful_Community_comment_score = successful_Community_comment_score[-6]
successful_Community_comment_score$standardDate = lapply(successful_Community_comment_score$date, function(date) {
    result = as.Date(date, format = "%B %Y")
})

a = table(successful_Community_comment_score$sentiment, sort(successful_Community_comment_score$date))
#Ideally will try to take care of sorting but it is a pain in R
a
plot(a, las=2, col = "blue", main = "Sentiment Over Time for Successful Campaigns" , ylab="Date",xlab = "Sentiment")

# Word cloud for comments left on successful campaigns
createWordCloud(successful_Community_comments$comment, "text")

########### TDM ##########

successful_Community_comments_TDM = createTDM(successful_Community_comments$comment, "text")
#Words that are in at least 2000 documents (descriptions)
findFreqTerms(successful_Community_comments_TDM, lowfreq = 1000)

TDM.common = removeSparseTerms(successful_Community_comments_TDM, sparse = 0.95)
numTerms = dim(TDM.common)[1]
inspect(TDM.common[1:numTerms, 1:10])

TDM.common = as.matrix(TDM.common)
TDM.common = melt(TDM.common, value.name = "count")
temp = TDM.common[1:4500,]

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


####################### For the unsuccessful campaigns #######################################
unsuccessful_Community = community[which(community$successful != 1),]

unsuccessful_Community_TDM = createTDM(unsuccessful_Community$perk_descr, "text")
#Words that are in at least 2000 documents (descriptions)
findFreqTerms(unsuccessful_Community_TDM, lowfreq = 1500)

TDM.common = removeSparseTerms(unsuccessful_Community_TDM, sparse = 0.95)
numTerms = dim(TDM.common)[1]
inspect(TDM.common[1:numTerms, 1:10])

TDM.common = as.matrix(TDM.common)

# Cluster Dendogram for Unsuccessful CommunityPerk Titles
plot(hclust(dist(TDM.common)), xlab="Words", main = "Cluster Dendogram for Unsuccessful Community Perk Descriptions")

TDM.common = melt(TDM.common, value.name = "count")
temp = TDM.common[1:4600,]

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

# Word cloud for unsucessful campaign perk descriptions
createWordCloud(unsuccessful_Community$perk_descr, "text")


######## TDM ##########

unsuccessful_Community_TDM = createTDM(unsuccessful_Community$perk_titl, "text")
#Words that are in at least 1000 documents (descriptions)
findFreqTerms(unsuccessful_Community_TDM, lowfreq = 500)

TDM.common = removeSparseTerms(unsuccessful_Community_TDM, sparse = 0.99)
numTerms = dim(TDM.common)[1]
inspect(TDM.common[1:numTerms, 1:10])

TDM.common = as.matrix(TDM.common)

#Cluster Dendogram for Unsuccessful Community Perk Titles
plot(hclust(dist(TDM.common)), xlab="Words", main = "Cluster Dendogram for Unsuccessful Community Perk Titles")

TDM.common = melt(TDM.common, value.name = "count")
temp = TDM.common[1:3900,]

table(TDM.common$Terms, TDM.common$count)

require(ggplot2)

ggplot(temp, aes(x = Docs, y = Terms, fill = count)) +
    geom_tile(color = "white") +
    scale_fill_gradient(high="#FF0000" , low="#FFFFFF") +
    ylab("") +
    xlab("Campaigns") +
    ggtitle("Word Frequency Matrix For Unsuccessful Campaign Titles") + 
    theme(panel.background = element_blank()) +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

# Word cloud for unsucessful campaign perk titles
createWordCloud(unsuccessful_Community$perk_descr, "text")


#### Comments of unsuccessful community perks ###
unsuccessful_Community_comments = sqldf("select * from comments inner join 
                                        (select campid from unsuccessful_Community) as a
                                        on comments.campid = a.campid")
unsuccessful_Community_comments = unsuccessful_Community_comments[,-9]
unsuccessful_Community_comments = unsuccessful_Community_comments[!duplicated(unsuccessful_Community_comments),]

unsuccessful_Community_comment_score = 
    score.sentiment(unsuccessful_Community_comments$comment, hu.liu.pos, hu.liu.neg, .progress="text")

unsuccessful_Community_sentiment_counts = table(unsuccessful_Community_comment_score$sentiment)

bp = barplot(unsuccessful_Community_sentiment_counts[c(4,1,2,3,5)]
             ,xlab = "sentiment", ylab = "frequency", main = "Sentiment Scores for Unsuccessful Community Comments",
             col = "red", ylim = c(0, 25000),
             args.legend = list(title = "Sentiment Score", x = "topleft", cex = .7))
text(bp, 0, round(unsuccessful_Community_sentiment_counts[c(4,1,2,3,5)], 1),cex=1,pos=3)

unsuccessful_Community_comment_score$campid = unsuccessful_Community_comments$campid
unsuccessful_Community_comment_score$commentdate = unsuccessful_Community_comments$commentdate

time = read.csv("date.txt", sep=",")
colnames(time) = c("timestamp", "date")

unsuccessful_Community_comment_score = sqldf("select * from unsuccessful_Community_comment_score inner join time
                                                on time.timestamp = unsuccessful_Community_comment_score.commentdate")
unsuccessful_Community_comment_score = unsuccessful_Community_comment_score[-6]
unsuccessful_Community_comment_score$standardDate = lapply(unsuccessful_Community_comment_score$date, function(date) {
    result = as.Date(date, format = "%B %Y")
})

a = table(unsuccessful_Community_comment_score$sentiment, sort(unsuccessful_Community_comment_score$date))
#Ideally will try to take care of sorting but it is a pain in R
a
plot(a, las=2, col = "red", main = "Sentiment over time for Unsuccessful Campaigns" , ylab="Sentiment",xlab = "Date")

# Word cloud for comments left on unsuccessful campaigns
createWordCloud(unsuccessful_Community_comments$comment, "text")

########### TDM ##########

unsuccessful_Community_comments_TDM = createTDM(unsuccessful_Community_comments$comment, "text")
#Words that are in at least 2000 documents (descriptions)
findFreqTerms(unsuccessful_Community_comments_TDM, lowfreq = 1000)

TDM.common = removeSparseTerms(unsuccessful_Community_comments_TDM, sparse = 0.95)
numTerms = dim(TDM.common)[1]
inspect(TDM.common[1:numTerms, 1:10])

TDM.common = as.matrix(TDM.common)
TDM.common = melt(TDM.common, value.name = "count")
temp = TDM.common[1:2000,]

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

#Percentage of successful community
nrow(successful_Community)/(nrow(unsuccessful_Community) + nrow(successful_Community))

#Number of unique successful community campaigns
length(unique(successful_Community$campid))
unique_successful_Community_campid = unique(successful_Community$campid)
subset_successful_Community_campid = sample(unique_successful_Community_campid, size = 50, replace = FALSE)


#Number of unique successful business campaigns
length(unique(unsuccessful_Community$campid))
unique_unsuccessful_Community_campid = unique(unsuccessful_Community$campid)
subset_unsuccessful_Community_campid = sample(unique_unsuccessful_Community_campid, size = 100, replace = FALSE)

subset = c(subset_successful_Community_campid, subset_unsuccessful_Community_campid)
subset = data.frame(campid = subset)

subset_Community = sqldf("select * from community inner join subset on
                         community.campid = subset.campid inner join success on community.campid = success.campid")
subset_Community = subset_Community[,c(-13,-14)]


# CREATE THE DOCUMENT-TERM MATRIX
doc_matrix = create_matrix(subset_Community$perk_descr, language="english", removeNumbers=TRUE, stemWords=TRUE, removeSparseTerms=0.999)

#Plotting k means clustering 
require(cluster)
kmeans4 <- kmeans(doc_matrix, 4)

#Merge cluster assignment back to keywords
kw_with_cluster <- as.data.frame(cbind(subset_Community$perk_descr,
                                       subset_Community$price, subset_Community$differential, kmeans4$cluster))
names(kw_with_cluster) <- c("keyword", "price", "differential", "kmeans4")

#Make df for each cluster result, quickly "eyeball" results
cluster1 <- subset(kw_with_cluster, subset=kmeans4 == 1)
cluster2 <- subset(kw_with_cluster, subset=kmeans4 == 2)
cluster3 <- subset(kw_with_cluster, subset=kmeans4 == 3)
cluster4 <- subset(kw_with_cluster, subset=kmeans4 == 4)

plot(kw_with_cluster, col = kmeans4$cluster)


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
trainingsize = 0.8*nrow(subset_Community)
container <- create_container(doc_matrix, subset_Community$successful, trainSize=1:trainingsize,testSize=(trainingsize+1):nrow(subset_Community), virgin=FALSE)
SVM = train_model(container, "SVM")
GLMNET <- train_model(container,"GLMNET")
MAXENT <- train_model(container,"MAXENT")
BOOSTING <- train_model(container,"BOOSTING")
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