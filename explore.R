setwd("~/Desktop/shared-chao")

require(sqldf)
require("RTextTools")
source("sentiment.R")

### For 'bustechPerks' dataset #############################################################
busTechPerks = read.csv("data/bus_tech_perks_full.txt", sep="\t")
success = read.csv("data/campaign_success.txt", sep = "\t")
comments = read.csv("data/campaign_comments.txt", sep="\t")

cleanBusTechPerks = sqldf("select * from busTechPerks inner join success on busTechPerks.campid = success.campid")
cleanBusTechPerks = cleanBusTechPerks[duplicated(cleanBusTechPerks$campid),]
cleanBusTechPerks = cleanBusTechPerks[,-7]
cleanBusTechPerks$differential = cleanBusTechPerks$money_raised - cleanBusTechPerks$campaign_goal

successful_busTechPerks = cleanBusTechPerks[which(cleanBusTechPerks$successful == 1),]
unsuccessful_busTechPerks = cleanBusTechPerks[which(cleanBusTechPerks$successful != 1),]

for (i in seq(1:length(cleanBusTechPerks))) {
    cleanBusTechPerks[which(cleanBusTechPerks[i] == ""), i] = NA
}

# Comments of successful busTechPerks
successful_busTechPerks_comments = sqldf("select * from comments inner join 
                                         (select campid from successful_busTechPerks) as a
                                         on comments.campid = a.campid")
successful_busTechPerks_comments = successful_busTechPerks_comments[,-9]
successful_busTechPerks_comments = successful_busTechPerks_comments[!duplicated(successful_busTechPerks_comments),]

# Comments of unsuccessful busTechPerks
unsuccessful_busTechPerks_comments = sqldf("select * from comments inner join 
                                         (select campid from unsuccessful_busTechPerks) as a
                                         on comments.campid = a.campid")
unsuccessful_busTechPerks_comments = unsuccessful_busTechPerks_comments[,-9]
unsuccessful_busTechPerks_comments = successful_busTechPerks_comments[!duplicated(successful_busTechPerks_comments),]

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


# CREATE THE DOCUMENT-TERM MATRIX
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

#######################################################################################

### For Comments Dataset ###

#After doing a quick sort of the data on indexno, there appears to be a lot of redundant data
# like the first 50000 comments or so have the same ID number, same number of comments, and same userID
#should I elimiate this redundant data?

#Identify which campaigns generated the most number of comments
#Calculate sentiment of comments. Which campaigns correspond to highest scored sentiment
#Each campaign is mapped to their own total number of comments 

# cleanComments = sqldf("select * from comments inner join success on comments.campid = success.campid")
# rm(comments)


#######################################################################################

### For Updates Dataset ###
# updates = read.csv("data/campaign_updates.txt", sep="\t")

#Not sure how I should use this?
# cleanUpdates = sqldf("select * from updates inner join success on updates.campid = success.campid")
# rm(updates)


#######################################################################################

### For Community Dataset ###
community = read.csv("data/Community perks.txt", sep="\t")
success = read.csv("data/campaign_success.txt", sep = "\t")
comments = read.csv("data/campaign_comments.txt", sep="\t")

#Can try to do same analysis for Community as the others
cleanCommunity = sqldf("select * from community inner join success on community.campid = success.campid")
cleanCommunity = sqldf("select * from cleanCommunity order by cleanCommunity.category")
cleanCommunity = cleanCommunity[,-13]
cleanCommunity$differential = cleanCommunity$money_raised - cleanCommunity$campaign_goal


for (i in seq(1:length(cleanCommunity))) {
    cleanCommunity[which(cleanCommunity[i] == ""), i] = NA
}

successful_Community = cleanCommunity[which(cleanCommunity$successful == 1),]
unsuccessful_Community = cleanCommunity[which(cleanCommunity$successful != 1),]

# Comments of successful community perks 
successful_Community_comments = sqldf("select * from comments inner join 
                                         (select campid from successful_Community) as a
                                         on comments.campid = a.campid")
successful_Community_comments = successful_Community_comments[,-9]
successful_Community_comments = successful_Community_comments[!duplicated(successful_Community_comments),]

# Comments of unsuccessful community perks
unsuccessful_Community_comments = sqldf("select * from comments inner join 
                                         (select campid from unsuccessful_Community) as a
                                         on comments.campid = a.campid")
unsuccessful_Community_comments = unsuccessful_Community_comments[,-9]
unsuccessful_Community_comments = unsuccessful_Community_comments[!duplicated(unsuccessful_Community_comments),]

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
start.time = Sys.time()
doc_matrix = create_matrix(subset_Community$perk_descr, language="english", removeNumbers=TRUE, stemWords=TRUE, removeSparseTerms=0.999)
end.time = Sys.time()
time.taken = end.time - start.time
time.taken

#state what is training set and what is test
trainingsize = 0.8*nrow(subset_Community)
container <- create_container(doc_matrix, subset_Community$successful, trainSize=1:trainingsize,testSize=(trainingsize+1):nrow(subset_Community), virgin=FALSE)
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

rm(community)
#######################################################################################

### For nonPerks Dataset ###
nonPerks = read.csv("data/nonTech nonBus nonCommunity perks.txt", sep="\t")
success = read.csv("data/campaign_success.txt", sep = "\t")
comments = read.csv("data/campaign_comments.txt", sep="\t")


#Again try the same analysis for nonPerks as the others
cleanNonPerks = sqldf("select * from nonPerks inner join success on nonPerks.campid = success.campid")
cleanNonPerks$perks_claimed = as.numeric(paste(cleanNonPerks$perks_claimed))
cleanNonPerks = sqldf("select * from cleanNonPerks order by cleanNonPerks.category")
cleanNonPerks = cleanNonPerks[,-13]
cleanNonPerks$differential = cleanNonPerks$money_raised - cleanNonPerks$campaign_goal

for (i in seq(1:length(cleanNonPerks))) {
    cleanNonPerks[which(cleanNonPerks[i] == ""), i] = NA
}

successful_nonPerks = cleanNonPerks[which(cleanNonPerks$successful == 1),]
unsuccessful_nonPerks = cleanNonPerks[which(cleanNonPerks$successful != 1),]

# Comments of successful busTechPerks 
successful_nonPerks_comments = sqldf("select * from comments inner join 
                                         (select campid from successful_nonPerks) as a
                                         on comments.campid = a.campid")
successful_nonPerks_comments = successful_nonPerks_comments[,-9]
successful_nonPerks_comments = successful_nonPerks_comments[!duplicated(successful_nonPerks_comments),]

# Comments of unsuccessful busTechPerks
unsuccessful_nonPerks_comments = sqldf("select * from comments inner join 
                                         (select campid from unsuccessful_nonPerks) as a
                                         on comments.campid = a.campid")
unsuccessful_nonPerks_comments = unsuccessful_nonPerks_comments[,-9]
unsuccessful_nonPerks_comments = unsuccessful_nonPerks_comments[!duplicated(unsuccessful_nonPerks_comments),]

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
start.time = Sys.time()
doc_matrix = create_matrix(subset_nonPerks$perk_descr, language="english", removeNumbers=TRUE, stemWords=TRUE, removeSparseTerms=0.999)
end.time = Sys.time()
time.taken = end.time - start.time
time.taken

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

#######################################################################################

### For smallPerks dataset ###
smallPerks = read.csv("data/Tech and Small Business perks.txt", sep="\t")
success = read.csv("data/campaign_success.txt", sep = "\t")
comments = read.csv("data/campaign_comments.txt", sep="\t")


#Apply same techniques to study that. Appears to have more missing data

cleanSmallPerks = sqldf("select * from smallPerks inner join success on smallPerks.campid = success.campid")
cleanSmallPerks$perks_claimed = as.numeric(paste(cleanSmallPerks$perks_claimed))
#can I regard NAs as 0?
cleanSmallPerks$differential = cleanSmallPerks$money_raised - cleanSmallPerks$campaign_goal

for (i in seq(1:length(cleanSmallPerks))) {
    cleanSmallPerks[which(cleanSmallPerks[i] == ""), i] = NA
}

successful_smallPerks = cleanSmallPerks[which(cleanSmallPerks$successful == 1),]
unsuccessful_smallPerks = cleanSmallPerks[which(cleanSmallPerks$successful != 1),]



# Comments of successful smallPerks 
successful_smallPerks_comments = sqldf("select * from comments inner join 
                                         (select campid from successful_smallPerks) as a
                                         on comments.campid = a.campid")
successful_smallPerks_comments = successful_smallPerks_comments[,-9]
successful_smallPerks_comments = successful_smallPerks_comments[!duplicated(successful_smallPerks_comments),]


#Comments of unsucessful smallPerks
unsuccessful_smallPerks_comments = sqldf("select * from comments inner join 
                                         (select campid from unsuccessful_smallPerks) as a
                                         on comments.campid = a.campid")
unsuccessful_smallPerks_comments = unsuccessful_smallPerks_comments[,-9]
unsuccessful_smallPerks_comments = unsuccessful_smallPerks_comments[!duplicated(unsuccessful_smallPerks_comments),]

nrow(successful_smallPerks)/(nrow(unsuccessful_smallPerks) + nrow(successful_smallPerks))

#Number of unique successful smallPerks campaigns
length(unique(successful_smallPerks$campid))
unique_successful_smallPerks_campid = unique(successful_smallPerks$campid)
subset_successful_smallPerks_campid = sample(unique_successful_smallPerks_campid, size = 50, replace = FALSE)


#Number of unique successful smallPerks campaigns
length(unique(unsuccessful_smallPerks$campid))
unique_unsuccessful_smallPerks_campid = unique(unsuccessful_smallPerks$campid)
subset_unsuccessful_smallPerks_campid = sample(unique_unsuccessful_smallPerks_campid, size = 100, replace = FALSE)

subset = c(subset_successful_smallPerks_campid, subset_unsuccessful_smallPerks_campid)
subset = data.frame(campid = subset)

subset_smallPerks = sqldf("select * from smallPerks inner join subset on
                        smallPerks.campid = subset.campid inner join success on smallPerks.campid = success.campid")
subset_smallPerks = subset_smallPerks[,c(-13,-14)]


# CREATE THE DOCUMENT-TERM MATRIX
start.time = Sys.time()
doc_matrix = create_matrix(subset_smallPerks$perk_descr, language="english", removeNumbers=TRUE, stemWords=TRUE, removeSparseTerms=0.999)
end.time = Sys.time()
time.taken = end.time - start.time
time.taken

#state what is training set and what is test
trainingsize = 0.8*nrow(subset_smallPerks)
container <- create_container(doc_matrix, subset_smallPerks$successful, trainSize=1:trainingsize,testSize=(trainingsize+1):nrow(subset_smallPerks), virgin=FALSE)
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

rm(smallPerks)
rm(success)

############################################################################################

#### IMPORTANT SUMMARY INFORMATION ####

#nrow(success) 
#191307-Number of unique campaigns that we have. Try to reduce data down to this number to eliminate duplicates

# length(unique(bustechPerks$campid)) #18852
# length(unique(comments$campid)) #72517
# length(unique(success$campid)) #191307
# length(unique(updates$campid)) #159610
# length(unique(nonPerks$campid)) #123538
# length(unique(community$campid)) #25131


# group by ones that raise the most money. 
# group by price
# group by number of units
# group by minimum donation


#money trying to raise vs success rate
#what you are actualy trying to raise money for
# Unsupervised clustering of words. Discarding common words. Seeing special words based on frequency
#Train a supervised model
# Extract features from data. 
# Some features more important than others
# Cluster on words
# Train some classifier based on feature you have
#amount trying to raise and amount they ended up raising
# amount raised vs actual donors /commented

#id for all words. sparse feature vector

#identify categories to look for based on words 

# SVMs, binary classifer, libSVM, 
# 

# far exceed goal or just made it
# per donor average contribution
# many donors, a lot less

# Find what kind of campaign it is
# correlation between perks and campaign success
# 


### Calculate summary statistics for campaigns
# Goal, duration, number of backers, final amount
# Categorize by successful, failed, all





