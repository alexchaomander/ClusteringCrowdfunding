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