setwd("~/Desktop/shared-chao")

require(sqldf)
require("RTextTools")
source("sentiment.R")

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

successful_nonPerks_comment_score = 
    score.sentiment(successful_nonPerks_comments$comment, hu.liu.pos, hu.liu.neg, .progress="text")



# Comments of unsuccessful busTechPerks
unsuccessful_nonPerks_comments = sqldf("select * from comments inner join 
                                       (select campid from unsuccessful_nonPerks) as a
                                       on comments.campid = a.campid")
unsuccessful_nonPerks_comments = unsuccessful_nonPerks_comments[,-9]
unsuccessful_nonPerks_comments = unsuccessful_nonPerks_comments[!duplicated(unsuccessful_nonPerks_comments),]
unsuccessful_nonPerks_comment_score = 
    score.sentiment(unsuccessful_nonPerks_comments$comment, hu.liu.pos, hu.liu.neg, .progress="text")


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