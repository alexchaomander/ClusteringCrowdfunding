setwd("~/Desktop/shared-chao")

require(sqldf)
require("RTextTools")
source("sentiment.R")

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