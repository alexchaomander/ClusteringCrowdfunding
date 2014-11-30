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
successful_smallPerks_comment_score = 
    score.sentiment(successful_smallPerks_comments$comment, hu.liu.pos, hu.liu.neg, .progress="text")


#Comments of unsucessful smallPerks
unsuccessful_smallPerks_comments = sqldf("select * from comments inner join 
                                         (select campid from unsuccessful_smallPerks) as a
                                         on comments.campid = a.campid")
unsuccessful_smallPerks_comments = unsuccessful_smallPerks_comments[,-9]
unsuccessful_smallPerks_comments = unsuccessful_smallPerks_comments[!duplicated(unsuccessful_smallPerks_comments),]
unsuccessful_smallPerks_comment_score = 
    score.sentiment(unsuccessful_smallPerks_comments$comment, hu.liu.pos, hu.liu.neg, .progress="text")



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