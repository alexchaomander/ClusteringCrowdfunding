# trained classifier for perks
# May 5, 2014

setwd("C:/Dropbox/COLLEAGUES/crowdfunding_LN_ADG/gender paper/data/data july/btechperk")
library("RTextTools")

console<-file("training_classifierlog.log")

perk<-read.csv("random_perks_coded.txt", sep = "\t", header = TRUE)

dim(perk)
head(perk)

# CREATE THE DOCUMENT-TERM MATRIX
doc_matrix <- create_matrix(perk$perk_descr, language="english", removeNumbers=TRUE,stemWords=TRUE, removeSparseTerms=.999)
doc_matrix = create_matrix(cleanBusTechPerks$perk_descr, language="english", removeNumbers=TRUE, stemWords=TRUE, removeSparseTerms=0.999)


#state what is training set and what is test
container <- create_container(doc_matrix, perk$code, trainSize=1:150,testSize=151:223, virgin=FALSE)
# sample(nrow(doc_matrix), .75*nrow(doc_matrix), replace = FALSE, prob = NULL)

# TRYING 3 out of 9 training algorithms. "ensemble" coding - testing correlation between the three training algs
SVM <- train_model(container,"SVM")
GLMNET <- train_model(container,"GLMNET")
MAXENT <- train_model(container,"MAXENT")
BOOSTING <- train_model(container,"BOOSTING")
BAGGING <- train_model(container,"BAGGING")
RF <- train_model(container,"RF")
NNET <- train_model(container,"NNET")

# classify the test data using the trained model
SVM_CLASSIFY <- classify_model(container, SVM)
GLMNET_CLASSIFY <- classify_model(container, GLMNET)
MAXENT_CLASSIFY <- classify_model(container, MAXENT)
BOOSTING_CLASSIFY <- classify_model(container, BOOSTING)
RF_CLASSIFY <- classify_model(container, RF)
NNET_CLASSIFY <- classify_model(container, NNET)



analytics <- create_analytics(container, cbind(SVM_CLASSIFY, NNET_CLASSIFY, RF_CLASSIFY,  BOOSTING_CLASSIFY, MAXENT_CLASSIFY))
summary(analytics)

# CREATE THE data.frame SUMMARIES
topic_summary <- analytics@label_summary
alg_summary <- analytics@algorithm_summary
ens_summary <-analytics@ensemble_summary
doc_summary <- analytics@document_summary

# coverage and recall. ADG needs to plot where max point?
create_ensembleSummary(analytics@document_summary)


SVM <- cross_validate(container, 4, "SVM")
GLMNET <- cross_validate(container, 4, "GLMNET")
MAXENT <- cross_validate(container, 4, "MAXENT")
BOOSTING <- cross_validate(container, 4, "BOOSTING")
RF <- cross_validate(container, 4, "RF")
NNET <- cross_validate(container, 4, "NNET")


write.csv(analytics@document_summary, "AllUpdates_labels_trained.csv")

#  perk[which(MAXENT_CLASSIFY<.25),3] - list comments that are not well classified? Not working yet.

## google work around for problem with GLMNET
ResortDtm <- function(working.dtm) {
  # sorts a sparse matrix in triplet format (i,j,v) first by i, then by j.
  # Args:
  #   working.dtm: a sparse matrix in i,j,v format using $i $j and $v respectively. Any other variables that may exist in the sparse matrix are not operated on, and will be returned as-is.
  # Returns:
  #   A sparse matrix sorted by i, then by j.
  working.df <- data.frame(i = working.dtm$i, j = working.dtm$j, v = working.dtm$v)  # create a data frame comprised of i,j,v values from the sparse matrix passed in.
  working.df <- working.df[order(working.df$i, working.df$j), ] # sort the data frame first by i, then by j.
  working.dtm$i <- working.df$i  # reassign the sparse matrix' i values with the i values from the sorted data frame.
  working.dtm$j <- working.df$j  # ditto for j values.
  working.dtm$v <- working.df$v  # ditto for v values.
  return(working.dtm) # pass back the (now sorted) data frame.
}  # end function

combined.matrix <- create_matrix(x, language = "english", stemWords = FALSE, ngramLength = 1, removeNumbers = TRUE, removePunctuation = TRUE, stripWhitespace = TRUE, toLower = TRUE, removeStopwords = TRUE)
combined.matrix <- ResortDtm(combined.matrix)
container <- create_container(combined.matrix, response.variable, trainSize = 1:nrow(cases.subset), testSize = NULL, virgin = FALSE) 
output.models <- train_models(container, algorithms = c("GLMNET")) 

 1 goodwishes
 2 thanks
 3 answer
 4 neutral
 5 evaluation
 6 askdetails

 
 announcements
 1. media
 2. delay
 3. info
 4. work
 5. ad
 6. perk
 7. thanks