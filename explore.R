setwd("~/Desktop/shared-chao")

require(sqldf)
library(AppliedPredictiveModeling)
require(tm)

### For 'bustechPerks' dataset #############################################################
bustechPerks = read.csv("data/bus_tech_perks_full.txt", sep="\t")
success = read.csv("data/campaign_success.txt", sep = "\t")
comments = read.csv("data/campaign_comments.txt", sep="\t")

cleanBusTechPerks = sqldf("select * from bustechPerks inner join success on bustechPerks.campid = success.campid")
cleanBusTechPerks = cleanBusTechPerks[duplicated(cleanBusTechPerks$campid),]
cleanBusTechPerks = cleanBusTechPerks[,-7]
cleanBusTechPerks$differential = cleanBusTechPerks$money_raised - cleanBusTechPerks$campaign_goal

successful_busTechPerks = cleanBusTechPerks[which(cleanBusTechPerks$successful == 1),]
unsuccessful_busTechPerks = cleanBusTechPerks[which(cleanBusTechPerks$successful != 1),]

for (i in seq(1:length(cleanBusTechPerks))) {
    cleanBusTechPerks[which(cleanBusTechPerks[i] == ""), i] = NA
}

#Percentage of successful business
nrow(successful_busTechPerks)/(nrow(unsuccessful_busTechPerks) + nrow(successful_busTechPerks))

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

# Comments of successful busTechPerks 
successful_Community_comments = sqldf("select * from comments inner join 
                                         (select campid from successful_Community) as a
                                         on comments.campid = a.campid")
successful_Community_comments = successful_Community_comments[,-9]
successful_Community_comments = successful_Community_comments[!duplicated(successful_Community_comments),]

# Comments of unsuccessful busTechPerks
unsuccessful_Community_comments = sqldf("select * from comments inner join 
                                         (select campid from unsuccessful_Community) as a
                                         on comments.campid = a.campid")
unsuccessful_Community_comments = unsuccessful_Community_comments[,-9]
unsuccessful_Community_comments = unsuccessful_Community_comments[!duplicated(unsuccessful_Community_comments),]

#Percentage of successful community
nrow(successful_Community)/(nrow(unsuccessful_Community) + nrow(successful_Community))

rm(community)
#######################################################################################

### For nonPerks Dataset ###
nonPerks = read.csv("data/nonTech nonBus nonCommunity perks.txt", sep="\t")

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

rm(nonPerks)

#######################################################################################

### For smallPerks dataset ###
smallPerks = read.csv("data/Tech and Small Business perks.txt", sep="\t")

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

nrow(successful_smallPerks)/(nrow(unsuccessful_smallPerks) + nrow(successful_smallPerks))

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


library("RTextTools")

# CREATE THE DOCUMENT-TERM MATRIX
doc_matrix = create_matrix(cleanBusTechPerks$perk_descr, language="english", removeNumbers=TRUE, stemWords=TRUE, removeSparseTerms=0.999)


container <- create_container(doc_matrix, perk$code, trainSize=1:150,testSize=151:223, virgin=FALSE)






