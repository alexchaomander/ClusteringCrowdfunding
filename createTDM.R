#Function to create a Term Document Matrix

createTDM = function(corpus, .progress='none') {
    require(tm)
    require(plyr)
    progress.bar = create_progress_bar(.progress)
    progress.bar$init(8)
    doc.vec = VectorSource(corpus)
    progress.bar$step()
    doc.corpus = Corpus(doc.vec)
    progress.bar$step()
    doc.corpus <- tm_map(doc.corpus, content_transformer(tolower))
    progress.bar$step()
    doc.corpus <- tm_map(doc.corpus, removePunctuation)
    progress.bar$step()
    doc.corpus <- tm_map(doc.corpus, removeNumbers)
    progress.bar$step()
    doc.corpus <- tm_map(doc.corpus, removeWords, stopwords("english"))
    progress.bar$step()
    require(SnowballC)
    doc.corpus = tm_map(doc.corpus, stemDocument)
    progress.bar$step()
    TDM = TermDocumentMatrix(doc.corpus)
    progress.bar$step()
    return(TDM)
}


# #Each of these words occurred more that 2000 times.
# findFreqTerms(TDM, 5000)
# 
# #From our first look at the TDM we know that there are many terms which
# #do not occur very often. It might make sense to simply remove these sparse terms
# #from the analysis.
# 
# 
# # A term-document matrix where those terms from x are removed which have at least
# # a sparse percentage of empty (i.e., terms occurring 0 times in a document) elements.
# # I.e., the resulting matrix contains only terms with a sparse factor of less than sparse.
# 
# TDM.common = removeSparseTerms(TDM, sparse = 0.99)
# inspect(TDM.common[1:100, 1:10])
# 
# require(reshape2)
# TDM.common = as.matrix(TDM.common)
# TDM.common = melt(TDM.common, value.name = "count")
# 
# require(ggplot2)
# 
# ggplot(TDM.common, aes(x = Docs, y = Terms, fill = log(count))) +
#     geom_tile(color = "white") +
#     scale_fill_gradient(high="#FF0000" , low="#FFFFFF") +
#     ylab("") +
#     theme(panel.background = element_blank()) +
#     theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())


