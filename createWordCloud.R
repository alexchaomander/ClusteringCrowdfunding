# Function to create a word cloud
### corpus is a vector of strings
### .progress can be "text" to create a progress bar
### ngrams is the number of grams you wish to look at (monograms, bigrams, etc.)

createWordCloud = function(corpus, .progress='none', ngrams) {
  require(wordcloud)
  require(tm)
  require(plyr)
  progress.bar = create_progress_bar(.progress)
  progress.bar$init(7)
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
  
  
  tokenizer = function(x) NGramTokenizer(x, Weka_control(min = ngrams, max = ngrams))
  TDM = TermDocumentMatrix(doc.corpus, control = list(tokenize = tokenizer))
  progress.bar$step()
  
  m = as.matrix(TDM)
  v = sort(rowSums(m),decreasing=TRUE)
  d = data.frame(word = names(v),freq=v)
  
  cloud = wordcloud(words = d$word,
                    freq = d$freq,
                    scale=c(5,0.5),
                    max.words=100,
                    random.order=FALSE,
                    rot.per=0.35,
                    use.r.layout=FALSE,
                    colors=brewer.pal(8, "Dark2"))
  return(cloud)
}