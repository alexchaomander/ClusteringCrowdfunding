#Find frequencies for n-grams. Returns a CSV file with a column of grams and their frequencies
setwd("~/Desktop/CSR_text")
source("~/Desktop/ClusteringCrowdfunding/createTDM.R")
allFiles = list.files("corpus/")

gramFrequencyCreator = function(filePath, ngrams) {
  f = paste("corpus/", filePath, sep="")
  a = readChar(f, file.info(f)$size)
  corpus = data.frame(Words = a, stringsAsFactors = F)
  TDM = createTDM(corpus$Words, "text", ngrams)
  m = as.matrix(TDM)
  v = sort(rowSums(m),decreasing=TRUE)
  d = data.frame(Gram = names(v),Frequency=v)
  row.names(d) = NULL
  return(d)
}

#### To output the files to the current directory.
### Can move files using unix comamnd: mv *.csv directoryName/ 

# for (i in 1:length(allFiles)) {
#   for (j in 1:2) {
#     fileName = paste(allFiles[i], "_", j, "-Gram", "_", "Frequency.csv", sep = "")
#     write.csv(gramFrequencyCreator(allFiles[i], j), file = fileName)
#   }
# }