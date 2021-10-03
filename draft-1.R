getwd()
setwd("D:/Edinburgh/Semester 1/Statistical Programming/SP-ws1-g2-practice1")
a <- scan("1581-0.txt",what="character",skip=156)
#ab <- scan("1581-1.txt", what="character", skip=156, fileEncoding = "UTF-8")
n <- length(a)
a <- a[-((n-2909):n)]


split_punct <- function(test){
  punc <- c(",", ":", ".", ";", "!", "?")
  for (i in 1:length(punc)) {
    p <- punc[i] 
    ipunc <- grep(p, test, fixed = T) ## search for words containing this mark
    npunc <- length(ipunc) ## number of words containing this punctuation
    if (npunc == 0) next ## if all words don't contain this punctuation, skip to next loop
    spunc <- rep("", length(test) + npunc) ## create a new vectore to store the splited words
    sii <- ipunc + 1:npunc ## vector to store the position of this punctuations
    spunc[sii] <- substr(test[ipunc], nchar(test[ipunc]), nchar(test[ipunc])) ## insert the punctuations
    spunc[sii-1] <- substr(test[ipunc], 1, nchar(test[ipunc]) - 1) ## insert words the punctuations attacthed to
    spunc[-c(sii, sii-1)] <- test[-ipunc] ## insert other words
    test <- spunc ## update words vector
  }
  splitted <- test[test != ""] ## delete "" in case of error in following operation
  return(splitted)
}

splitted_words <- split_punct(a)
splitted_words <- tolower(splitted_words) ## replaced capital letters
uni_words <- unique(splitted_words) ## a vector of unique words
uid <- match(splitted_words, uni_words) ## vector of indicies indicating which element in the unique word vector each element in the bible text corresponds to
tab <- tabulate(uid) ## count up how many times each unique word occurs in the text
freq <- sort(tab, decreasing = T) ## sort the times each word occurs in decreasing order
m <- 1000 ## the number of words that will be used to train the model
threshold <- freq[m] ## search for the threshold number of the m most common words, but may require further adjustment according to the result of next step
b <- uni_words[which(tab >= threshold)] ## select the m most common words to create vector b
