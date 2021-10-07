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

test <- a[1:1000]
s <- split_punct(test)
s[500:600]

ss <- split_punct(s)
ss[500:600]
