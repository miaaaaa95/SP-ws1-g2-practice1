setwd("~/SP-ws1-g2-practice1/")
a <- scan("1581-0.txt", what = "character", skip = 156)
n <- length(a)
a <- a[- ((n - 2909):n)] ## strip license


split_punct_test <- function(test) {
  punc <- c(",", ":", ".", ";", "!", "?")
  for (i in seq_len(length(punc))) {
    p <- punc[i]
    ipunc <- grep(p, test, fixed = T) ## search for words containing this mark
    npunc <- length(ipunc) ## number of words containing this punctuation
    if (npunc == 0) next
    ## if all words don't contain this punctuation, skip to next loop
    ##count the number of punctuations of each word
    ipunc_count <- rep(0, npunc)
    for (j in seq_len(npunc)) {
        ipunc_count[j] <- length(gregexpr(p, test[ipunc[j]],
        fixed = TRUE)[[1]])
    }
    ## create a new vector to store the splited words
    spunc <- rep("", length(test) + sum(ipunc_count))
    ##inicate punctuations'position
    sii <- c((ipunc[1] + 1) : (ipunc[1] + ipunc_count[1]))
    splitted_words <- c(ipunc[1]) ##indicate splitted words'position
    if (npunc >= 2) {
    for (k in 2:npunc) {
      sii <- c(sii, (ipunc[k] + 1 + sum(ipunc_count[1 : (k - 1)]))
      :(ipunc[k] + sum(ipunc_count[1 : k]))) ##Iterate over the position of the punc
      splitted_words <-
      c(splitted_words, ipunc[k] + sum(ipunc_count[1 : (k - 1)]))##Iterate over the position of the splitted words
    }
    }
    ## insert the punctuations
    spunc[sii] <- p
    ## insert words the punctuations attacthed to
    spunc[splitted_words] <- gsub(p, "", test[ipunc], fixed = TRUE)
    spunc[-c(sii, splitted_words)] <- test[-ipunc] ## insert other words
    test <- spunc ## update words vector
  }
  ## delete "" in case of error in following operation
  splitted <- test[test != ""]
  return(splitted)
}


s = split_punct_test(a[500:600])
ss = split_punct_test(s)
s == ss


