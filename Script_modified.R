getwd()
setwd("~/SP-ws1-g2-practice1")
a <- scan("1581-0.txt", what = "character", skip = 156)
n <- length(a)
a <- a[- ((n - 2909):n)] ## strip license


split_punct <- function(test){
  punc <- c(",", ":", ".", ";", "!", "?")
  for (i in 1:length(punc)) {
    p <- punc[i] 
    ipunc <- grep(p, test, fixed = T) ## search for words containing this mark
    npunc <- length(ipunc) ## number of words containing this punctuation
    if (npunc == 0) next ## if all words don't contain this punctuation, skip to next loop
    spunc <- rep("", length(test) + npunc) ## create a new vectore to store the splited words
    sii <- ipunc + 1:npunc ## vector to store the position of this punctuations
    spunc[sii] <- p ## insert the punctuations
    spunc[sii-1] <- gsub(p, "", test[ipunc], fixed = TRUE) ## insert words the punctuations attacthed to
    spunc[-c(sii, sii-1)] <- test[-ipunc] ## insert other words
    test <- spunc ## update words vector
  }
  splitted <- test[test != ""] ## delete "" in case of error in following operation
  return(splitted)
}

##split_punct(as.character(c("A....B", "1:1.", "A??,an!"))) test example
##split_punct(a[1:100])

splitted_words <- split_punct(a)
splitted_words <- tolower(splitted_words) ## replaced capital letters
uni_words <- unique(splitted_words) ## a vector of unique words
uid <- match(splitted_words, uni_words) ## vector of indicies indicating which element in the unique word vector each element in the bible text corresponds to
tab <- tabulate(uid) ## count up how many times each unique word occurs in the text
freq <- sort(tab, decreasing = T) ## sort the times each word occurs in decreasing order
m <- 1000 ## the number of words that will be used to train the model
threshold <- freq[m] ## search for the threshold number of the m most common words, but may require further adjustment according to the result of next step
b <- uni_words[which(tab >= threshold)] ## select the m most common words to create vector b

mat_word <- match(splitted_words, b) ## a vector giving which element of  vector, b, each element of the full text vector corresponds to
prior <- mat_word[1:(length(mat_word) - 1)] ## the first column, which is the index for common words
following <- mat_word[2:length(mat_word)] ## the second column, which gives the following words
pair_words <- cbind(prior, following) ## a matrix where each row is a pair of common words
pair_words <- pair_words[- which(is.na(rowSums(pair_words))),] ## delete rows having NA
A <- matrix(0, length(b), length(b)) ## a matrix where A[i,j] means the jth word in the common words follows the ith word
for (k in 1:dim(pair_words)[1]) {
  i = pair_words[k, 1]
  j = pair_words[k, 2]
  A[i,j] = A[i,j] + 1
} ## caculate how many times the ith common word is followed by the jth common word
for (k in 1:length(b)) {
  A[k, ] <- A[k, ]/sum(A[k, ])
} ## standardize each row of A, so that A[i,j] can be interpreted as probablity that b[j] will follow b[i]

prior_word <- sample(b, size = 1)  ##  starting from a randomly selected entry in vector b
num_words <- 50 ## how many words the model will simulate
sentence <- rep("", num_words) ## a vector to store words in the sentence
sentence[1] <- prior_word
for (i in 2:num_words) {
  ipwords <- match(prior_word, b) ## find the position of the word in vector b
  pr <- A[ipwords, ] ## the probabilities of each word in b following this word
  sentence[i] <- b[sample(length(b), size = 1, prob = pr)] ## select one word according to the probablities
  prior_word <- sentence[i] ## update the prior word
}

cat(sentence) ## print out the text
