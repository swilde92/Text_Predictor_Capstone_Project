library(ggplot2)
library(dplyr)
library(tidyr)
library(ngram)
library(tm)
library(stringr)
library(stringi)
#*note that running with full files and .5 samples, .2 for test, took 7 hours and didnt make it half way
##########################################################################################
### Load data
##########################################################################################
# setwd("C:/Users/Sarah Lynn/Desktop/Self Study/Coursera DS JH - Capstone")
# wd_loc <- getwd()
# unzip(zipfile="Coursera-SwiftKey.zip")

# Start the clock!
ptm <- proc.time()

setwd("C:/Users/Sarah Lynn/Desktop/Self Study/Coursera DS JH - Capstone/final/en_US")

##open connection to twitter file
con <- file("en_US.twitter.txt", "r")
tot_lines_t <- length(readLines(con))
close(con) 
samp_size_t <- round(.5*tot_lines_t)  
con <- file("en_US.twitter.txt", "r")
sdata_t <- readLines(con,samp_size_t)   ##grabs ~1/3 of the data, 700k
close(con) 

##open connection to blog file
con <- file("en_US.blogs.txt", "r")
tot_lines_b <- length(readLines(con))
close(con) 
samp_size_b <- round(.5*tot_lines_b)
con <- file("en_US.blogs.txt", "r")
sdata_b <- readLines(con,samp_size_b)   ##grabs ~2/3 of the data, 630k
close(con) 

##open connection to news file
con <- file("en_US.news.txt", "r")
tot_lines_n <- length(readLines(con))
close(con) 
samp_size_n <- round(.5*tot_lines_n)  
con <- file("en_US.news.txt", "r")
sdata_n <- readLines(con,samp_size_n)   ##grabs ~90% of the data, 70k
close(con) 


###########################################################################################
##summary stats about the data
###########################################################################################
# # file sizes
# file.size("en_US.twitter.txt")/1000000 ###in megabytes
# file.size("en_US.news.txt")/1000000
# file.size("en_US.blogs.txt")/1000000
# # docs
# tot_lines_t
# tot_lines_b
# tot_lines_n
# #words
# sum(stri_count_words(sdata_t))
# sum(stri_count_words(sdata_b))
# sum(stri_count_words(sdata_n))
# #sentences
# sum(stri_count_boundaries(sdata_t, type='sentence'))
# sum(stri_count_boundaries(sdata_b, type='sentence'))
# sum(stri_count_boundaries(sdata_n, type='sentence'))
# ##words per sentence
# sum(stri_count_words(sdata_t))/sum(stri_count_boundaries(sdata_t, type='sentence'))
# sum(stri_count_words(sdata_b))/sum(stri_count_boundaries(sdata_b, type='sentence'))
# sum(stri_count_words(sdata_n))/sum(stri_count_boundaries(sdata_n, type='sentence'))
# ##median words per doc
# median(stri_count_words(sdata_t))
# median(stri_count_words(sdata_b))
# median(stri_count_words(sdata_n))
# ##median sentences per doc
# median(stri_count_boundaries(sdata_t, type='sentence'))
# median(stri_count_boundaries(sdata_b, type='sentence'))
# median(stri_count_boundaries(sdata_n, type='sentence'))

###########################################################################################
##test/sample subset of data
###########################################################################################
#################holding out a test set that will only be used once at the end
set.seed(10)
finalTest_t_size <- .05*samp_size_t
finalTest_t_ind <-sample(1:samp_size_t,finalTest_t_size,replace=FALSE)
finalTest_t <- sdata_t[finalTest_t_ind]

finalTest_b_size <- .05*samp_size_b
finalTest_b_ind <-sample(1:samp_size_b,finalTest_b_size,replace=FALSE)
finalTest_b <- sdata_b[finalTest_b_ind]

finalTest_n_size <- .05*samp_size_n
finalTest_n_ind <-sample(1:samp_size_n,finalTest_n_size,replace=FALSE)
finalTest_n <- sdata_n[finalTest_n_ind]

sdata_t <- sdata_t[-finalTest_t_ind]
sdata_b <- sdata_b[-finalTest_b_ind]
sdata_n <- sdata_n[-finalTest_n_ind]


############################change seed below and run multiple times to iterate (do not change above)
set.seed(101)
train_t_size <- .15*samp_size_t  ##factor can't be more than .8
train_t_ind <-sample(1:samp_size_t,train_t_size,replace=FALSE)
training_t <- sdata_t[train_t_ind]
ind <- 1:samp_size_t
test_t_size <- .05*samp_size_t
test_t_ind<-sample(ind[-train_t_ind],test_t_size,replace=FALSE)
testing_t <- sdata_t[test_t_ind]

train_b_size <- .15*samp_size_b
train_b_ind <-sample(1:samp_size_b,train_b_size,replace=FALSE)
training_b <- sdata_b[train_b_ind]
ind <- 1:samp_size_t
test_t_size <- .05*samp_size_t
test_t_ind<-sample(ind[-train_t_ind],test_t_size,replace=FALSE)
testing_t <- sdata_t[test_t_ind]

train_n_size <- .15*samp_size_n
train_n_ind <-sample(1:samp_size_n,train_n_size,replace=FALSE)
training_n <- sdata_n[train_n_ind]
ind <- 1:samp_size_t
test_t_size <- .05*samp_size_t
test_t_ind<-sample(ind[-train_t_ind],test_t_size,replace=FALSE)
testing_t <- sdata_t[test_t_ind]

###############################

###get rid of profanity
setwd("C:/Users/Sarah Lynn/Desktop/Self Study/Coursera DS JH - Capstone")
wd_loc <- getwd()
profane_words <- read.csv(paste(wd_loc,"/profanity.csv",sep=""))
training_t_clean <- removeWords(training_t,profane_words[1:14,1])  #removed 4433
training_b_clean <- removeWords(training_b,profane_words[1:14,1])  #removed 766
training_n_clean <- removeWords(training_n,profane_words[1:14,1])  #removed 35

#############################################################################################
##Unigrams
#############################################################################################


###clean out punctuation, stopwords, uppercase, and numbers
##combine datasets
training1 <- c(training_t_clean,training_b_clean,training_n_clean)
training1 <- tolower(training1)
training2 <-gsub('[[:punct:] ]+',' ',training1)
training2 <- Corpus(VectorSource(training2))
##training5 <- tm_map(training4, removeWords, stopwords("english"))   keep in because we want to predict stopwords too
dtm_unigram <- removeSparseTerms(TermDocumentMatrix(training2), 0.9998)
freq_mat<- slam::row_sums(dtm_unigram)
m <- as.matrix(freq_mat)
words <- rownames(m)
counts1 <- as.vector(as.numeric(m))
sort_freq_mat <- sort(freq_mat, decreasing=TRUE)


#plot number of words with each count to see distribution
#hist(sort_freq_mat,breaks=50,ylim=c(0,5000)) ####too skewed to be useful

df <- as.data.frame(cbind(words,counts1))
df$counts1 <- as.numeric(df$counts1)
sdf <- df %>% arrange(desc(counts1))  


###try to plot some interestin gparts, not all of it
ggplot(sdf[1:100,], aes(x = reorder(words[1:100], counts1[1:100]), y = counts1[1:100])) +
  geom_col() +
  labs(title="Word Count Data",
       x = NULL,
       y = "Frequency") +
  coord_flip()

sdf$cumcounts1 <- cumsum(sdf$counts1)   
sdf$totcounts1 <- as.numeric(rep(max(sdf$cumcounts1),length(sdf$cumcounts1)))
sdf$perccum <- sdf$cumcounts1/sdf$totcounts1
sdf$perc <- sdf$counts1/sdf$totcounts1
sdf$rank <- 1:nrow(sdf)
unigram_perc <- rep(0,99)
for (i in 1:99) {
  p <- i/100
  unigram_perc[i] <- min(which(sdf$perccum>p))  }
plot(unigram_perc, xlab="% of total word instances", ylab="number of unigrams",type="l")

#################################################################################################
## bigrams
#################################################################################################


bigram_training3 <- removePunctuation(training1)
bigram_training3 <- VCorpus(VectorSource(bigram_training3))
#bigram_training3 <- tm_map(bigram_training3, removeWords, stopwords("english")) ##keep in because we want to predict stopwords

BigramTokenizer <-function(x) {unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)}
dtm_bigram <- TermDocumentMatrix(bigram_training3, control = list(tokenize = BigramTokenizer))
dtm_bigram <- removeSparseTerms(dtm_bigram, 0.9998)

bigram_freq <-slam::row_sums(dtm_bigram)
sort_bigram_freq <- sort(bigram_freq,decreasing=TRUE)

bigram_m <- as.matrix(bigram_freq)
bigram_words <- rownames(bigram_m)
bigram_counts1 <- as.vector(as.numeric(bigram_m))

bigram_df <- as.data.frame(cbind(bigram_words,bigram_counts1))
bigram_df$bigram_counts1 <- as.numeric(bigram_df$bigram_counts1)
bigram_sdf <- bigram_df %>% arrange(desc(bigram_counts1))  
bigram_sdf$bigram_stems <- word(bigram_sdf$bigram_words, 1, 1)


ggplot(bigram_sdf[1:30,], aes(x = reorder(bigram_words[1:30], bigram_counts1[1:30]), y = bigram_counts1[1:30])) +
  geom_col() +
  labs(title="Bigram Count Data",
       x = NULL,
       y = "Frequency") +
  coord_flip()

bigram_sdf$cumcounts1 <- cumsum(bigram_sdf$bigram_counts1)   
bigram_sdf$totcounts1 <- as.numeric(rep(max(bigram_sdf$cumcounts1),length(bigram_sdf$cumcounts1)))
bigram_sdf$perccum <- bigram_sdf$cumcounts1/bigram_sdf$totcounts1
bigram_sdf$perc <- bigram_sdf$bigram_counts1/bigram_sdf$totcounts1
bigram_perc <- rep(0,99)
for (i in 1:99) {
  p <- i/100
  bigram_perc[i] <- min(which(bigram_sdf$perccum>p))  }
plot(bigram_perc, xlab="% of total bigram instances", ylab="number of bigrams",type="l")


#################################################################################################
## trigrams
#################################################################################################


trigram_training3 <- removePunctuation(training1)
trigram_training3 <- VCorpus(VectorSource(trigram_training3))
#trigram_training3 <- tm_map(trigram_training3, removeWords, stopwords("english")) ##keep in because we want to predict stopwords

TrigramTokenizer <-function(x) {unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)}
dtm_trigram <- TermDocumentMatrix(trigram_training3, control = list(tokenize = TrigramTokenizer))
dtm_trigram <- removeSparseTerms(dtm_trigram, 0.9998)

trigram_freq <-slam::row_sums(dtm_trigram)
sort_trigram_freq <- sort(trigram_freq,decreasing=TRUE)

trigram_m <- as.matrix(trigram_freq)
trigram_words <- rownames(trigram_m)
trigram_counts1 <- as.vector(as.numeric(trigram_m))

trigram_df <- as.data.frame(cbind(trigram_words,trigram_counts1))
trigram_df$trigram_counts1 <- as.numeric(trigram_df$trigram_counts1)
trigram_sdf <- trigram_df %>% arrange(desc(trigram_counts1))  
trigram_sdf$trigram_stems <- word(trigram_sdf$trigram_words, 1, 2)

ggplot(trigram_sdf[1:30,], aes(x = reorder(trigram_words[1:30], trigram_counts1[1:30]), y = trigram_counts1[1:30])) +
  geom_col() +
  labs(title="Trigram Count Data",
       x = NULL,
       y = "Frequency") +
  coord_flip()

trigram_sdf$cumcounts1 <- cumsum(trigram_sdf$trigram_counts1)   
trigram_sdf$totcounts1 <- as.numeric(rep(max(trigram_sdf$cumcounts1),length(trigram_sdf$cumcounts1)))
trigram_sdf$perccum <- trigram_sdf$cumcounts1/trigram_sdf$totcounts1
trigram_sdf$perc <- trigram_sdf$trigram_counts1/trigram_sdf$totcounts1
trigram_perc <- rep(0,99)
for (i in 1:99) {
  p <- i/100
  trigram_perc[i] <- min(which(trigram_sdf$perccum>p))  }
plot(trigram_perc, xlab="% of total bigram instances", ylab="number of bigrams",type="l")

#############################################################################################################################
#################################################################################################################################
####EXCLUDE STOP WORDS
##################################################################################################################################
############################################################################################################################

#############################################################################################
##Unigrams
#############################################################################################


###clean out punctuation, stopwords, uppercase, and numbers
##combine datasets
# training1 <- c(training_t_clean,training_b_clean,training_n_clean)
# training1 <- tolower(training1)
# training2 <-gsub('[[:punct:] ]+',' ',training1)
# training2 <- Corpus(VectorSource(training2))
training22 <- tm_map(training2, removeWords, stopwords("english"))  
dtm_unigramSW <- removeSparseTerms(TermDocumentMatrix(training22), 0.9998)
freq_matSW<- slam::row_sums(dtm_unigramSW)
mSW <- as.matrix(freq_matSW)
wordsSW <- rownames(mSW)
counts1SW <- as.vector(as.numeric(mSW))
sort_freq_matSW <- sort(freq_matSW, decreasing=TRUE)


#plot number of words with each count to see distribution
#hist(sort_freq_mat,breaks=50,ylim=c(0,5000)) ####too skewed to be useful


dfSW <- as.data.frame(cbind(wordsSW,counts1SW))
dfSW$counts1SW <- as.numeric(dfSW$counts1SW)
sdfSW <- dfSW %>% arrange(desc(counts1SW))  

# ggplot(sdfSW[1:100,], aes(x = reorder(wordsSW[1:100], counts1SW[1:100]), y = counts1SW[1:100])) +
#   geom_col() +
#   labs(title="Word Count Data",
#        x = NULL,
#        y = "Frequency") +
#   coord_flip()

sdfSW$cumcounts1SW <- cumsum(sdfSW$counts1SW)   
sdfSW$totcounts1SW <- as.numeric(rep(max(sdfSW$cumcounts1SW),length(sdfSW$cumcounts1SW)))
sdfSW$perccumSW <- sdfSW$cumcounts1SW/sdfSW$totcounts1SW
sdfSW$percSW <- sdfSW$counts1SW/sdfSW$totcounts1SW
sdfSW$rankSW <- 1:nrow(sdfSW)
unigram_percSW <- rep(0,99)
for (i in 1:99) {
  p <- i/100
  unigram_percSW[i] <- min(which(sdfSW$perccumSW>p))  }
#plot(unigram_percSW, xlab="% of total word instances", ylab="number of unigrams",type="l")

#################################################################################################
## bigrams
#################################################################################################


bigram_training3 <- removePunctuation(training1)
bigram_training3 <- VCorpus(VectorSource(bigram_training3))
bigram_training33 <- tm_map(bigram_training3, removeWords, stopwords("english")) 

#BigramTokenizer <-function(x) {unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)}
dtm_bigramSW <- TermDocumentMatrix(bigram_training33, control = list(tokenize = BigramTokenizer))
dtm_bigramSW <- removeSparseTerms(dtm_bigramSW, 0.9998)

bigram_freqSW <-slam::row_sums(dtm_bigramSW)
sort_bigram_freqSW <- sort(bigram_freqSW,decreasing=TRUE)

bigram_mSW <- as.matrix(bigram_freqSW)
bigram_wordsSW <- rownames(bigram_mSW)
bigram_counts1SW <- as.vector(as.numeric(bigram_mSW))

bigram_dfSW <- as.data.frame(cbind(bigram_wordsSW,bigram_counts1SW))
bigram_dfSW$bigram_counts1SW <- as.numeric(bigram_dfSW$bigram_counts1SW)
bigram_sdfSW <- bigram_dfSW %>% arrange(desc(bigram_counts1SW))  
bigram_sdfSW$bigram_stemsSW <- word(bigram_sdfSW$bigram_wordsSW, 1, 1)


# ggplot(bigram_sdfSW[1:30,], aes(x = reorder(bigram_wordsSW[1:30], bigram_counts1SW[1:30]), y = bigram_counts1SW[1:30])) +
#   geom_col() +
#   labs(title="Bigram Count Data",
#        x = NULL,
#        y = "Frequency") +
#   coord_flip()

bigram_sdfSW$cumcounts1SW <- cumsum(bigram_sdfSW$bigram_counts1SW)   
bigram_sdfSW$totcounts1SW <- as.numeric(rep(max(bigram_sdfSW$cumcounts1SW),length(bigram_sdfSW$cumcounts1SW)))
bigram_sdfSW$perccumSW <- bigram_sdfSW$cumcounts1SW/bigram_sdfSW$totcounts1SW
bigram_sdfSW$percSW <- bigram_sdfSW$bigram_counts1SW/bigram_sdfSW$totcounts1SW
bigram_percSW <- rep(0,99)
for (i in 1:99) {
  p <- i/100
  bigram_percSW[i] <- min(which(bigram_sdfSW$perccumSW>p))  }
#plot(bigram_percSW, xlab="% of total bigram instances", ylab="number of bigrams",type="l")


#################################################################################################
## trigrams
#################################################################################################


trigram_training3 <- removePunctuation(training1)
trigram_training3 <- VCorpus(VectorSource(trigram_training3))
trigram_training33 <- tm_map(trigram_training3, removeWords, stopwords("english")) 

#TrigramTokenizer <-function(x) {unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)}
dtm_trigramSW <- TermDocumentMatrix(trigram_training33, control = list(tokenize = TrigramTokenizer))
dtm_trigramSW <- removeSparseTerms(dtm_trigramSW, 0.9998)

trigram_freqSW <-slam::row_sums(dtm_trigramSW)
sort_trigram_freqSW <- sort(trigram_freqSW,decreasing=TRUE)

trigram_mSW <- as.matrix(trigram_freqSW)
trigram_wordsSW <- rownames(trigram_mSW)
trigram_counts1SW <- as.vector(as.numeric(trigram_mSW))

trigram_dfSW <- as.data.frame(cbind(trigram_wordsSW,trigram_counts1SW))
trigram_dfSW$trigram_counts1SW <- as.numeric(trigram_dfSW$trigram_counts1SW)
trigram_sdfSW <- trigram_dfSW %>% arrange(desc(trigram_counts1SW))  
trigram_sdfSW$trigram_stemsSW <- word(trigram_sdfSW$trigram_wordsSW, 1, 2)

# ggplot(trigram_sdfSW[1:30,], aes(x = reorder(trigram_wordsSW[1:30], trigram_counts1SW[1:30]), y = trigram_counts1SW[1:30])) +
#   geom_col() +
#   labs(title="Trigram Count Data",
#        x = NULL,
#        y = "Frequency") +
#   coord_flip()

trigram_sdfSW$cumcounts1SW <- cumsum(trigram_sdfSW$trigram_counts1SW)   
trigram_sdfSW$totcounts1SW <- as.numeric(rep(max(trigram_sdfSW$cumcounts1SW),length(trigram_sdfSW$cumcounts1SW)))
trigram_sdfSW$perccumSW <- trigram_sdfSW$cumcounts1SW/trigram_sdfSW$totcounts1SW
trigram_sdfSW$percSW <- trigram_sdfSW$trigram_counts1SW/trigram_sdfSW$totcounts1SW
trigram_percSW <- rep(0,99)
for (i in 1:99) {
  p <- i/100
  trigram_percSW[i] <- min(which(trigram_sdfSW$perccumSW>p))  }
#plot(trigram_percSW, xlab="% of total bigram instances", ylab="number of bigrams",type="l")






##############################################################################################################################
##################################################################################################################################
#################################################################################################################################

#############################################################################################3
###MODEL
############################################################################################
#### index ngram tables ???????
# setkey(uni_words, word_1)
# setkey(bi_words, word_1, word_2)
# setkey(tri_words, word_1, word_2, word_3)

# test_phrase <- "I was going to say thanks for the"
# test_phrase <- "I will go "
# test_phrase <- "I think it was"
# test_phrase <- "Have you ever"
# test_phrase <- "I don't like looking at the"
# test_phrase <- "I was riding her blue bike"
# test_phrase <- "I love reading ralph waldo"
# test_phrase <- "I don't even know what to"

word_predictor <- function(text) {
test_phrase <- text
##clean the input
test_phrase1 <- tolower(test_phrase)
test_phrase2 <-removePunctuation(test_phrase1) ##gsub("[^[:alnum:][:space:]']", "", test_phrase1)
test_phrase3 <- unlist(strsplit(test_phrase2, split=" "))
test_phrase4 <- if (length(test_phrase3) > 1) {test_phrase3[(length(test_phrase3)-1):length(test_phrase3)]} else {test_phrase3[(length(test_phrase3))]}
test_phrase5 <- paste(test_phrase4,collapse=" ")
test_phrase55 <- removeWords(test_phrase5, stopwords("english"))
num_stopwrds <- ifelse(test_phrase55==" ",2,stri_count_words(test_phrase5)-stri_count_words(test_phrase55))

test_phrase222 <- str_trim(removeWords(test_phrase2, stopwords("english")))
test_phrase333 <- unlist(strsplit(test_phrase222, split=" "))
test_phrase444 <- if (length(test_phrase333) > 1) {test_phrase333[(length(test_phrase333)-1):length(test_phrase333)]} else {test_phrase333[(length(test_phrase333))]}
test_phrase555 <- paste(test_phrase444,collapse=" ")

###predict

####if the number of stopwords is less than 2, then predict with stopwords included
if (num_stopwrds<2) {

if (length(test_phrase4)==2) {
chck <- sum(trigram_sdf$trigram_stems==test_phrase5)  
    if (chck!=0) {
    possible_phrases_ind <- which(trigram_sdf$trigram_stems==test_phrase5)   
    #possible_phrases_ind2 <-min(which(max(trigram_sdf$perc[possible_phrases_ind])==trigram_sdf$perc))
    possible_phrases_ind2 <- min(possible_phrases_ind)  ###relies on sdf tables being sorted by probability
    winner_trigram <- trigram_sdf$trigram_words[possible_phrases_ind2]
    winner_trigram2 <- unlist(strsplit(winner_trigram, split=" "))
    pred_Word <-winner_trigram2[length(winner_trigram2)]
    }
} 

if (chck==0 | length(test_phrase4)==1) {
test_phrase6  <- unlist(strsplit(test_phrase5, split=" "))
test_phrase7 <- ifelse(length(test_phrase6)==1,test_phrase6,test_phrase6[2])
chck <- sum(bigram_sdf$bigram_stems==test_phrase7)
    if (chck!=0) {
    possible_phrases_ind <- which(bigram_sdf$bigram_stems==test_phrase7)
    #possible_phrases_ind2 <- min(which(max(bigram_sdf$perc[possible_phrases_ind])==bigram_sdf$perc))
    possible_phrases_ind2 <- min(possible_phrases_ind)  ###relies on sdf tables being sorted by probability
    winner_bigram <- bigram_sdf$bigram_words[possible_phrases_ind2]
    winner_bigram2 <- unlist(strsplit(winner_bigram, split=" "))
    pred_Word <-winner_bigram2[length(winner_bigram2)]
    } else {pred_Word <- sdf$words[which(which(max(sdf$perc)==sdf$perc)==sdf$rank)]}
}
pred_Word
algoType <- c("stopwords included")


} else {
####if the number of stopwords is 2 (or more) then predict without stoppwords

  
  if (length(test_phrase444)==2) {
    chck <- sum(trigram_sdfSW$trigram_stemsSW==test_phrase555)  
    if (chck!=0) {
      possible_phrases_ind <- which(trigram_sdfSW$trigram_stemsSW==test_phrase555)   
      #possible_phrases_ind2 <-min(which(max(trigram_sdfSW$perc[possible_phrases_ind])==trigram_sdfSW$percSW))
      possible_phrases_ind2 <- min(possible_phrases_ind)  ###relies on sdf tables being sorted by probability
      winner_trigram <- trigram_sdfSW$trigram_wordsSW[possible_phrases_ind2]
      winner_trigram2 <- unlist(strsplit(winner_trigram, split=" "))
      pred_Word <-winner_trigram2[length(winner_trigram2)]
    }
  } 
  
  if (chck==0 | length(test_phrase444)==1) {
    test_phrase6  <- unlist(strsplit(test_phrase555, split=" "))
    test_phrase7 <- ifelse(length(test_phrase6)==1,test_phrase6,test_phrase6[2])
    chck <- sum(bigram_sdfSW$bigram_stemsSW==test_phrase7)
    if (chck!=0) {
      possible_phrases_ind <- which(bigram_sdfSW$bigram_stemsSW==test_phrase7)
      #possible_phrases_ind2 <- min(which(max(bigram_sdfSW$percSW[possible_phrases_ind])==bigram_sdfSW$percSW))
      possible_phrases_ind2 <- min(possible_phrases_ind)  ###relies on sdf tables being sorted by probability
      winner_bigram <- bigram_sdfSW$bigram_wordsSW[possible_phrases_ind2]
      winner_bigram2 <- unlist(strsplit(winner_bigram, split=" "))
      pred_Word <-winner_bigram2[length(winner_bigram2)]
    } else {pred_Word <- sdfSW$wordsSW[which(which(max(sdfSW$percSW)==sdfSW$percSW)==sdfSW$rankSW)]}
  }
  pred_Word  
  algoType <- c("stopwords removed")
  
  
  
}
return(pred_Word)
} ###end of function
####################################################################################################
############################################################################################################
###end of model: i expect accuracy to be pretty low, maybe 10-20%
################################################################################################################
###############################################################################################################

# Stop the clock
proc.time() - ptm



##evaluate accuracy with test set
##build perplexity
##segments parts to be "offline"
##consider adding Kneser-Ney smoothing to improve accuracy
##consider gc() and other methods to speed it up



