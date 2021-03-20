#################################################################################
###Evaluate model accuracy
#################################################################################

word_predictor(c("I have always wanted"))

test_text <- c("I have always wanted","If you never","Sure but","So for today")
word_preds <- test_text
for (i in 1:length(test_text)) {
  text <- test_text[i]
  word_preds[i] <- word_predictor(text)
}



############################

##note that changing sample from 10% to 15%-25% didnt change the 4 predictions tested, but took an hour to run isntead of 15 min.
set.seed(101)
train_t_size <- .1*samp_size_t
train_t_ind <-sample(1:samp_size_t,train_t_size,replace=FALSE)
training_t <- sdata_t[train_t_ind]
ind <- 1:samp_size_t
test_t_size <- .1*samp_size_t
test_t_ind<-sample(ind[-train_t_ind],test_t_size,replace=FALSE)
testing_t <- sdata_t[test_t_ind]

train_b_size <- .1*samp_size_b
train_b_ind <-sample(1:samp_size_b,train_b_size,replace=FALSE)
training_b <- sdata_b[train_b_ind]
ind <- 1:samp_size_t
test_t_size <- .1*samp_size_t
test_t_ind<-sample(ind[-train_t_ind],test_t_size,replace=FALSE)
testing_t <- sdata_t[test_t_ind]

train_n_size <- .1*samp_size_n
train_n_ind <-sample(1:samp_size_n,train_n_size,replace=FALSE)
training_n <- sdata_n[train_n_ind]
ind <- 1:samp_size_t
test_t_size <- .1*samp_size_t
test_t_ind<-sample(ind[-train_t_ind],test_t_size,replace=FALSE)
testing_t <- sdata_t[test_t_ind]




###start here

##code to subset test set to allow for predictions with right answers
testing_master <- c(training_t_clean,training_b_clean,training_n_clean)

wordcount(testing_master[2])
test_counts<-unlist(lapply(testing_master[1:5], wordcount))
##samples of 1 word
testing_mat <- as.matrix(test_counts)
testing_mat$samp_st1 <- sapply(test_counts,function(x) sample(1:x,1))
testing_mat$samp_st2 <- sapply(test_counts,function(x) ifelse(x<3,1,sample(1:(x-2),1)))
testing_mat$samp_st3 <- sapply(test_counts,function(x) ifelse(x<4,sample(1:2,1),sample(1:(x-3),1)))
testing_mat$str1<- word(testing_master[1:5], start=testing_mat$samp_st1,end=testing_mat$samp_st1, sep = fixed(" "))
testing_mat$str2<- word(testing_master[1:5], start=testing_mat$samp_st2,end=testing_mat$samp_st2+1, sep = fixed(" "))
testing_mat$str3<- word(testing_master[1:5], start=testing_mat$samp_st3,end=testing_mat$samp_st3+2, sep = fixed(" "))
