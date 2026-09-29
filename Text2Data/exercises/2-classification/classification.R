## ----------------------------------------------------------------------------------------------
library(tidyverse)
library(quanteda)
library(quanteda.textmodels)
library(plotROC) # for evaluating models


## ----------------------------------------------------------------------------------------------
corp_movies <- data_corpus_moviereviews
head(docvars(corp_movies))


## ----------------------------------------------------------------------------------------------
as.character(corp_movies)[507]


## ----------------------------------------------------------------------------------------------
corp_movies$id_numeric <- 1:ndoc(corp_movies) # add a docvar to the main corpus

# randomize the training set
set.seed(800)
id_train <- c(sample(1:1000, 500), sample(1001:2000, 500)) 
id_test <- setdiff(1:2000, id_train)

# make two corpora
train_corp <- corpus_subset(corp_movies, id_numeric %in% id_train)
test_corp <- corpus_subset(corp_movies, id_numeric %in% id_test)




## ----------------------------------------------------------------------------------------------

train_corp

train_dfm <- tokens(train_corp, remove_punct = TRUE, remove_symbols = TRUE, 
                    remove_numbers = TRUE, remove_url = TRUE) #|# make sure it's all lowercase
                      #tokens_tolower() |
                        # count those tokens
                        #dfm(train_dfm) |
                      # remove columns of tokens that didn't occur 'often enough'
                      #dfm_trim(min_termfreq = 5)

class(train_dfm)

train_dfm <- tokens_tolower(train_dfm)

# convert into dfm() object
train_dfm <- dfm(train_dfm)

train_dfm <- dfm_trim(train_dfm, min_termfreq = 5)

train_dfm
class(train_dfm)

train_dfm$sentiment

## ----------------------------------------------------------------------------------------------

test_dfm <- tokens(test_corp, remove_punct = TRUE, remove_symbols = TRUE, 
                    remove_numbers = TRUE, remove_url = TRUE) #|
  # make sure it's all lowercase
  #tokens_tolower() #|>
  # count everything, which is fine because
  #dfm() |>
  # this drops out everything new and zeros everything we didn't see 
  # in the training data
  #dfm_match(featnames(train_dfm))

# convert into dfm() object
test_dfm <- dfm(test_dfm)

test_dfm 
class(test_dfm)


## ----------------------------------------------------------------------------------------------

tmod1 <- textmodel_lr(train_dfm, train_dfm$sentiment)
tmod1
summary(tmod1)


## ----------------------------------------------------------------------------------------------
tmod1_coefs <- coef(tmod1)
tmod1_coefs

dim(tmod1_coefs) # hmm, it's basically one skinny matrix / vector

# so we'll make it something easier to deal with
tmp <- data.frame(word = rownames(tmod1_coefs),
                  coef = as.vector(tmod1_coefs)) 



#filter(coef != 0) #|>
#arrange(coef)

tmp
head(tmp)
tail(tmp)

## ----------------------------------------------------------------------------------------------
training_preds <- predict(tmod1) 
table(model = training_preds, true = train_dfm$sentiment)


## ----------------------------------------------------------------------------------------------
test_preds <- predict(tmod1, newdata = test_dfm) 
testconf <- table(model = test_preds, true = test_dfm$sentiment)
testconf


## ----------------------------------------------------------------------------------------------
precision_tab <- prop.table(testconf, 1) # make rows (1) sum to 1 => precision
precision_tab
recall_tab <- prop.table(testconf, 2) # make columns (2) sum to 1 => recall
recall_tab

# precision for each category
diag(precision_tab)

# recall for each category
diag(recall_tab)


## ----------------------------------------------------------------------------------------------
test_probs <- predict(tmod1, newdata = test_dfm, type = "probability")
test_probs <- as.data.frame(test_probs) # traditionally but annoyingly predict returns a matrix
head(test_probs)


## ----------------------------------------------------------------------------------------------
ggplot(test_probs, aes(x = pos)) + 
  geom_density(fill = "grey") + 
  labs(x = "Predicted probability of being a positive review")


## ----------------------------------------------------------------------------------------------
# for which reviews are we more than 80% sure one way or the other?
surer_ones <- which(test_probs[,"pos"] > 0.8 | test_probs$neg > 0.8)
# new confident results by subsetting the predictions and the true topics
surer_testconf <- table(model = test_preds[surer_ones], 
                        true = test_dfm$sentiment[surer_ones])
surer_testconf
diag(prop.table(surer_testconf, 1)) # precision
diag(prop.table(surer_testconf, 2)) # recall


## ----------------------------------------------------------------------------------------------
# these are the *midpoints* of the intervals
labels <- seq(0.05, 1, by = 0.1) 

# predicted probability of being positive
cal <- data.frame(pred_prob = test_probs$pos, truth = test_dfm$sentiment) #| # whether it was actually positive
  # divide the reviews into groups according to their predictive probabilities
  # using 'cut' and providing our own labels
#mutate(bin = cut(test_probs$pos, breaks = 10, labels = labels)) #|>
  # bundle together similar predictions
  # .drop = FALSE means even if there are no predictions in a bin, keep the bin
#group_by(bin, .drop = FALSE) #|>
  # within each bin, what proportion were really positive?
#summarize(prop_pos = mean(truth == "pos"))
cal


## ----------------------------------------------------------------------------------------------
# make sure we can plot the midpoints properly (bin is now a factor, so we'll 
# use the midpoints we made)
cal <- mutate(cal) #, midpoint = labels)

ggplot(cal, aes(midpoint, prop_pos))  
  geom_abline(intercept = 0, slope = 1, linetype = "dotted") +
  geom_point() + 
  geom_line() +
  lims(x = c(0,1), y = c(0,1)) + 
  labs(x = "Midpoint of predicted probabilities (positive)",
       y = "Proportion of reviews positive")



## ----------------------------------------------------------------------------------------------
train_dfm_nb <- tokens(train_corp, remove_punct = TRUE, remove_symbols = TRUE, 
                       remove_numbers = TRUE, remove_url = TRUE) #|>
  
  #tokens_remove(stopwords()) |>
  #tokens_tolower() |>
  #dfm() |>
  #dfm_trim(min_termfreq = 5)
  
train_dfm_nb <- tokens_remove(train_dfm_nb, stopwords()) 
train_dfm_nb <- tokens_tolower(train_dfm_nb) 
train_dfm_nb <- dfm(train_dfm_nb) 
train_dfm_nb <- dfm_trim(train_dfm_nb, min_termfreq = 5)

test_dfm_nb <- tokens(test_corp, remove_punct = TRUE, remove_symbols = TRUE, 
                      remove_numbers = TRUE, remove_url = TRUE) #|>
  #tokens_tolower() |>
  #dfm() |>
  #dfm_match(featnames(train_dfm_nb))

test_dfm_nb <- tokens_tolower(test_dfm_nb)
test_dfm_nb <- dfm(test_dfm_nb)
#dfm_match(featnames(train_dfm_nb)

tmod2 <- textmodel_nb(train_dfm_nb, train_dfm_nb$sentiment)


## ----------------------------------------------------------------------------------------------
nb_coefs <- coef(tmod2) 

nb_coefs
  
nb_coefs <- as.data.frame(nb_coefs) #|>
# construct the ubiquitous log ratios to sort with
nb_coefs <- mutate(nb_coefs, valence = log(pos/neg)) # |>
nb_coefs <- arrange(nb_coefs, valence) 

head(nb_coefs) # the worst (I lol-ed)
tail(nb_coefs) # the best


## ----------------------------------------------------------------------------------------------
test_preds_nb <- predict(tmod2) # , newdata = test_dfm_nb

# confusion matrix
testconf_nb <- table(model = test_preds_nb, true = test_dfm_nb$sentiment)
testconf_nb


## ----------------------------------------------------------------------------------------------
table(test_preds, test_preds_nb)


## ----------------------------------------------------------------------------------------------
diag(prop.table(testconf_nb, 1)) # precision
diag(prop.table(testconf_nb, 2)) # recall


## ----------------------------------------------------------------------------------------------
# calibration
test_probs_nb <- predict(tmod2) #,  newdata = test_dfm_nb, type = "probability")
test_probs_nb <- as.data.frame(test_probs_nb)

cal_nb <- data.frame(pred_prob = test_probs_nb$pos, truth = test_dfm_nb$sentiment) #|> 
 # mutate(bin = cut(pred_prob, breaks = 10, labels = labels)) 
  #group_by(bin, .drop = FALSE) |>
  #summarize(prop_pos = mean(truth == "pos"))
cal_nb

cal_nb <- mutate(cal_nb, midpoint = labels)
ggplot(cal_nb, aes(midpoint, prop_pos)) + 
  geom_abline(intercept = 0, slope = 1, linetype = "dotted") +
  geom_point() + 
  geom_line() +
  lims(x = c(0,1), y = c(0,1)) + 
  labs(x = "Midpoint of predicted probabilities (positive)",
       y = "Proportion of reviews positive")


## ----------------------------------------------------------------------------------------------
ggplot(test_probs_nb, aes(x = pos)) + 
  geom_density(fill = "grey") + 
  labs(x = "Predicted probability of being a positive review")


## ----------------------------------------------------------------------------------------------
# some stuff we need now but didn't create earlier
train_probs <- predict(tmod1, type = "probability") 
train_probs <- as.data.frame(train_probs)

train_probs_nb <- predict(tmod2, type = "probability") 
train_probs_nb <- as.data.frame(train_probs_nb)

# the 'training data' (or previous model in sample predictions)
# with positive -> 1 and negative -> 0
ens_train <- data.frame(lr = train_probs$pos, 
                        nb = test_probs_nb$pos,
                        is_pos = as.numeric(train_dfm$sentiment == "pos"))

# the 'test data' (our previous models test data predictions)
ens_test <- data.frame(lr = test_probs$pos, 
                       nb = test_probs_nb$pos)
 
# fit a model
ens_model <- glm(is_pos ~ lr + nb, data = ens_train, family = binomial)
summary(ens_model)

# predicted probabilities from the uber-model
ens_probs <- predict(ens_model, newdata = ens_test, type = "response")

# classification
ens_topic <- ifelse(ens_probs > 0.5, "pos", "neg")

# how confused are we this time?
testconf_ens <- table(model = ens_topic, true = test_dfm_nb$sentiment)
testconf_ens


## ----------------------------------------------------------------------------------------------
roc_data <- data.frame(pred = c(test_probs$pos, test_probs_nb$pos, ens_probs),
                       true = rep(ifelse(test_dfm$sentiment == "pos", 1, 0), 3),
                       model = rep(c("logit", "naive-bayes", "combined"), each = 1000))

ggplot(roc_data, aes(d = true, m = pred, color = model)) + 
  geom_roc(n.cuts = 0) + 
  labs(x = "1 - Precision",
       y = "Recall")

