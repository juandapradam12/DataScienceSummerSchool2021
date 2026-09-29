## ----------------------------------------------------------------------------------------------
library(tidyverse)
library(quanteda)
library(stm) # the topic model package we'll use

# ¡¡¡Abrir la carpeta del archivo y darle clic desde ahí, aceptar que lo queremos usar acá!!!
# load("data/corpus_us_debate.rda") NO usar esta linea

summary(corpus_us_debate, n = 5)


## ----------------------------------------------------------------------------------------------
para_corp <- corpus_reshape(corpus_us_debate, to = "paragraphs") 
head(summary(para_corp))


## ----------------------------------------------------------------------------------------------
table(ntoken(para_corp)) # 15 documents with, err, no words


## ----------------------------------------------------------------------------------------------
para_corp <- corpus_subset(para_corp, ntoken(para_corp) > 2)

toks <- tokens(para_corp, 
               remove_punct = TRUE, 
               remove_numbers = TRUE) #|
  #tokens_remove(stopwords()) # living dangerously!

para_dfm <- dfm(toks)


## ---- eval = FALSE-----------------------------------------------------------------------------
mod <- stm(para_dfm, K = 10, seed = 12345)




## ----------------------------------------------------------------------------------------------
topic_props <- mod$theta
dim(topic_props) # a documents by K matrix

head(topic_props)


## ----------------------------------------------------------------------------------------------
word_logprobs <- mod$beta[[1]][[1]] # !!
dim(word_logprobs) # K by vocabulary size

# sort from high to low log beta
topic4_highest <- order(word_logprobs[4,], decreasing = TRUE)
# look at the top 20
mod$vocab[topic4_highest[1:20]]


## ----------------------------------------------------------------------------------------------
labelTopics(mod)


## ----------------------------------------------------------------------------------------------
topic_labs <- labelTopics(mod)
str(topic_labs)


## ----------------------------------------------------------------------------------------------
topic_labs <- apply(topic_labs$frex, 1, paste, collapse = "-")
topic_labs


## ----------------------------------------------------------------------------------------------
plot(mod, type = "labels", labeltype = "frex") # or frex, lift, score


## ----------------------------------------------------------------------------------------------
findThoughts(mod, texts = as.character(para_corp), topics = 4)


## ----------------------------------------------------------------------------------------------
checkBeta(mod)$problemWords # we'll look at the 'problem words field'


## ----------------------------------------------------------------------------------------------
dotchart(exclusivity(mod), labels = 1:10)


## ----------------------------------------------------------------------------------------------
cohere <- semanticCoherence(mod, para_dfm)
dotchart(cohere, labels = 1:10)


## ----------------------------------------------------------------------------------------------
head(mod$theta)


## ----------------------------------------------------------------------------------------------
df <- data.frame(topic4 = mod$theta[,4], docvars(para_dfm))
head(df)


## ---- eval = FALSE-----------------------------------------------------------------------------
mod2 <- stm(para_dfm, K = 10, 
           prevalence = ~ party, data = docvars(para_dfm),
           seed = 12345)




## ----------------------------------------------------------------------------------------------
gg <- estimateEffect(c(4,8) ~ party, mod2, docvars(para_dfm))
summary(gg)


## ----------------------------------------------------------------------------------------------
plot(gg, "party", labeltype = "prob", model = mod2)

