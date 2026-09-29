## ----setup, include = FALSE--------------------------------------------------------------------
knitr::opts_chunk$set(collapse = T)
library(tidyverse)
library(quanteda)


## ----------------------------------------------------------------------------------------------
library(tidyverse)
library(quanteda)
library(quanteda.textmodels)
library(quanteda.textstats)

library(ggrepel) # for labelling ggplots 


## ----------------------------------------------------------------------------------------------
# ¡¡¡Abrir la carpeta del archivo y darle clic desde ahí, aceptar que lo queremos usar acá!!!
# load("data/corpus_us_debate_speaker.rda") NO usar esta linea


summary(corpus_us_debate_speaker)

corp <- corpus_us_debate_speaker # give it a shorter name!


## ----------------------------------------------------------------------------------------------
# add 2 but remove 6
stops <- c(setdiff(stopwords(),  
                   c("her", "she", "hers", "he", "his", "him")),
           "bill", "can")

toks <- tokens(corp,  
               remove_punct = TRUE, 
               remove_symbols = TRUE,  
               remove_numbers = TRUE) # tocó quitarle lo siguiente para que funcione #|>
  #tokens_remove(stops) |>
  #tokens_tolower()
 
corpdfm <- dfm(toks)


## ----------------------------------------------------------------------------------------------
wfish <- textmodel_wordfish(corpdfm, dir = c(3, 21))
summary(wfish)


## ----------------------------------------------------------------------------------------------
wf_coef <- coef(wfish)
names(wf_coef)
head(wf_coef$documents) # speaker parameters 
head(wf_coef$features) # word parameters


## ----------------------------------------------------------------------------------------------
preds  <- predict(wfish, interval = "confidence")
preds


## ----------------------------------------------------------------------------------------------
# make a data frame from fit matrix and the document variables
ests <- data.frame(preds$fit)
speaker_pos <- data.frame(docvars(corpdfm), ests) #|>
  #arrange(fit) # sort left (negative) to right (positive)
head(speaker_pos)


## ----------------------------------------------------------------------------------------------
N <- nrow(speaker_pos)
speaker_names <- speaker_pos$speaker

ggplot(speaker_pos, aes(x = fit, y = 1:N, col = party)) +
  geom_point() +
  geom_errorbarh(aes(xmin = lwr, xmax = upr), height = 0) +
  scale_color_manual(values = c("blue", "red")) +
  scale_y_continuous(labels = speaker_names, breaks = 1:N) +
  labs(x = "Position", y = "Speaker") +
  ggtitle("Estimated Positions from Senate Partial-Birth Abortion Debate",
          subtitle = "October 21st, 2003")


## ----------------------------------------------------------------------------------------------
word_pos <- data.frame(word = wfish$features,
                       position = wfish$beta,
                       offset = wfish$psi)


## ----------------------------------------------------------------------------------------------
testwords <- c("life", "choice", "womb", "her", "woman", "health",
               "born", "baby", "little", "gruesome", "kill", "roe",
               "wade", "medical", "her", "his", "child", "religion",
               "catholic", "doctor", "nurse")


## ----------------------------------------------------------------------------------------------
testscores <- word_pos #|>
  #filter(word %in% testwords) |>
  #arrange(position)

testscores[,1:2] # just word and score columns


## ----------------------------------------------------------------------------------------------
ggplot(word_pos, aes(position, offset, label = word)) +
  geom_point(color = "grey", alpha = 0.2) +
  geom_text_repel(data = testscores, col = "black") +
  geom_point(data = testscores) +
  labs(x = "Word position", y = "Offset parameter") +
  ggtitle("Estimated word positions for selected debate vocabulary",
          subtitle = "Note: Offset parameter is roughly proportional to word frequency")

# No hacer caso al error de max overlaps

## ---- eval = FALSE-----------------------------------------------------------------------------
kwic(tokens(corp), "baby", window = 15)


## ----------------------------------------------------------------------------------------------
mod2 <- textmodel_ca(corpdfm, nd = 2)


## ----------------------------------------------------------------------------------------------
ca_coefs <- coef(mod2)
names(ca_coefs)


## ----------------------------------------------------------------------------------------------
ca_coefs$coef_document


## ----------------------------------------------------------------------------------------------
cor(ca_coefs$coef_document, ests$fit)


## ----------------------------------------------------------------------------------------------
ca_coefs$coef_document_se # seems rather rude 



## ----------------------------------------------------------------------------------------------
library(ca) # this comes with quanteda.textmodels

speaker_pos2 <- cacoord(mod2, dim = 1:2, rows = TRUE)
speaker_pos2


## ----------------------------------------------------------------------------------------------
sp_pos <- data.frame(speaker_pos2, 
                     speaker = rownames(speaker_pos2)) # add the speaker as a column
ggplot(sp_pos, aes(Dim1, Dim2)) + 
  geom_point() + 
  geom_text_repel(aes(label = speaker))


## ----------------------------------------------------------------------------------------------
bb <- textstat_keyness(corpdfm, target = "BROWNBACK")
head(bb, 20)


## ----------------------------------------------------------------------------------------------
# extract word positions
word_pos2 <- cacoord(mod2, dim = 1:2, cols = TRUE)
bb_words <- data.frame(word_pos2, word = rownames(word_pos2)) #|
  #filter(word %in% bb$feature[1:20]) |
  #arrange(Dim2)
bb_words


## ----------------------------------------------------------------------------------------------
corp_nobb <- corpus_subset(corp, speaker != "BROWNBACK")

toks_nobb <- tokens(corp_nobb,  
                    remove_punct = TRUE, 
                    remove_symbols = TRUE,  
                    remove_numbers = TRUE) #|
  #tokens_remove(stops) 

mod2_nobb <- textmodel_ca(dfm(toks_nobb), nd = 2)


## ----------------------------------------------------------------------------------------------
pos <- data.frame(cacoord(mod2, dim = 1:2, rows = TRUE))
pos_nobb <- data.frame(cacoord(mod2_nobb, dim = 1:2, rows = TRUE))

# connect the positions from both runs of the model together
pos_both <- merge(pos, pos_nobb, by = "row.names", 
                  suffixes = c("",".nobb"))
head(pos_both)

# does Brownback affect the first dimension?
cor(pos_both$Dim1, pos_both$Dim1.nobb)

# how about the second?
cor(pos_both$Dim2, pos_both$Dim2.nobb)

