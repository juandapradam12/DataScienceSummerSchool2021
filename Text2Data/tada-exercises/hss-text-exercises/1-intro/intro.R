## ----------------------------------------------------------------------------------------------
library(tidyverse)

library(quanteda) # for working with corpora
library(quanteda.textplots) # for plotting 'keyness'
library(readtext) # for getting documents and their info into a data frame 


## ----------------------------------------------------------------------------------------------
manifestos <- readtext("texts/uk-election-manifestos/")
head(manifestos)


## ----------------------------------------------------------------------------------------------
docinfo <- 
manifestos <- readtext("texts/uk-election-manifestos/",
                       docvarsfrom = "filenames",
                       docvarnames = c("country", "national", "year", "language", "party"))
head(manifestos)


## ----------------------------------------------------------------------------------------------
# easiest to use dplyr here
manifestos <- select(manifestos, doc_id, text, year, party) 
manifestos


## ----------------------------------------------------------------------------------------------
# Summary od texts
manif_corp <- corpus(manifestos) 
manif_corp


## ----------------------------------------------------------------------------------------------
# the top few docvars
head(docvars(manif_corp))


## ----------------------------------------------------------------------------------------------
tail(docvars(manif_corp, "party"))


## ----------------------------------------------------------------------------------------------
txts <- as.character(manif_corp) # don't type txts !
# show the first 100 characters of the first text
substr(txts[1], 1, 100)


## ----------------------------------------------------------------------------------------------
# summary restricted to the first 10 documents
summary(manif_corp, n = 10)


## ---- eval = FALSE-----------------------------------------------------------------------------
ndoc(manif_corp) # document count

docnames(manif_corp) # unique document identifiers
ntype(manif_corp) # types in each document
ntoken(manif_corp) # tokens in each document
nsentence(manif_corp) # sentences in each documenta


## ----------------------------------------------------------------------------------------------
main_parties <- c("Lab", "Con", "LD")
manif_subcorp <- corpus_subset(manif_corp, 
                               year > 2000 & party %in% main_parties)
summary(manif_subcorp)


## ----------------------------------------------------------------------------------------------
partycorp <- corpus_group(manif_subcorp, groups = party)
summary(partycorp)


## ----------------------------------------------------------------------------------------------
load("data/corpus_bara_speaker.rda")
summary(corpus_bara_speaker)


## ----------------------------------------------------------------------------------------------
no_corp <- corpus_subset(corpus_bara_speaker, 
                         vote == "no" & ntoken(corpus_bara_speaker) > 100)
no_corp


## ----------------------------------------------------------------------------------------------
para_corp <- corpus_reshape(corpus_bara_speaker, 
                            to = "paragraphs") # or "sentences"
head(summary(para_corp)) # Just the top few lines


## ----------------------------------------------------------------------------------------------
toks <- tokens(corpus_bara_speaker)



## ----------------------------------------------------------------------------------------------
library(quanteda.textstats)

colls <- textstat_collocations(toks)
head(colls, 20)


## ----------------------------------------------------------------------------------------------
stps <- stopwords() 
head(stps)




## ----------------------------------------------------------------------------------------------
toks2 <- tokens_remove(toks, stps, padding = TRUE)

toks2
## ----------------------------------------------------------------------------------------------
toks2[[1]][1:20] # first 20 tokens of document 1



## ----------------------------------------------------------------------------------------------
coll2 <- textstat_collocations(toks2, tolower = FALSE, size = 2)
head(coll2, 20)


## ----------------------------------------------------------------------------------------------
coll3 <- textstat_collocations(toks2, tolower = FALSE, size = 3)
head(coll3, 30)


## ----------------------------------------------------------------------------------------------
toks <- tokens(corpus_bara_speaker)
kw_mother <- kwic(toks, "mother*", window = 10)
head(kw_mother)


## ---- eval = FALSE-----------------------------------------------------------------------------
View(kw_mother)


## ----------------------------------------------------------------------------------------------
medprof <- kwic(toks, phrase("medical profession"))

## --

kw_speaker <- kwic(toks, "Speaker")
kw_speaker

## ---- eval = FALSE-----------------------------------------------------------------------------
phrases <- phrase(c("medical profession", "human life")) # make a phrase
toks <- tokens_compound(toks, phrases) # make the tokens show it as _ connected


## ---- eval = FALSE-----------------------------------------------------------------------------
kwic(toks, "medical_profession")


## ----------------------------------------------------------------------------------------------
corpdfm <- dfm(toks) # lowercases by default, but not much more
dim(corpdfm)
featnames(corpdfm)[1:40] # really just colnames
docnames(corpdfm)


## ----------------------------------------------------------------------------------------------
toks <- tokens(corpus_bara_speaker, ## yes, yes
               remove_punct = TRUE, 
               remove_numbers = TRUE)
toks <- tokens_remove(toks, stps) # those stopwords we saw earlier

corpdfm <- dfm(toks)
dim(corpdfm) # a bit smaller
featnames(corpdfm)[1:40]


## ----------------------------------------------------------------------------------------------
stoks <- tokens(corpus_bara_speaker, ## yes, yes
                remove_punct = TRUE, 
                remove_numbers = TRUE)
stoks <- tokens_wordstem(stoks)

scorpdfm <- dfm(stoks)
dim(scorpdfm) # a bit smaller
featnames(scorpdfm)[1:40]


## ----------------------------------------------------------------------------------------------
smallcorpdfm <- dfm_trim(corpdfm, min_termfreq = 5, min_docfreq = 5)
dim(smallcorpdfm) # this might have been a bit drastic...


## ----------------------------------------------------------------------------------------------
dfm_subset(corpdfm, vote != "abs") # remove abstentions


## ----------------------------------------------------------------------------------------------
dfm_votes <- dfm_group(corpdfm, vote)
dfm_votes


## ----------------------------------------------------------------------------------------------
corpdfm_yesno <- dfm_subset(corpdfm, vote != "abs")

textstat_frequency(corpdfm_yesno, 
                   n = 20, groups = vote)


## ----------------------------------------------------------------------------------------------
dfm_yesno <- dfm_group(corpdfm_yesno, vote)



## ----------------------------------------------------------------------------------------------
no_terms <- textstat_keyness(dfm_yesno, "no")
head(no_terms, 25)


## ----------------------------------------------------------------------------------------------
yes_terms <-  textstat_keyness(dfm_yesno, "yes")
head(yes_terms, 25)


## ----------------------------------------------------------------------------------------------
textplot_keyness(yes_terms)


## ----------------------------------------------------------------------------------------------
nsjs_terms <- textstat_keyness(corpdfm, "Mr Norman St John-Stevas")
head(nsjs_terms, 10)

