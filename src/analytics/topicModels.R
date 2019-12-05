

# DATE: 26.09.2019
# Topic Modelling explorations

library(plyr)
library(tidyverse)
library(stringr)
library(stringi) 
library(rdrop2)
library(openssl)
library(lubridate)
library(quanteda)
library(topicmodels)

# DropBox Info:
# httr-oauth created that uses the NRW2019 Team DropBox, data 4good Folder

source("R Code/AnalyseUtilities.R")


load("Data/ProcessedData/PolPosts.RData")
load("Data/ProcessedData/UserComments.RData")

allData <- bind_rows(PolPosts, UserComments) %>%
  select(Name, textID, text, Level, Site)

polData <- PolPosts  %>%
  select(Name, textID, text, Level, Site)

userData <- UserComments %>%
  select(Name, textID, text, Level, Site)



corpusData <- polData

NRWcorpus <- corpus(corpusData)
summary(NRWcorpus)[c(1, 100, 500), ]

NRWdfm <- dfm(NRWcorpus, remove_punct = TRUE, remove = stopwords('de')) %>% 
  dfm_remove(c('*-time', '*-timeUpdated', 'GMT', 'BST', "http://", "https://", "t.co")) %>% 
  dfm_trim(min_termfreq = 0.75, termfreq_type = "quantile", 
           max_docfreq = 0.4, docfreq_type = "prop")

# get rid of those elements that now don't contain any tokens anymore
NRWdfm <- NRWdfm[ntoken(NRWdfm) > 0,]


# convert to a topicmodels object, and run LDA
NRWdtm <- convert(NRWdfm, to = "topicmodels")

# This takes a while ... 12:52 until 13:00
NRWlda <- LDA(NRWdtm, k = 30)




