library(dplyr)
library(stringr)
library(tidytext)
library(tidyr)
library(tm)
library(data.table)

## This function reads in the selected text based on the file name
## It also prints some elementry file meta data
read_text <- function(filename)
{
  ## establish a connect and read the lines in
  con <- file(filename, "rb") 
  text.file <- readLines(con, ok = TRUE, skipNul = TRUE, encoding = "latin1")
  close(con)
  
  iconv(text.file, "latin1", "ASCII", sub="")
  
  nlines <- length(text.file)
  
  ## Now for each line read - record line length
  max.length <- 0
  for (i in 1:nlines) {
    
    ## read the line of text and store the line length
    mytext <- text.file[i]
    linelength <- nchar(mytext)
    if (max.length < linelength){
      max.length <- linelength
    }
  }
  
  cat("File Name = ", filename, "\n")
  cat("File Size (MB)= ", file.info(filename)$size/1024/1024, "\n")
  cat("Number LInes = ", nlines, "\n")
  cat("Max Line Chars = ", max.length, "\n")
  
  return(text.file)
}

## Given a filename this funciton writes a sample based on a percentage supplied
sample_text <- function(input.text,sample.perc)
{
  output.text <- c("")
  nlines <- length(input.text)
  j <- 0
  
  ## for all the lines
  for (i in 1:nlines) {
    ## read the line of text and store the line length
    if (rbinom(1, 1, prob = sample.perc) == 1) {
      j <- j + 1
      output.text[j] <- input.text[i]
    }
  }
  
  return(output.text)
}

## This function performs pre-processesing on a corpus.  This involves:
## i) removing website URLs, twitter tags and names
## ii) ensures plaintextdoc chars and removes puncuation, numbers and white spaces
## iii) removes a selection of words passed along as a parameter.
##

preprocess_corpus <- function(MyCorpus, remove.words)
{
  ## remove URL's
  MyCorpus <- tm_map(MyCorpus, 
                     content_transformer(function(x) gsub("http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+", "", x)))
  
  ## remove emails
  MyCorpus <- tm_map(MyCorpus, 
                     content_transformer(function(x) gsub("\\b[A-Z a-z 0-9._ - ]*[@](.*?)[.]{1,3} \\b", "", x)))
  
  ## remove twitter tags
  MyCorpus <- tm_map(MyCorpus, 
                     content_transformer(function(x) gsub("RT |via", "", x)))
  
  ## remove twitter usernames
  MyCorpus <- tm_map(MyCorpus,
                     content_transformer(function(x) gsub("[@][a - zA - Z0 - 9_]{1,15}", "", x)))
  
  ## For the purpose of creating a Corpus which will be used 
  ## there will be no stopword removal 
  ## MyCorpus <- tm_map(MyCorpus, removeWords, stopwords("en"))  
  
  ## Remove non-ascii characters
  nonASCII <- function(x) iconv(x, "latin1", "ASCII", sub = "")
  MyCorpus <- tm_map(MyCorpus, content_transformer(nonASCII))
  
  ## Remove punctuation, numbers and whitespace
  MyCorpus <- tm_map(MyCorpus, removePunctuation)
  MyCorpus <- tm_map(MyCorpus, removeNumbers)
  MyCorpus <- tm_map(MyCorpus, stripWhitespace)
  
  ## Convert characters to lower case
  MyCorpus <- tm_map(MyCorpus, content_transformer(tolower))
  
  ## Do Not Perform stemming
  ##MyCorpus <- tm_map(MyCorpus, stemDocument)
  
  ## If word list has been supplied then remove those words
  if (!is.null(remove.words))
    MyCorpus <- tm_map(MyCorpus, removeWords, remove.words[,1])
  
  return(MyCorpus)
}

## Download the capstone data 

## set the data directory 
dataDir <- "./data/NLP Datasets" 
sampleDir <- "./data/NLP Datasets/sample"

## extract the capstone data 
dest.file <- file.path(dataDir, "capstone-data.zip")
if (!file.exists(dest.file)){
  source.file  <- "http://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
  files.extract <- c("final/en_US/en_US.blogs.txt","final/en_US/en_US.news.txt","final/en_US/en_US.twitter.txt")
  download.file(source.file, destfile=dest.file)
  unzip(dest.file, files= files.extract, exdir = dataDir, junkpaths = TRUE)
}

## get profanity data from a source found on git hub
dest.file <- file.path(dataDir, "profanity.txt")
if (!file.exists(dest.file)){
  source.file <-"https://raw.githubusercontent.com/shutterstock/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en"
  download.file(source.file, destfile=dest.file)
}

## load the blogs and create a sample
filename <- file.path(dataDir, "en_US.blogs.txt")
full.blog <- read_text(filename)
sample.blog <- sample_text(full.blog, .5)

## load the news and create a sample
filename <- file.path(dataDir, "en_US.news.txt")
full.news <- read_text(filename)
sample.news <- sample_text(full.news, .5)

## load the twitter feed and create a sample
filename <- file.path(dataDir, "en_US.twitter.txt")
full.twitter <- read_text(filename)
sample.twitter <- sample_text(full.twitter, .5)

## load the profanity data
filename <- file.path(dataDir, "profanity.txt")
profanity.data <- read.table(filename, header=FALSE, sep="\n", strip.white=TRUE)
names(profanity.data) <-"Profane Words"

sample.all <- c(sample.blog, sample.news, sample.twitter)
rm(full.blog, full.news, full.twitter)
rm(sample.blog, sample.news, sample.twitter)

MyCorpus <- VCorpus(VectorSource(sample.all))
MyCorpus <- preprocess_corpus(MyCorpus, profanity.data)
rm(sample.all)

## split the input ngram into base and predictor
## and return a data table

split_ngram <- function (tidy_corpus, nodes, drop = 3)
{
  
  ngram_n <- tidy_corpus %>%
    unnest_tokens(ngram, text, token = "ngrams", n = nodes) %>%
    count(ngram, sort = TRUE)
  
  cat( nodes, "-gram rows before =", nrow(ngram_n), "\n")
  ngram_n <- ngram_n %>% 
    mutate(base = "", predictor = "") %>%
    filter(n > drop)
  cat("After dropping rows with freq <=", drop, " = ", nrow(ngram_n), "\n")
  
  ngram_n <- as.data.table(ngram_n)
  ##rm(ngram_df)
  
  if (nodes > 0){
    ## split last word out as the predictor
    ngram_n[, predictor := word(ngram, start = -1)]
    ngram_n[, base := word(ngram, end = nodes - 1)]
  }
  else
  {
    ngram_n$base <- ngram_n$ngram
  }
  
  ngram_n <- ngram_n[,"ngram":=NULL]
  setcolorder(ngram_n, c("base", "predictor", "n"))
  
  return(ngram_n)
}

## Convert to a tidy corpus and accumulate n-grams
tidy_corpus <- tidy(MyCorpus)
rm(MyCorpus)

ngram_1 <- split_ngram(tidy_corpus, 1, 0)
ngram_1[,n:=n/sum(n)*100]
ngram_1 <- ngram_1[1:5]

## redistribute the frequency so probability is correct
ngram_1[1]$n <- 1333
ngram_1[2]$n <- 768
ngram_1[3]$n <- 672
ngram_1[4]$n <- 664
ngram_1[5]$n <- 560

ngram_2 <- split_ngram(tidy_corpus, 2, 0)
ngram_3 <- split_ngram(tidy_corpus, 3, 1)
ngram_4 <- split_ngram(tidy_corpus, 4, 1)
ngram_5 <- split_ngram(tidy_corpus, 5, 1)
ngram_all <- rbind(ngram_1, ngram_2, ngram_3, ngram_4, ngram_5) 
setkey(ngram_all, base)

filename <- file.path(dataDir, "ngram_small.Rds")
saveRDS(ngram_all, filename)

filename <- file.path(dataDir, "tidy_corpus.Rds")
saveRDS(tidy_corpus, filename)

## 2 -gram rows before = 4419794 
## 3 -gram rows before = 11328169 
## 4 -gram rows before = 15221737 
## 5 -gram rows before = 15993045 

scrub_and_predict <- function(sentence, num_nodes = 5, num_pred_words = 5)
{
  ## retrieve last number of words in sentence to start applying ngram search
  sentence <- tolower(sentence)
  sentence <- gsub("'", "", sentence)
  sentence <- gsub("[[:punct:][:blank:]]+", " ", sentence)
  sentence <- trimws(sentence, "both")
  
  ## set number of nodes to lessor of max n-grams or number of words in sentence
  num_nodes <- min(num_nodes, str_count(sentence, "\\S+"))
  
  ans <- predict_next_word(sentence, num_nodes)
  
  setkey(ans, predictor)
  ans <- subset(ans, !duplicated(predictor))
  
  ## return top num_pred_words 
  setorder(ans, -prob)
  return(ans[1:num_pred_words])
}

## sentence contains sentence that needs to be matched within ngram_all 
## nodes contains N-Gram that we are introspecting, 

predict_next_word <- function(sentence, node)
{
  ## set search word 
  search_words <- word(sentence, start = -node, end = -1)
  ## if we are completely done backing off - then fill with an empty string
  if (is.na(search_words)){
    search_words <- ""
  }
    
  ans <- ngram_all[base == search_words]
  
  ## if we found matches then 
  if (nrow(ans) > 0){
    ans[,prob:= n/sum(n)]
    if (node > 0){
      backoff <- predict_next_word(search_words, node - 1)
      ## stupid backoff
      backoff[,prob:=prob*.4]
      ans <- rbind(ans, backoff)
    }
  } else {
    if (node > 0){
      ## stupid backoff
      ans <- predict_next_word(search_words, node - 1)
      ans[,prob:=prob*.4]
    }
  }
  
  return(ans)
}

dataDir <- "./data/NLP Datasets" 

filename <- file.path(dataDir, "ngram_small.Rds")
ngram_all <- readRDS(filename)

## sentences from Quiz #2

# Question 1. 
sentence <- "When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd"
next_word <- scrub_and_predict(sentence, num_pred_words = 10000)
next_word[predictor %in% c("give", "sleep", "eat", "die")]
## give ** (wrong)
## sleep
## eat
## die ++

## Question 2
sentence <- "Guy at my table's wife got up to go to the bathroom and I asked about dessert and he started telling me about his"
next_word <- scrub_and_predict(sentence, num_pred_words = 10000)
next_word[predictor %in% c("spiritual", "marital", "horticultural", "financial")]
## horticultural
## financial ** (wrong)
## marital &&
## spiritual ++ (wrong 2)

## Question 3
sentence <- "I'd give anything to see arctic monkeys this"
next_word <- scrub_and_predict(sentence, num_pred_words = 10000)
next_word[predictor %in% c("morning", "decade", "weekend", "month")]
## morning ** (wrong)
## decade
## weekend ++
## month

## Question 4
sentence <- "Talking to your mom has the same effect as a hug and helps reduce your"
next_word <- scrub_and_predict(sentence, num_pred_words = 10000)
next_word[predictor %in% c("hunger", "stress", "sleepiness", "happiness")]
## hunger
## stress && 
## sleepiness
## happiness ** (wrong)

## Question 5
sentence <- "When you were in Holland you were like 1 inch away from me but you hadn't time to take a"
next_word <- scrub_and_predict(sentence, num_pred_words = 10000)
next_word[predictor %in% c("picture", "minute", "walk", "look")]
next_word
## picture ++
## minute
## walk
## look ** (wrong)

## Question 6
sentence <- "I'd just like all of these questions answered, a presentation of evidence, and a jury to settle the"
next_word <- scrub_and_predict(sentence, num_pred_words = 10000)
next_word[predictor %in% c("account", "case", "matter", "incident")]
## account
## case ** (wrong)
## matter ++
## incident

## Question 7
sentence <- "I can't deal with unsymetrical things. I can't even hold an uneven number of bags of groceries in each"
next_word <- scrub_and_predict(sentence, num_pred_words = 10000)
next_word[predictor %in% c("finger", "hand", "arm", "toe")]
## finger
## hand **
## arm
## toe

## Question 8
sentence <- "Every inch of you is perfect from the bottom to the"
next_word <- scrub_and_predict(sentence, num_pred_words = 10000)
next_word[predictor %in% c("middle", "side", "center", "top")]
## middle
## side
## center
## top **

## Question 9
sentence <- "I'm thankful my childhood was filled with imagination and bruises from playing"
next_word <- scrub_and_predict(sentence, num_pred_words = 10000)
next_word[predictor %in% c("outside", "inside", "daily", "weekly")]
## outside **
## inside
## daily
## weekly

## Question 10
sentence <- "I like how the same people are in almost all of Adam Sandler's"
next_word <- scrub_and_predict(sentence, num_pred_words = 10000)
next_word[predictor %in% c("movies", "stories", "novels", "pictures")]
## TOP result is films.  None for these but movies is a good substitute
## movies **
## stories
## novels
## pictures

## Benchmark 1
## Overall top-3 score: 16.78 %
## Overall top-1 precision: 12.90 %
## Overall top-3 precision: 20.18 %
## Average runtime: 9.55 msec
## Number of predictions: 28464
## Total memory used: 1327.58 MB

## Dataset details
## Dataset "blogs" (599 lines, 14587 words, hash 14b3c593e543eb8b2932cf00b646ed653e336897a03c82098b725e6e1f9b7aa2)
## Score: 16.12 %, Top-1 precision: 11.97 %, Top-3 precision: 19.87 %
## Dataset "tweets" (793 lines, 14071 words, hash 7fa3bf921c393fe7009bc60971b2bb8396414e7602bb4f409bed78c7192c30f4)
## Score: 17.44 %, Top-1 precision: 13.82 %, Top-3 precision: 20.49 %

## - second benchmark after fixing duplicates, mass of unigrams, apostraphes

##Overall top-3 score:     18.35 %
##Overall top-1 precision: 13.78 %
##Overall top-3 precision: 22.28 %
##Average runtime:         12.97 msec
##Number of predictions:   28464
##Total memory used:       1229.52 MB

##Dataset details
##Dataset "blogs" (599 lines, 14587 words, hash 14b3c593e543eb8b2932cf00b646ed653e336897a03c82098b725e6e1f9b7aa2)
##Score: 18.05 %, Top-1 precision: 13.33 %, Top-3 precision: 22.09 %
##Dataset "tweets" (793 lines, 14071 words, hash 7fa3bf921c393fe7009bc60971b2bb8396414e7602bb4f409bed78c7192c30f4)
##Score: 18.65 %, Top-1 precision: 14.23 %, Top-3 precision: 22.46 %


## benchmark after removing n > 1 (mainly bigrams)

##Overall top-3 score:     18.38 %
##Overall top-1 precision: 13.80 %
##Overall top-3 precision: 22.31 %
##Average runtime:         9.77 msec
##Number of predictions:   28464
##Total memory used:       1018.00 MB

##Dataset details
##Dataset "blogs" (599 lines, 14587 words, hash 14b3c593e543eb8b2932cf00b646ed653e336897a03c82098b725e6e1f9b7aa2)
##Score: 18.12 %, Top-1 precision: 13.40 %, Top-3 precision: 22.16 %
##Dataset "tweets" (793 lines, 14071 words, hash 7fa3bf921c393fe7009bc60971b2bb8396414e7602bb4f409bed78c7192c30f4)
##Score: 18.65 %, Top-1 precision: 14.20 %, Top-3 precision: 22.46 %

## benchmark after removing n > 2 (all n-grams)
##Overall top-3 score:     18.29 %
##Overall top-1 precision: 13.78 %
##Overall top-3 precision: 22.18 %
##Average runtime:         8.03 msec
##Number of predictions:   28464
##Total memory used:       763.08 MB

##Dataset details
##Dataset "blogs" (599 lines, 14587 words, hash 14b3c593e543eb8b2932cf00b646ed653e336897a03c82098b725e6e1f9b7aa2)
##Score: 18.03 %, Top-1 precision: 13.40 %, Top-3 precision: 22.03 %
##Dataset "tweets" (793 lines, 14071 words, hash 7fa3bf921c393fe7009bc60971b2bb8396414e7602bb4f409bed78c7192c30f4)
##Score: 18.55 %, Top-1 precision: 14.16 %, Top-3 precision: 22.32 %