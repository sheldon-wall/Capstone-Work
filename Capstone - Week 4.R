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
  
  ngram_df <- tidy_corpus %>%
    unnest_tokens(ngram, text, token = "ngrams", n = nodes) %>%
    count(ngram, sort = TRUE)

  cat( nodes, "-gram rows before =", nrow(ngram_df), "\n")
  ngram_df <- ngram_df %>% 
    mutate(base = "", predictor = "") %>%
    filter(n > drop)
  cat("After dropping rows with freq <=", drop, " = ", nrow(ngram_df), "\n")
  
  ngram_dt <- as.data.table(ngram_df)

  if (nodes > 1){
    ## split last word out as the predictor
    ngram_dt$predictor <- word(ngram_dt$ngram, start = -1)
    ngram_dt$base <- word(ngram_dt$ngram, end = nodes - 1)
  }
  else
  {
    ngram_dt$base <- ngram_dt$ngram
  }
  
  ngram_dt <- ngram_dt[,"ngram":=NULL]
  setcolorder(ngram_dt, c("base", "predictor", "n"))
  
  return(ngram_dt)
}

## Convert to a tidy corpus and accumulate n-grams
tidy_corpus <- tidy(MyCorpus)
rm(MyCorpus)

ngram_2 <- split_ngram(tidy_corpus, 2, 0)
ngram_3 <- split_ngram(tidy_corpus, 3, 1)
ngram_4 <- split_ngram(tidy_corpus, 4, 1)
ngram_5 <- split_ngram(tidy_corpus, 5, 1)
ngram_all <- rbind(ngram_2, ngram_3, ngram_4, ngram_5) 
setkey(ngram_all, base)

filename <- file.path(dataDir, "ngram_all.Rds")
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
  sentence <- gsub("[[:punct:][:blank:]]+", " ", sentence)
  sentence <- trimws(sentence, "both")
  
  ## set number of nodes to lessor of max n-grams or number of words in sentence
  num_nodes <- min(num_nodes, str_count(sentence, "\\S+"))
  
  ans <- predict_next_word(sentence, ngram_all, num_nodes)
  
  ## return top num_pred_words 
  setorder(ans, -prob)
  return(ans[1:num_pred_words])
}

## sentence contains sentence that needs to be matched within ngram_all 
## nodes contains N-Gram that we are introspecting, 

predict_next_word <- function(sentence, ngram_all, node)
{
  ## set search word 
  search_words <- word(sentence, start = -node, end = -1)
  
  ans <- ngram_all[base == search_words]
  
  ## if we found matches then 
  if (nrow(ans) > 0){
    ans[,prob:= n/sum(n)]
    if (node > 1){
      backoff <- predict_next_word(search_words, ngram_all, node - 1)
      ## stupid backoff
      backoff[,prob:=prob*.4]
      ans <- rbind(ans, backoff)
    }
  } else {
    if (node > 1){
      ## stupid backoff
      ans <- predict_next_word(search_words, ngram_all, node - 1)
      ans[,prob:=prob*.4]
    }
  }
  
  return(ans)
}

dataDir <- "./data/NLP Datasets" 

filename <- file.path(dataDir, "ngram_all.Rds")
ngram_all <- readRDS(filename)

sentence <- "adfasdf asdfa3asd 1243adsf3"
##sentence <- "Hello, how are you"
next_word <- scrub_and_predict(sentence)
next_word

sentence <- "The guy in front of me just bought a pound of bacon, a bouquet, and a case of"
next_word <- scrub_and_predict(sentence)
next_word
## correct = beer

sentence <- "You're the reason why I smile everyday. Can you follow me please? It would mean the"
next_word <- scrub_and_predict(sentence)
next_word
## correct = world

sentence <- "Hey sunshine, can you follow me and make me the"
next_word <- scrub_and_predict(sentence, ngram_all, 5)
next_word
## correct = happiest

sentence <- "Very early observations on the Bills game: Offense still struggling but the"
next_word <- scrub_and_predict(sentence, ngram_all, 5)
next_word
## correct = defense 
## mine = crowd

sentence <- "Go on a romantic date at the"
next_word <- scrub_and_predict(sentence, ngram_all, num_pred_words = 15)
next_word
## correct = beach
## mine = grocery

sentence <- "Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my"
next_word <- scrub_and_predict(sentence, ngram_all, 5)
next_word
## correct = way

sentence <- "Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some"
scrub_and_predict(sentence, ngram_all)[1:3]$predictor

## correct = time

sentence <- "After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little"
next_word <- scrub_and_predict(sentence, ngram_all, num_pred_words = 10)
next_word
## correct = fingers

sentence <- "Be grateful for the good times and keep the faith during the"
next_word <- scrub_and_predict(sentence, ngram_all, num_pred_words = 20)
next_word
## correct = bad
## mine = hard

sentence <- "If this isn't the cutest thing you've ever seen, then you must be"
next_word <- scrub_and_predict(sentence, ngram_all, num_pred_words = 100)
## correct = insane



for (i in 1:nrow(qunitgram_dt)){
  set(qunitgram_dt, i, 5L, 
      ngram_dt[i,2] / 
        ngram_dt[ngram %like% paste("^", ngram_dt[i,3], " ", sep = ""), sum(n)])
}


if (candidateIs5gram) {
  score = matched5gramCount / input4gramCount
} else if (candidateIs4gram) {
  score = 0.4 * matched4gramCount / input3gramCount
} else if (candidateIs3gram) {
  score = 0.4 * 0.4 * matched3gramCount / input2gramCount
} else if (candidateIs2gram) {
  score = 0.4 * 0.4 * 0.4 * matched2gramcount / input1gramCount
}

