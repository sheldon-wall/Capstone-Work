library(dplyr)
library(stringr)
library(tidytext)
library(tidyr)
library(tm)
library(data.table)
library(sqldf)

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
sample.blog <- sample_text(full.blog, .1)

## load the news and create a sample
filename <- file.path(dataDir, "en_US.news.txt")
full.news <- read_text(filename)
sample.news <- sample_text(full.news, .1)

## load the twitter feed and create a sample
filename <- file.path(dataDir, "en_US.twitter.txt")
full.twitter <- read_text(filename)
sample.twitter <- sample_text(full.twitter, .1)

## load the profanity data
filename <- file.path(dataDir, "profanity.txt")
profanity.data <- read.table(filename, header=FALSE, sep="\n", strip.white=TRUE)
names(profanity.data) <-"Profane Words"

sample.all <- c(sample.blog, sample.news, sample.twitter)
MyCorpus <- VCorpus(VectorSource(sample.all))
MyCorpus <- preprocess_corpus(MyCorpus, profanity.data)

unigram_df <- tidy(MyCorpus) %>%
  unnest_tokens(unigram, text, token = "ngrams", n = 1) %>%
  count(unigram, sort = TRUE)

bigram_df <- tidy(MyCorpus) %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  mutate(type = 2, bigram) %>%
  count(bigram, sort = TRUE)

bigram_df <- bigram_df %>%
  mutate(prob = n / nrow(bigram_df)) %>% 

trigram_df <- tidy(MyCorpus) %>% 
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  select(trigram) %>%
  count(trigram, sort = TRUE)

setDT(trigram_df)
setDT(bigram_df)

trigram_df %>% select(trigram)

trigram_dt <- trigram_df %>%
  mutate(prob = n / nrow(trigram_df))

quadgram_df <- tidy(MyCorpus) %>% 
  unnest_tokens(quadgram, text, token = "ngrams", n = 4) %>%
  select(quadgram) %>%
  count(quadgram, sort = TRUE)

quadgram_df <- quadgram_df %>%
  mutate(prob = n / nrow(quadgram_df))

my_text <- "you had me"

sqldf("select * from quadgram_df where guadgram like '%you had me%'")


row.names(a10r) <- NULL




split_ngram <- function (ngram, nodes)
{

  ## loop through for all the rows in the dataframe
  ## split the frame by n-first words and 
  new_ngram <- ngram %>%
    separate(1, c("base", "prediction"), -1) 
  
  split_node <- nodes - 1
  
  word(ngram[n,1], start = -1)
  word(ngram[n,1], end = split_node)
    
  
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

