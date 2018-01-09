library(dplyr)
library(stringr)
library(tidytext)
library(tidyr)
library(tm)
library(urltools)
library(ggplot2)
library(grid)
library(reshape2)

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

generate_ngram_df <- function(tdm)
{
  condense_tdm <- removeSparseTerms(tdm,0.999)
  in_matrix <- as.matrix(condense_tdm)
  freq <- as.data.frame(rowSums(in_matrix)) 
  freq$words <- row.names(freq)
  colnames(freq)[1] <- "counts"
  # sorting based on word occurrences
  freq_ordered <- freq[order(-freq[,1]), ]        
}

# N-gram tokenization
BigramTokenizer <- function(x) RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 2, max = 2))
TrigramTokenizer <- function(x) RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 3, max = 3))

unigram <- generate_ngram_df(TermDocumentMatrix(MyCorpus))
bigram <- generate_ngram_df(TermDocumentMatrix(MyCorpus, control = list(tokenize = BigramTokenizer)))
trigram <- generate_ngram_df(TermDocumentMatrix(MyCorpus, control = list(tokenize = TrigramTokenizer)))


