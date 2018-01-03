library(dplyr)
library(stringr)
library(tidytext)
library(tidyr)
library(tm)
library(urltools)
library(wordcloud)
library(ggplot2)
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

## This extracts website information based on text passed as a parameter 
## that match the pattern passed into it

extract_website <- function(text)
{
  
  pattern <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"
  
  index <- str_which(text, pattern)
  website.list <- str_extract_all(text[index], pattern)
  website.df <- url_parse("")
  
  ## for all content found with a website - parse out the occurences
  for (i in 1:length(website.list)) {
    ## read the line of text and store the line length
    for (j in 1:length(website.list[[i]])){
      url <- url_parse(website.list[[i]][j])
      ##      if (url$domain == "bit.ly"){
      ##        bitly_LinksExpand(shortURL = str_c(url$scheme, "://", 
      ##                                           url$domain, "/", 
      ##                                           url$path,
      ##                                           collapse = ""), 
      ##      }
      website.df <- bind_rows(website.df, url)
    }
  }
  
  return(website.df %>% filter(!is.na(domain)))
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

  ## Perform stemming
  MMyCorpus <- tm_map(MyCorpus, stemDocument)
    
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
sample.twitter <- sample_text(full.news, .1)

## load the profanity data
filename <- file.path(dataDir, "profanity.txt")
profanity.data <- read.table(filename, header=FALSE, sep="\n", strip.white=TRUE)
names(profanity.data) <-"Profane Words"

## Extract and store all the web site information for sake of examination 
## before we remove it from the text
web.sites <- extract_website(full.blog)
web.sites <- bind_rows(web.sites, extract_website(full.news))
web.sites <- bind_rows(web.sites, extract_website(full.twitter))

## Remove full size text vectors
rm(full.blog, full.news, full.twitter)

## Load the blog text into a Corpus
MyCorpus.blog <- VCorpus(VectorSource(sample.blog))
meta(MyCorpus.blog, "description", type = "indexed") <- "Blog Text - English Language"
meta(MyCorpus.blog, "document_id", type = "indexed") <- "Blog Text"
MyCorpus.blog <- preprocess_corpus(MyCorpus.blog, profanity.data)

## tokenize and make tidy
dtm.blog <- DocumentTermMatrix(MyCorpus.blog)
tidy_blog <- tidy(dtm.blog) %>% mutate(document = "Blog")

## Load the news text into a Corpus
MyCorpus.news <- VCorpus(VectorSource(sample.news))
meta(MyCorpus.news, "description", type = "indexed") <- "News Text - English Language"
meta(MyCorpus.news, "document_id", type = "indexed") <- "News Text"
MyCorpus.news <- preprocess_corpus(MyCorpus.news, profanity.data)

## tokenize and make tidy
dtm.news <- DocumentTermMatrix(MyCorpus.news)
tidy_news <- tidy(dtm.news) %>% mutate(document = "News")

## Load the twitter text into a Corpus
MyCorpus.twitter <- VCorpus(VectorSource(sample.twitter))
meta(MyCorpus.news, "description", type = "indexed") <- "Twitter Text - English Language"
meta(MyCorpus.news, "document_id", type = "indexed") <- "Twitter Text"
MyCorpus.twitter <- preprocess_corpus(MyCorpus.twitter, profanity.data)

## tokenize and make tidy
dtm.twitter <- DocumentTermMatrix(MyCorpus.twitter)
tidy_twitter <- tidy(dtm.twitter) %>% mutate(document = "Twitter")

## Let's plot the most common words - excluding stop words

## Read in the stopwords data set and change column name to 'term'
data("stop_words")
stop_words <- stop_words %>%
  transmute(term = word, lexicon)

## Show distribution of the top words - not including stop words
bind_rows(tidy_blog, tidy_news, tidy_twitter) %>%
  anti_join(stop_words, by='term') %>%
  count(term, sort = TRUE) %>%
  top_n(25, n) %>%
  mutate(term = reorder(term, n)) %>%
  ggplot(aes(x = term, y = n, fill = n)) +
  geom_bar(stat = "identity", colour = "Black") +
  labs(title = "Top 25 words overall (not including stop words)", 
       x = "Word", y = "Num occurences") +
  coord_flip()

## generate a word cloud of the top 25 frequently used words
bind_rows(tidy_blog, tidy_news, tidy_twitter) %>%
  anti_join(stop_words, by='term') %>%
  count(term, sort = TRUE) %>%
  top_n(25, n) %>%
  mutate(term = reorder(term, n)) %>%
  with(wordcloud(term, n, scale=c(3,.2), max.words = 25, random.order = FALSE, 
                 random.color = FALSE, rot.per = .5, colors = rainbow(10)))

## plot the top 5 most frequently used words for each media type
## not including all stopwords from all lexicons

bind_rows(tidy_blog, tidy_news, tidy_twitter) %>%
  anti_join(stop_words, by='term') %>%
  group_by(document) %>%
  count(term, sort = TRUE) %>%
  top_n(5, n) %>%
  mutate(term = reorder(term, n)) %>%
  ggplot(aes(x= term, y = n, fill = document, order = document)) +
  geom_bar(stat = "identity", colour = "black") +
  labs(title = "Top 5 words by media type", 
       x = "Word", y = "Num occurences") +
  facet_wrap(~document, ncol = 1)

## Word frequency distribution plots - which is the number of times a word appears 
## in a media channel divided by the total number of terms in that channel

text_words <- bind_rows(tidy_blog, tidy_news, tidy_twitter) %>%
  group_by(document) %>%
  count(term, sort = TRUE) %>%
  ungroup()
  
document_words <- text_words %>%
  group_by(document) %>%
  summarise(total = sum(n))

text_words <- left_join(text_words, document_words)

ggplot(text_words, aes(n/total, fill = document)) + 
  geom_histogram(show.legend = FALSE) + 
  xlim(NA, 0.0009) + 
  labs(title = "Term Frequency - distribution of n/total for each media type", 
       x = "Word", y = "Num occurences") +
  facet_wrap(~document, ncol = 1, scales = "free_y")

## Zipf's law states that the frequency that a term appears is inversely 
## proportional to its rank

freq_by_rank <- text_words %>% 
  group_by(document) %>%
  mutate(rank = row_number(),
         term_frequency = n/total)

rank_subset <- freq_by_rank %>%
  filter(rank < 500, 
         rank > 10)

fit <- lm(log10(term_frequency) ~ log10(rank), data=rank_subset)

freq_by_rank %>%  
  ggplot(aes(rank, term_frequency, color = document)) + 
  geom_line(size = 1.1, alpha = 0.8, show.legend = TRUE) + 
  geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2], 
              color = "gray50", linetype = 2) +
  labs(title = "Zipf's Law for media type", 
       x = "Term Rank", y = "Term Frequency") +
  scale_x_log10() + 
  scale_y_log10(labels = scales::percent) 

## Analyzing high tf-idf words
## words that are common but not too common

text_words <- text_words %>%
  bind_tf_idf(term, document, n)

text_words %>%
  select(-total) %>%
  arrange(desc(tf_idf))

text_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(term = factor(term, levels = rev(unique(term)))) %>% 
  group_by(document) %>% 
  top_n(15) %>% 
  ungroup %>%
  ggplot(aes(term, tf_idf, fill = document)) +
  geom_col(show.legend = FALSE) +
  labs(title = "Important Word Analysis (TF-IDF)",
       x = "Term", y = "tf-idf score") +
  facet_wrap(~document, ncol = 2, scales = "free") +
  coord_flip()

## analysis ends here so far

mytidytext <- tidy(MyCorpus.twitter)

bigram_twitter <- tidy(MyCorpus.twitter) %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  transmute(document = "Twitter", bigram)

bigram_news <- tidy(MyCorpus.news) %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  transmute(document = "News", bigram)

bigram_blog <- tidy(MyCorpus.blog) %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  transmute(document = "Blog", bigram)

bind_rows(bigram_blog, bigram_news, bigram_twitter) %>%
  separate(bigram, c("term1", "term2"), sep = " ") %>%
  filter(!term1 %in% stop_words$term,
         !term2 %in% stop_words$term) %>%
  group_by(document) %>%
  count(term1, term2, sort = TRUE) %>%
  bind_tf_idf(bigram, document, n) %>%
  arrange(desc(tf_idf))

## end of analysis here

tidy_corpus %>%
  count(word, sort = TRUE) %>%
  top_n(5, n) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

find


tidy_term %>% 
  group_by(document) %>%
  count(term, sort = TRUE) %>%
  top_n(5, n) %>%
  ggplot(aes(x= term, y = n, fill = document, order = document)) + 
  geom_bar(stat = "identity", colour = "black") +
  labs(title = "Top 5 words by document type", 
       x = "Word", y = "Num occurences") +
  facet_wrap(~document, ncol = 1)


## Now show top 5 websites imbedded in blogs, news and twitter



web.sites %>%
  count(domain, sort = TRUE) %>%
  top_n(n = 5) %>%
  mutate(domain = reorder(domain, n)) %>%
  ggplot(aes(domain, n)) +
  geom_bar(stat = "identity", colour = "black") +
  labs(title = "Top 5 websites", 
       x = "Website", y = "Num occurences") +
  xlab(NULL) +
  coord_flip()



