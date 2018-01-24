library(dplyr)
library(stringr)
library(data.table)

scrub_and_predict_kneser <- function(sentence, num_nodes = 5, num_pred_words = 5)
{
  ## retrieve last number of words in sentence to start applying ngram search
  sentence <- tolower(sentence)
  sentence <- gsub("'", "", sentence)
  sentence <- gsub("[[:punct:][:blank:]]+", " ", sentence)
  sentence <- trimws(sentence, "both")
  
  ## set number of nodes to lessor of max n-grams or number of words in sentence
  num_nodes <- min(num_nodes, str_count(sentence, "\\S+"))
  
  ## this is the tallest
  ans <- ngram_all[base == sentence]
  nlines <- nrow(ans)
  
  denom <- ans[,sum(n)]
  setorder(ans, -n)
  for (i in 1:min(nlines, num_pred_words)) {
    
    discount <- ngram_discount[num_nodes]
      
    ## this is the tallest building
    first_term <- max((ans[i]$n - discount), 0) / denom
    
    num_nodes <- str_count(ans[i]$base, "\\S+")
    search_words <- word(ans[i]$base, 2, end = -1)
    
    continuous_prob <- prob_next_word_kneser(ans[i]$predictor, search_words, num_nodes - 1)
    
    lambda <- (discount * ngram_all[predictor == search_words, sum(n)]) / 
      ngram_all[base == search_words, sum(n)]
    
    second_term <- lambda * continuous_prob
    
    ans[,prob:= first_term + second_term] 
  }
  
  setkey(ans, predictor)
  ans <- subset(ans, !duplicated(predictor))
    
  ## return top num_pred_words 
  setorder(ans, -prob)
  return(ans[1:num_pred_words])
}

prob_next_word_kneser <- function(predicted_word, base_sentence, num_nodes)
{
  ## if the last node - then calculate the final term and start passing it back
  if (num_nodes == 0){
    search_words <- paste(predicted_word,"$", sep = "")
    ## number of unique words preceeding predictor divided by unique bigrams
    first_term <- (ngram_all[base %like% search_words, .N]) / ngram_all[ngram_type == 2, .N]
    second_term <- 0
  }
  else {
    discount <- ngram_discount[num_nodes]
    
    full_word <- paste(base_sentence, predicted_word, sep = " ")
    
    ## number of unique words preceeding base_sentence
    search_words <- paste(base_sentence, "$", sep = "")
    denom <- (ngram_all[base %like% search_words, .N])
    
    ## calculate the first term which is the 
    ## number of unique words precdeding base_sentence + predicted  less discount
    ## divided by unique words preceeding base sentence
    search_words <- paste(full_word, "$", sep = "")
    first_term <- max(ngram_all[base %like% search_words, .N] - discount, 0) / denom
    
    search_words <- word(base_sentence, 2, end = -1)
    continuous_prob <- prob_next_word_kneser(predicted_word, search_words, num_nodes - 1)
    
    search_words <- paste("^", base_sentence, " ", sep = "")
    lambda <- (discount * ngram_all[base %like% search_words, sum(n)]) / denom
    
    second_term <- lambda * continuous_prob
    
  }
  return(first_term + second_term)
}



dataDir <- "./data/NLP Datasets" 

filename <- file.path(dataDir, "ngram_all.Rds")
ngram_all <- readRDS(filename)

ngram_all[str_count(base, "\\S+") == 1, ngram_type:=1]
ngram_all[str_count(base, "\\S+") == 2, ngram_type:=2]
ngram_all[str_count(base, "\\S+") == 3, ngram_type:=3]
ngram_all[str_count(base, "\\S+") == 4, ngram_type:=4]
ngram_all[str_count(base, "\\S+") == 5, ngram_type:=5]

num_nodes <- 5

ngram_discount <- 1:num_nodes
ngram_discount[1:num_nodes] <- 0

## loop through all the predictors and create a probability 
## this is the tallest building then this is the tallest skyscraper
for (i in 1:num_nodes){
  ngram_discount[i] <- (ngram_all[n == 1 & ngram_type == i, .N]) / 
    ((ngram_all[n == 1 & ngram_type == i, .N]) + 2*(ngram_all[n == 2 & ngram_type == i, .N]))
}

setkey(ngram_all, base)
sentence <- "This is the"
next_word <- scrub_and_predict_kneser(sentence)

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


