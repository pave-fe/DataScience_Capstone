############################################
## Michael Hulin
## August 2018
## Coursera Data Science Capstone Project
## Word Prediction Application
## Uses 4 ngram models and a backoff algorithm to determine the most likely next word
## Shiny app: https://pavefe.shinyapps.io/Word_predictor/
## Presentation: 
############################################

library(quanteda)
library(readtext)
library(tm)
library(R.utils)
library(dplyr)
library(parallel)
library(stringi)
library(stringr)
library(tidytext)
library(tidyr)
library(qdap)
library(wordcloud2)
library(data.table)

quanteda_options(threads = 7)

setwd("~/Coursera/Capstone/Project")
doc.twitter <- "~/Coursera/Capstone/Project/data/en_US/en_US.twitter.txt"
doc.blog <- "~/Coursera/Capstone/Project/data/en_US/en_US.blogs.txt"
doc.new <- "~/Coursera/Capstone/Project/data/en_US/en_US.news.txt"
doc.badwords <- "~/Coursera/Capstone/Project/data/badwords.txt"


twitter.con <- file(doc.twitter, open = "rb")
twitter <- readLines(twitter.con, skipNul = TRUE, encoding="UTF-8")
close(twitter.con)

news.con <- file(doc.new, open = "rb")
news <- readLines(news.con, skipNul = TRUE, encoding="UTF-8")
close(news.con)

blog.con <- file(doc.blog, open = "rb")
blogs <- readLines(blog.con, skipNul = TRUE, encoding="UTF-8")
close(blog.con)

# Load bad words file for profanity filtering
bw.con <- file(doc.badwords, open = "rb") 
badwords <- readLines(bw.con, skipNul = TRUE, encoding="UTF-8")
close(bw.con); rm(bw.con)

## Clean up input files
rm(doc.twitter, doc.blog, doc.new, doc.badwords,  twitter.con, blog.con, news.con)

### Load Data Cleaning Functions  ####
removeURL <- function(x) gsub("http|www[[:alnum:][:punct:]]*", "", x)


clean_text <- function (textcp) {
  textcp <- str_replace_all(textcp, "[^0-9A-Za-z///' ]", "")  ## needed to remove hex codes
  textcp <- removeURL(textcp)
  textcp <- bracketX(textcp)
  textcp <- stripWhitespace(textcp)
  textcp <- replace_contraction(textcp)
  textcp <- removeNumbers(textcp)
  textcp <- replace_symbol(textcp)
  textcp <- tolower(textcp)
  textcp <- str_replace_all(textcp, "[0-9A-Za-z]{18,}", "")  ## removes long words
  textcp <- removeWords(textcp, badwords)
  textcp
}

clean <- function(x, y=1) {
  s_clean <- clean_text(x) 
  toks <- tokens(s_clean, remove_numbers = TRUE, remove_punct = TRUE,
                 remove_symbols = TRUE, remove_separators = TRUE,
                 remove_twitter = TRUE, remove_hyphens = TRUE, remove_url = TRUE,
                 ngrams = y, concatenator = " ")
  ngram_freq <- toks %>%
                dfm() %>%
                dfm_sort(decreasing = T) %>%
                dfm_trim(min_termfreq = 5) %>%  # Initially remove terms occuring less than 5 times
                textstat_frequency()
  return(ngram_freq)
}


concat <- function(x, y, z) {
  x <- x[, 1:2]
  y <- y[, 1:2]
  z <- z[, 1:2]
  res <- aggregate(frequency~feature, rbind(x,y), sum)
  res <- aggregate(frequency~feature, rbind(res,z), sum)
  # sort and rename first column
  res <- res %>% arrange(desc(frequency)) 
             %>% dplyr::rename(terms=feature)
  res <- data.table(res)   ## convert to table for better performance
  return(res)
}

### Create unigrams  ##########

twit1 <- clean(x=twitter, y=1)
blog1 <- clean(x=blogs, y=1)
news1 <- clean(x=news, y=1)
df_1 <- concat(twit1, blog1, news1)
df_1 <-df_1[df_1$frequency>=100, ]  #trim the file
## clean up files
rm(twit1, blog1, news1)
saveRDS(df_1, file = "~/Coursera/Capstone/Project/data/df_1.rds")

## Create Bigrams  ##########
twit2 <- clean(x=twitter, y=2)
blog2 <- clean(x=blogs, y=2)
news2 <- clean(x=news, y=2)
df_2 <- concat(twit2, blog2, news2)
df_2 <- df_2[df_2$frequency>=100, ]  #trim the file
saveRDS(df_2, file = "~/Coursera/Capstone/Project/data/df_2.rds")
## clean up files
rm(df_2x, twit2, blog2, news2)

## create Trigrams  ##########
twit3 <- clean(x=twitter, y=3)
blog3 <- clean(x=blogs, y=3)
news3 <- clean(x=news, y=3)
df_3 <- concat(twit3, blog3, news3)
df_3 <- df_3[df_3$frequency>=10, ]  #trim the file
saveRDS(df_3, file = "~/Coursera/Capstone/Project/data/df_3.rds")
## clean up files
rm(twit3, blog3, news3)


## Create quadgrams #########
twit4 <- clean(x=twitter, y=4)
blog4 <- clean(x=blogs, y=4)
news4 <- clean(x=news, y=4)
df_4 <- concat(twit4, blog4, news4)
df_4 <- df_4[df_4$frequency>=5, ]  #trim the file

saveRDS(df_4, file = "~/Coursera/Capstone/Project/data/df_4.rds")
## clean up files
rm(twit4, blog4, news4)