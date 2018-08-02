############################################
## Michael Hulin
## August 2018
## Coursera Data Science Capstone Project
## Word Prediction Application
## Uses 4 ngram models and a backoff algorithm to determine the most likely next word
## Shiny app: https://pavefe.shinyapps.io/Word_predictor/
## Presentation: 
############################################

library(tm)
library(stringr)
library(data.table)

## Read in the data files
df_1 <- readRDS(file = "data/df_1.rds")
df_2 <- readRDS(file = "data/df_2.rds")
df_3 <- readRDS(file = "data/df_3.rds")
df_4 <- readRDS(file = "data/df_4.rds")

setkey(df_1, terms)
setkey(df_2, terms)
setkey(df_3, terms)
setkey(df_4, terms)

## Prediction with Katz Back-off model
## Input filter
CleanInputString<- function(input_string)
{
  # cleaning up data
  input_string<- iconv(input_string, "latin1", "ASCII", sub=" ");
  input_string<- gsub("[^[:alpha:][:space:][:punct:]]", "", input_string);
  # corpus
  input_corpus<- VCorpus(VectorSource(input_string))
  input_corpus<- tm_map(input_corpus, content_transformer(tolower))
  input_corpus<- tm_map(input_corpus, removePunctuation)
  input_corpus<- tm_map(input_corpus, removeNumbers)
  input_corpus<- tm_map(input_corpus, stripWhitespace)
  input_string<- as.character(input_corpus[[1]])
  input_string<- gsub("(^[[:space:]]+|[[:space:]]+$)", "", input_string)  
  if (nchar(input_string) > 0) {
    return(input_string); 
  } else {
    return("");
  }
}

## Prediction Function
Get_next_word<- function(input_string)
{
  # Data cleansing using the function written earlier
  input_string <- CleanInputString(input_string);
  # extract the string length
  input_string <- unlist(strsplit(input_string, split=" "));
  input_stringLen <- length(input_string);
  next_word_present <- FALSE;
  term_next <- as.character(NULL);
  # backoff N-gram model
  if (input_stringLen >= 3 & !next_word_present)
  {
    # collate the terms
   input_string1 <- paste(input_string[(input_stringLen-2):input_stringLen], collapse=" ");
    # take the subset of 4-gram data
    searchStr <- paste("^",input_string1, sep = "");
    df_4Temp <- df_4[terms %like% searchStr][order(-frequency)];
    if (df_4Temp[, .N] > 1 )  # check if there are more than 1 match
    {
      term_next <- df_4Temp[1:5,1];# select top 3 matching terms
      next_word_present <- TRUE;
    }
    df_4Temp <- NULL;
  }
  # n-1 gram -- checking 3-gram model if there were not more than 1 match with the 4-gram
  if (input_stringLen>= 2 & !next_word_present)
  {
    # collate input terms
    input_string1 <- paste(input_string[(input_stringLen-1):input_stringLen], collapse=" ");
    searchStr <- paste("^",input_string1, sep = "");
    df_3Temp <- df_3[terms %like% searchStr][order(-frequency)];
    if ( df_3Temp[, .N] > 1 )
    {
      term_next <- df_3Temp[1:5,1];  # select top 3 matching terms
      next_word_present <- TRUE;
    }
    df_3Temp <- NULL;
  }
  # ngram -- checking the 2 gram model for second to last word
  if (input_stringLen >= 1 & !next_word_present)
  {
    input_string1 <- input_string[input_stringLen];
    searchStr <- paste("^",input_string1, sep = "");
    df_2Temp <- df_2[terms %like% searchStr][order(-frequency)];
    if ( df_2Temp[, .N] > 1 )
    {
      term_next<- df_2Temp[1:5,1];  # select top 3 matching terms
      next_word_present<- TRUE;
    }
    df_2Temp <- NULL;
  }  
  # if no matched return the top unigrams
#  if (!next_word_present&input_stringLen >= 0)
#  {
#    term_next <- df_1[order(-frequency)][1:5, 1];
#  }
  if (next_word_present) 
  { 
    word_nxt <- term_next[,word(terms, -1)];
  }
    else word_nxt <- df_1[order(-frequency)][1:5, 1];
# final if statement to return the df of predicted words    
  if (input_stringLen > 0)
  {
    df <- data.frame(word_nxt);
    return(df);
  } else 
  {
    word_nxt <- df_1[order(-frequency)][1:5, 1];
    df <- data.frame(word_nxt);
    return(df);
  }
}
