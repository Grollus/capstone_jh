library(quanteda)
library(data.table)
library(stringi)

options(mc.cores = 4)
# Read in file
ReadFile <- function(filename){
  con <- file(filename, open = "rb")
  file <- readLines(con, encoding = "UTF-8", skipNul = TRUE)
  close(con)
  file
}

# Select reproducible random sample from input file; need to set this up to allow
# for the creating of random test set
RandomSample <- function(file, percent = .15){
  set.seed(2015)
  sample(file, size = length(file) * percent, replace = FALSE)
}

# Creating tokens
TokenizeText <- function(data, ngram = 1){
  # convert to lowercase
  d <- toLower(data)
  # quanteda doesn't remove ' with punctuation or separators.
  # a gsub call works, but is slow.
  # Trying with stri_replace_all_fixed from stringi package
  d <- stri_replace_all_fixed(d, c("'", "’", "‘", "`", "′"), c("", "", "", "", ""), vectorize_all = FALSE)
  # remove numbers, punctuation, separators(whitespace included)
  # default to unigrams
  tokenize(d, removeNumbers = TRUE, removePunct = TRUE, removeSeparators = TRUE,
           removeTwitter = TRUE, ngrams = ngram, simplify = TRUE)
}

# Creating Document Frequency Matrix
CreateDfm <- function(data){
  dfm(data)
}

#Creating frequency table out of the dfm
CreateFreqTable <- function(data){
  freq <- colSums(sort(data))
  data.table(word = names(freq), freq = freq)
}

### This turned out to be slower than SplitPhrase. Kept for reference.
# # Split the ngram into components and add as new columns in data table
# BeginningCurrent <- function(data){
#   
#   data[, c("beg", "cur"):=list(unlist(strsplit(word, "_"))[1],
#                              unlist(strsplit(word, "_"))[2]),
#      by = word]
# }

# This is about 30% faster than above; also allows expansion to more than two words
SplitPhrase<- function(data, ngram = NULL){
  if(is.null(ngram))
     stop("Enter a valid number of words")
  if(ngram == 5){
    data[, c("word1", "word2", "word3", "word4", "word5"):= 
           list(stri_split_fixed(word, "_", n = 5, omit_empty = NA)[[1]][1],
                stri_split_fixed(word, "_", n = 5, omit_empty = NA)[[1]][2],
                stri_split_fixed(word, "_", n = 5, omit_empty = NA)[[1]][3],
                stri_split_fixed(word, "_", n = 5, omit_empty = NA)[[1]][4],
                stri_split_fixed(word, "_", n = 5, omit_empty = NA)[[1]][5]),
         by = word]
    data[complete.cases(data),]
  }else if(ngram == 4){
    data[, c("word1", "word2", "word3", "word4"):= 
           list(stri_split_fixed(word, "_", n = 4, omit_empty = NA)[[1]][1],
                stri_split_fixed(word, "_", n = 4, omit_empty = NA)[[1]][2],
                stri_split_fixed(word, "_", n = 4, omit_empty = NA)[[1]][3],
                stri_split_fixed(word, "_", n = 4, omit_empty = NA)[[1]][4]),
         by = word]
    data[complete.cases(data),]
  }else if(ngram == 3){
    data[, c("word1", "word2", "word3"):= 
           list(stri_split_fixed(word, "_", n = 3, omit_empty = NA)[[1]][1],
                stri_split_fixed(word, "_", n = 3, omit_empty = NA)[[1]][2],
                stri_split_fixed(word, "_", n = 3, omit_empty = NA)[[1]][3]),
         by = word]
    data[complete.cases(data),]
  }else{
    data[, c("word1", "word2"):= list(stri_split_fixed(word, "_", n = 2, omit_empty = NA)[[1]][1],
                                      stri_split_fixed(word, "_", n = 2, omit_empty = NA)[[1]][2]),
         by = word]
    data[complete.cases(data),]
  }
  
}

# # Inserting Data into DB; I think this is going to be removed.
# DbInsert <- function(sql, key_counts){
#   dbBegin(db)
#   dbGetPreparedQuery(db, sql, bind.data = key_counts)
#   dbCommit(db)
# }

# Trimming DataSets Down to more manageable size
TrimData <- function(data, freqThreshold){
  data[freq >= freqThreshold]
}

