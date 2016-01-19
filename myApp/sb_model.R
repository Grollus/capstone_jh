library(stringi)
library(quanteda)
library(magrittr)
library(data.table)
# Cleaning input phrase exactly like corpus was processed
# TODO--check if using the stringi cleaning method below results in higher accuracy
# if used in the corpus cleaning stage
CleanInput <- function(input){
  text <- toLower(input)%>%
    stri_replace_all_fixed(c("'", "’"), c("", ""), vectorize_all = FALSE)%>%
    iconv(to = "ASCII", sub = "")%>%
    tokenize(removeNumbers = TRUE, removePunct = TRUE, removeSeparators = TRUE,
             removeTwitter = TRUE, simplify = TRUE)
  text
}

# Splitting input phrase to grab the last n words
LastNWords <- function(input, n){
  text <- CleanInput(input)
  s.text <- unlist(stri_split_fixed(text, " "))
  len <- length(s.text)
  
  if(n < 1){
    stop("LastNWords() error: number of words  <= 0")
  }
  if(n == 1){
    s.text[len]
  }else if(n > len){
    s.text[1:len]
  }else{
    w <- s.text[len]
    for(i in 1:(n-1)){
      w <- c(s.text[len - i], w)
    }
    w
  }
  
}

# Function to search through ngram datasets and find matching results
# Once results are found, MLE calculation is performed and the desired number of 
# results are returned
SearchNGram <- function(inputText, ngramData, ngramSize, rows){
  # A bit unweildy, but will have to do till I can revise it
  # args:
  #   inputText - phrase to be predicted
  #   ngramData - Dataset of ngrams--e.g. tri would be the dataset of trigrams.
  #   ngramSize - integer representing the size token we are searching through
  #   rows      - number of results returned
  if(ngramSize > 5 | ngramSize < 2){
    stop("SearchNGram() Error: ngramSize must be >= 2 & <= 5")
  }
  if(is.numeric(ngramSize) == FALSE){
    stop("SearchNGram() Error: ngramSize argument must be type numeric.")
  }
  if(ngramSize == 5){
    text <- LastNWords(inputText, ngramSize - 1)
    matches <- ngramData[word1 == text[1]][word2 == text[2]][word3 == text[3]][word4 == text[4]]
    matches <- matches[, .(word5, freq)][order(-freq)]
    sum.freq <- sum(matches$freq)
    matches$freq <- round(matches$freq / sum.freq * 100)
    colnames(matches) <- c("prediction", "quint.MLE")
    if(nrow(matches) < rows){
      rows <- nrow(matches)
    }
    matches[1:rows, ]
  }else if(ngramSize == 4){
    text <- LastNWords(inputText, ngramSize - 1)
    matches <- ngramData[word1 == text[1]][word2 == text[2]][word3 == text[3]]
    matches <- matches[, .(word4, freq)][order(-freq)]
    sumfreq <- sum(matches$freq)
    matches$freq <- round(matches$freq / sumfreq * 100)
    colnames(matches) <- c("prediction", "quad.MLE")
    if(nrow(matches) < rows){
      rows <- nrow(matches)
    }
    matches[1:rows, ]
  }else if(ngramSize == 3){
    text <- LastNWords(inputText, ngramSize - 1)
    matches <- ngramData[word1 == text[1]][word2 == text[2]]
    matches <- matches[, .(word3, freq)][order(-freq)]
    sumfreq <- sum(matches$freq)
    matches$freq <- round(matches$freq / sumfreq * 100)
    colnames(matches) <- c("prediction", "tri.MLE")
    if(nrow(matches) < rows){
      rows <- nrow(matches)
    }
    matches[1:rows, ]
  }else{
    text <- LastNWords(inputText, ngramSize - 1)
    matches <- ngramData[word1 == text[1]]
    matches <- matches[, .(word2, freq)][order(-freq)]
    sumfreq <- sum(matches$freq)
    matches$freq <- round(matches$freq / sumfreq * 100)
    colnames(matches) <- c("prediction", "bi.MLE")
    if(nrow(matches) < rows){
      rows <- nrow(matches)
    }
    matches[1:rows, ]
  }
  
}


# Function to compute the stupid backoff scores
sbScore <- function(alpha = 0.4, xQuint, xQuad, xTri, xBi){
  score <- 0
  if(xQuint > 0){
    score <- xQuint
  }else if(xQuad >= 1){
    score <- xQuad * alpha
  }else if(xTri > 0){
    score <- xTri * alpha^2
  }else if(xBi > 0){
    score <- xBi * alpha^3
  }
  round(score, 1)
}


# Function to combine matches into single dataframe and score them using sbScore function
scoreNgrams <- function(x, rows = 10){
  # Get matches from each ngram dataset
  quint.match <- SearchNGram(x, quint, 5, rows)
  quad.match <- SearchNGram(x, quad, 4, rows)
  tri.match <- SearchNGram(x, tri, 3, rows)
  bi.match <- SearchNGram(x, bi, 2, rows)
  
  # merge datasets
  quintQuad <- merge(quint.match, quad.match, by = "prediction", all = TRUE)
  quintQuadTri <- merge(quintQuad, tri.match, by = "prediction", all = TRUE)
  quintQuadTriBi <- merge(quintQuadTri, bi.match, by = "prediction", all = TRUE)
  data <- quintQuadTriBi[!is.na(prediction)]
  if(nrow(data) > 0){
    data[is.na(data)] <- 0
    data$score <- mapply(sbScore, alpha = 0.4, data$quint.MLE, data$quad.MLE, data$tri.MLE,
                         data$bi.MLE)
    data <- data[order(-score)]
  }
  data
}


# stupid backoff algorithm
sbAlgo <- function(x, alpha = 0.4, rows = 10, nResults = 1, rmProfanity = TRUE){
  prediction <- ""
  if(x == ""){
    return(c("the", "to", "and"))
  }
  
  data <- scoreNgrams(x, rows)
  if(nrow(data) == 0){
    return(c("the", "to", "and"))
  }
  if(nResults > nrow(data)){
    nResults <- nrow(data)
  }
  if(nResults == 1){
    top.words <- data[data$score == max(data$score), ]$prediction
    prediction <- sample(top.words, 1)
  }else{
    prediction <- data$prediction[1:nResults]
  }
  prediction
}