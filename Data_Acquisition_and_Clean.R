library(quanteda)
setwd("D:/RProgram/capstone_jh/final/en_US")


con <- file("en_US.twitter.txt", 'rb')
us_tweets <- readLines(con, encoding = "UTF-8", skipNul = TRUE)
close(con)
con <- file("en_US.news.txt", 'rb')
us_news <- readLines(con, encoding = "UTF-8", skipNul = TRUE)
close(con)
con <- file("en_US.blogs.txt", 'rb')
us_blogs <- readLines(con, encoding = "UTF-8", skipNul = TRUE)
close(con)

#Sampling data
set.seed(2015)
tweet_samp <- sample(us_tweets, size = length(us_tweets)*.15, replace = FALSE)
news_samp <- sample(us_news, size = length(us_news)*.15, replace = FALSE)
blog_samp <- sample(us_blogs, size = length(us_blogs)*.15, replace = FALSE)

# Combine to one object
full_text_samples <- c(tweet_samp, news_samp, blog_samp)

# tokenization - both unigram/bigram/trigram and unigram individually
full_tokens_samples <- tokenize(full_text_samples, removePunct = TRUE, removeSeparators = TRUE,
                        removeTwitter = TRUE, removeNumbers = TRUE, ngrams = 1:3,
                        verbose = TRUE, simplify = TRUE) 
unigram_tokens <- tokenize(full_text_samples, removePunct = TRUE, removeSeparators = TRUE,
                           removeTwitter = TRUE, removeNumbers = TRUE, ngrams = 1,
                           verbose = TRUE, simplify = TRUE)
# creating full dfm and unigram dfm
full_dfm <- dfm(full_tokens_samples)
unigram_dfm <- dfm(unigram_tokens)
#Looking at the top n features in the dfm
top_words <- topfeatures(full_dfm, n = 300000)
top_unigrams <- topfeatures(unigram_dfm, n = 10000)
# Plotting top words to get a sense of the distribution of words
plot(top_words, log = 'y', cex = .6, ylab = "Term Frequency")
plot(top_unigrams, log = 'y', cex = .6, ylab = "Term Frequency")

# Making a table of unigram frequencies
freq_unigram <- colSums(sort(unigram_dfm))
unigram_table <- as.matrix(x = freq_unigram)
unigram_freq <- unigram_table / sum(unigram_table[, 1])
plot(unigram_freq,log = 'y', cex = .6)
# Making a table of all token frequencies
freq_table <- colSums(sort(full_dfm))
freq_table <- data.table(word = names(freq_table), freq = freq_table)
full_table <- as.matrix(x = freq_table)
full_freq <- full_table/sum(full_table[, 1])
