library(quanteda)
library(RSQLite)
source("utility_functions.R")

# Initial Reading of Full Files
blogs <- ReadFile("D:/RProgram/capstone_jh/final/en_US/en_US.blogs.txt") 
tweets <- ReadFile("D:/RProgram/capstone_jh/final/en_US/en_US.twitter.txt")
news <- ReadFile("D:/RProgram/capstone_jh/final/en_US/en_US.news.txt")

# Also include a profanity list for word exclusion when making dfm
profanity <- ReadFile("D:/RProgram/capstone_jh/profanity.txt")

# Dictionary of 15k most common words to filter and reduce size of word list
word.list <- read.table("D:/RProgram/capstone_jh/corncob_lowercase.txt", sep = '\t',
                        header = FALSE, stringsAsFactors = FALSE)
word.list <- word.list$V1

# Splitting into train and test data
full <- c(blogs, tweets, news)
set.seed(2015)
full.indx <- sample(1:length(full), size = .1*length(full), replace = FALSE)
train <- full[-full.indx]
test <- full[full.indx]

# sampling of Files; For now will try 15%
full.samp <- RandomSample(train, percent = .75)

# Creating Tokens- created on 75% of data
system.time(full.unigrams <- TokenizeText(full.samp, ngram = 1))
system.time(full.bigrams <- TokenizeText(full.samp, ngram = 2))
system.time(full.trigrams <- TokenizeText(full.samp, ngram = 3))
system.time(full.quadgrams <- TokenizeText(full.samp, ngram = 4))
system.time(full.quintgrams <- TokenizeText(full.samp, ngram = 5))

# Creating DFM
full.unigrams.dfm <- CreateDfm(full.unigrams)
full.bigrams.dfm <- CreateDfm(full.bigrams)
full.trigrams.dfm <- CreateDfm(full.trigrams)
full.quadgrams.dfm <- CreateDfm(full.quadgrams)
full.quintgrams.dfm <- CreateDfm(full.quintgrams)

# Creating frequency table
full.unigrams.ft <- CreateFreqTable(full.unigrams.dfm)
full.bigrams.ft <- CreateFreqTable(full.bigrams.dfm)
full.trigrams.ft <- CreateFreqTable(full.trigrams.dfm)
full.quadgrams.ft <- CreateFreqTable(full.quadgrams.dfm)
full.quintgrams.ft <- CreateFreqTable(full.quintgrams.dfm)

# Removing ngrams below a threshold frequency level; removing some profanity that
# for unknown reason made it through quanteda when doing ngrams where n > 1
full.bigrams.ft <- TrimData(full.bigrams.ft, 2)
full.trigrams.ft <- TrimData(full.trigrams.ft, 2)
full.quadgrams.ft <- TrimData(full.quadgrams.ft, 2)
full.quintgrams <- TrimData(full.quintgrams.ft, 2)
# # stripping ngrams apart for prediction. Get beginning of phrase/ngram and then 
# # split off the last word
# BeginningCurrent(full.bigrams.ft)
# BeginningCurrent(full.trigrams.ft)
# BeginningCurrent(full.quadgrams.ft)
# BeginningCurrent(full.quintgrams.ft)
# 
# # Inserting data into SQLite database
# db <- dbConnect(SQLite(), dbname = 'ngram.db')
# dbSendQuery(conn = db, "CREATE TABLE ngram 
#             (beg TEXT,
#             word TEXT,
#             freq INTEGER,
#             n INTEGER)")
# 
# 
# DbInsert("INSERT INTO ngram VALUES ($beg, $cur, $freq, 2)", full.bigrams.ft)
# DbInsert("INSERT INTO ngram VALUES ($beg, $cur, $freq, 3)", full.trigrams.ft)
# DbInsert("INSERT INTO ngram VALUES ($beg, $cur, $freq, 4)", full.quadgrams.ft)
# DbInsert("INSERT INTO ngram VALUES ($beg, $cur, $freq, 5)", full.quintgrams.ft)
# 
# dbDisconnect(db)

save(full.bigrams.ft, file = 'bigrams.RData')
save(full.trigrams.ft, file = 'trigrams.RData')
save(full.quadgrams.ft, file = 'quadgrams.RData')
save(full.quintgrams.ft, file = 'quintgrams.RData')

save(full.bigrams.dfm, file = 'bigrams.dfm.RData')
save(full.trigrams.dfm, file = 'trigrams.dfm.RData')
save(full.quadgrams.dfm, file = 'quadgrams.dfm.RData')
save(full.quintgrams.dfm, file = 'quintgrams.dfm.RData')
