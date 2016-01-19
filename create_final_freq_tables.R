load("full_bigrams.RData")
full.bigrams.ft <- TrimData(full.bigrams.ft, 2)
full.bigrams.ft <- SplitPhrase(full.bigrams.ft, 2)
full.bigrams.ft$word <- NULL
save(full.bigrams.ft, file = "full_bi.RData")
rm(full.bigrams.ft)
gc()

load("full_trigrams.RData")
full.trigrams.ft <- TrimData(full.trigrams.ft, 2)
full.trigrams.ft <- SplitPhrase(full.trigrams.ft, 3)
full.trigrams.ft$word <- NULL
save(full.trigrams.ft, file = "full_tri.RData")
rm(full.trigrams.ft)
gc()

load("full_quadgrams.RData")
full.quadgrams.ft <- TrimData(full.quadgrams.ft, 2)
full.quadgrams.ft <- SplitPhrase(full.quadgrams.ft, 4)
full.quadgrams.ft$word <- NULL
save(full.quadgrams.ft, file = "full_quad.RData")
rm(full.quadgrams.ft)
gc()

load("full_quintgrams.RData")
full.quintgrams.ft <- TrimData(full.quintgrams.ft, 2)
full.quintgrams.ft <- SplitPhrase(full.quintgrams.ft, 5)
full.quintgrams.ft$word <- NULL
save(full.quintgrams.ft, file = "full_quint.RData")
rm(full.quintgrams.ft)
gc()
