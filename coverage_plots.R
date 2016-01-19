# Bigram Coverage
total.bigram.occurrances <- sum(full.bigrams.ft$freq)
bi.coverage.df <- data.frame(Terms = round(seq(1, nrow(full.bigrams.ft), length.out = 5000)))
full.bigrams.ft$cs <- cumsum(full.bigrams.ft$freq)
for(n in 1:nrow(bi.coverage.df)){
  bi.coverage.df$coverage[n] <- full.bigrams.ft$cs[bi.coverage.df$Terms[n]]/total.bigram.occurrances
}
bi.lines <- with(bi.coverage.df, approx(coverage, Terms, c(0.5, 0.8, 0.9)))
ggplot(bi.coverage.df, aes(x = Terms, y = coverage))+
  geom_line()+
  geom_hline(yintercept = .9, linetype = 'dashed')+
  geom_vline(xintercept = c(38661, 1036413, 4220701), linetype = 'dashed', color = 'red')

# Trigram Coverage
total.trigram.occurrances <- sum(full.trigrams.ft$freq)
tri.coverage.df <- data.frame(Terms = round(seq(1, nrow(full.trigrams.ft), length.out = 5000)))
full.trigrams.ft$cs <- cumsum(full.trigrams.ft$freq)
for(n in 1:nrow(tri.coverage.df)){
  tri.coverage.df$coverage[n] <- full.trigrams.ft$cs[tri.coverage.df$Terms[n]]/total.trigram.occurrances
}

tri.lines <- with(tri.coverage.df, approx(coverage, Terms, c(0.5, 0.8, 0.9)))
ggplot(tri.coverage.df, aes(x = Terms, y = coverage))+
  geom_line()+
  geom_hline(yintercept = .9, linetype = 'dashed')+
  geom_vline(xintercept = c(tri.lines$y), linetype = 'dashed', color = 'red')

# Quadgram Coverage
total.quadgram.occurrances <- sum(full.quadgrams.ft$freq)
quad.coverage.df <- data.frame(Terms = round(seq(1, nrow(full.quadgrams.ft), length.out = 5000)))
full.quadgrams.ft$cs <- cumsum(full.quadgrams.ft$freq)
for(n in 1:nrow(quad.coverage.df)){
  quad.coverage.df$coverage[n] <- full.quadgrams.ft$cs[quad.coverage.df$Terms[n]]/total.quadgram.occurrances
}

quad.lines <- with(quad.coverage.df, approx(coverage, Terms, c(0.5, 0.8, 0.9)))
ggplot(quad.coverage.df, aes(x = Terms, y = coverage))+
  geom_line()+
  geom_hline(yintercept = .9, linetype = 'dashed')+
  geom_vline(xintercept = c(quad.lines$y), linetype = 'dashed', color = 'red')

# quintgram Coverage
total.quintgram.occurrances <- sum(full.quintgrams.ft$freq)
quint.coverage.df <- data.frame(Terms = round(seq(1, nrow(full.quintgrams.ft), length.out = 5000)))
full.quintgrams.ft$cs <- cumsum(full.quintgrams.ft$freq)
for(n in 1:nrow(quint.coverage.df)){
  quint.coverage.df$coverage[n] <- full.quintgrams.ft$cs[quint.coverage.df$Terms[n]]/total.quintgram.occurrances
}

quint.lines <- with(quint.coverage.df, approx(coverage, Terms, c(0.5, 0.8, 0.9)))
ggplot(quint.coverage.df, aes(x = Terms, y = coverage))+
  geom_line()+
  geom_hline(yintercept = .9, linetype = 'dashed')+
  geom_vline(xintercept = c(quint.lines$y), linetype = 'dashed', color = 'red')
