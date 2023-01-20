library(dplyr)
library(stringr)
library(tidytext)
library(janeaustenr)
library(ggplot2)
library(tidyr)
library(igraph)
library(ggraph)



################################################################################
################################################################################

#Year 2017

text1 <-read.csv("file:///Users/joslynpereira/Downloads/archive/2017.csv", header = TRUE)

text<-text1$tweet

text

text_df <- tibble(line = 1, text = text)
text_df

# Sentences as tokens
text_df %>%
  unnest_sentences(sentence, text)

# Words as tokens
text_df %>%
  unnest_tokens(word, text)

text_df

# Word frequency
tweet_words <- text_df %>%
  unnest_tokens(word, text) %>%
  count(line, word, sort = TRUE)

tweet_words$line <- NULL


tweet_words

tweet_words17 <- head(tweet_words, n = 10)

ggplot(tweet_words17, aes(x=word, y= n)) +
  geom_bar(stat='identity')



# Zipf's law
freq_by_rank <- tweet_words %>% 
  group_by(n) %>% 
  mutate(rank = row_number(), 
         `term frequency` = n) %>%
  ungroup()

freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = n)) + 
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()

rank_subset <- freq_by_rank %>% 
  filter(rank < 500,
         rank > 10)

lm(log10(`term frequency`) ~ log10(rank), data = rank_subset)



# bigrams

tweet_bigrams <- text1 %>%
  unnest_tokens(bigram, tweet, token = "ngrams", n = 2)

tweet_bigrams

################################################################################
################################################################################

##Year 2018

text2 <-read.csv("file:///Users/joslynpereira/Downloads/archive/2018.csv", header = TRUE)

text18<-text2$tweet

text18

text_df18 <- tibble(line = 1, text = text18)
text_df18

# Sentences as tokens
text_df18 %>%
  unnest_sentences(sentence, text18)

# Words as tokens
text_df18 %>%
  unnest_tokens(word, text18)

text_df18


# Word frequency
tweet_words18 <- text_df18 %>%
  unnest_tokens(word, text) %>%
  count(line, word, sort = TRUE)

tweet_words18$line <- NULL


tweet_words18

tweet_words18x <- head(tweet_words18, n = 10)

ggplot(tweet_words18x, aes(x=word, y= n)) +
  geom_bar(stat='identity')

# Zipf's law
freq_by_rank18 <- tweet_words18 %>% 
  group_by(n) %>% 
  mutate(rank = row_number(), 
         `term frequency` = n) %>%
  ungroup()

freq_by_rank18 %>% 
  ggplot(aes(rank, `term frequency`, color = n)) + 
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()

rank_subset18 <- freq_by_rank18 %>% 
  filter(rank < 500,
         rank > 10)

lm(log10(`term frequency`) ~ log10(rank), data = rank_subset18)



# bigrams

tweet_bigrams18 <- text2 %>%
  unnest_tokens(bigram, tweet, token = "ngrams", n = 2)

tweet_bigrams18

################################################################################
################################################################################

##Year 2019

text3 <-read.csv("file:///Users/joslynpereira/Downloads/archive/2019.csv", header = TRUE)

text19<-text3$tweet

text19

text_df19 <- tibble(line = 1, text = text19)
text_df19

# Sentences as tokens
text_df19 %>%
  unnest_sentences(sentence, text19)

# Words as tokens
text_df19 %>%
  unnest_tokens(word, text19)

text_df19

# Word frequency
tweet_words19 <- text_df19 %>%
  unnest_tokens(word, text) %>%
  count(line, word, sort = TRUE)

tweet_words19$line <- NULL


tweet_words19

tweet_words19x <- head(tweet_words19, n = 10)

ggplot(tweet_words19x, aes(x=word, y= n)) +
  geom_bar(stat='identity')

# Zipf's law
freq_by_rank19 <- tweet_words19 %>% 
  group_by(n) %>% 
  mutate(rank = row_number(), 
         `term frequency` = n) %>%
  ungroup()

freq_by_rank19 %>% 
  ggplot(aes(rank, `term frequency`, color = n)) + 
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()

rank_subset19 <- freq_by_rank19 %>% 
  filter(rank < 500,
         rank > 10)

lm(log10(`term frequency`) ~ log10(rank), data = rank_subset19)



# bigrams

tweet_bigrams19 <- text3 %>%
  unnest_tokens(bigram, tweet, token = "ngrams", n = 2)

tweet_bigrams19

################################################################################
################################################################################

##Year 2020

text4 <-read.csv("file:///Users/joslynpereira/Downloads/archive/2020.csv", header = TRUE)

text20<-text4$tweet

text20

text_df20 <- tibble(line = 1, text = text20)
text_df20

# Sentences as tokens
text_df20 %>%
  unnest_sentences(sentence, text20)

# Words as tokens
text_df20 %>%
  unnest_tokens(word, text20)

text_df20

# Word frequency
tweet_words20 <- text_df20 %>%
  unnest_tokens(word, text) %>%
  count(line, word, sort = TRUE)

tweet_words20$line <- NULL


tweet_words20

tweet_words20x <- head(tweet_words20, n = 10)

ggplot(tweet_words20x, aes(x=word, y= n)) +
  geom_bar(stat='identity')

# Zipf's law
freq_by_rank20 <- tweet_words20 %>% 
  group_by(n) %>% 
  mutate(rank = row_number(), 
         `term frequency` = n) %>%
  ungroup()

freq_by_rank20 %>% 
  ggplot(aes(rank, `term frequency`, color = n)) + 
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()

rank_subset20 <- freq_by_rank20 %>% 
  filter(rank < 500,
         rank > 10)

lm(log10(`term frequency`) ~ log10(rank), data = rank_subset20)



# bigrams

tweet_bigrams20 <- text4 %>%
  unnest_tokens(bigram, tweet, token = "ngrams", n = 2)

tweet_bigrams20


################################################################################
################################################################################

##Year 2021

text5 <-read.csv("file:///Users/joslynpereira/Downloads/archive/2021.csv", header = TRUE)

text21<-text5$tweet

text21

text_df21 <- tibble(line = 1, text = text21)
text_df21

# Sentences as tokens
text_df21 %>%
  unnest_sentences(sentence, text21)

# Words as tokens
text_df21 %>%
  unnest_tokens(word, text21)

text_df21

# Word frequency
tweet_words21 <- text_df21 %>%
  unnest_tokens(word, text) %>%
  count(line, word, sort = TRUE)

tweet_words21$line <- NULL


tweet_words21

tweet_words21x <- head(tweet_words21, n = 10)

ggplot(tweet_words21x, aes(x=word, y= n)) +
  geom_bar(stat='identity')

# Zipf's law
freq_by_rank21 <- tweet_words21 %>% 
  group_by(n) %>% 
  mutate(rank = row_number(), 
         `term frequency` = n) %>%
  ungroup()

freq_by_rank21 %>% 
  ggplot(aes(rank, `term frequency`, color = n)) + 
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()

rank_subset21 <- freq_by_rank21 %>% 
  filter(rank < 500,
         rank > 10)

lm(log10(`term frequency`) ~ log10(rank), data = rank_subset21)



# bigrams

tweet_bigrams21 <- text5 %>%
  unnest_tokens(bigram, tweet, token = "ngrams", n = 2)

tweet_bigrams21


################################################################################
################################################################################

##Year 2022

text6 <-read.csv("file:///Users/joslynpereira/Downloads/archive/2022.csv", header = TRUE)

text22<-text6$tweet

text22

text_df22 <- tibble(line = 1, text = text22)
text_df22

# Sentences as tokens
text_df22 %>%
  unnest_sentences(sentence, text22)

# Words as tokens
text_df22 %>%
  unnest_tokens(word, text22)

text_df22

# Word frequency
tweet_words22 <- text_df22 %>%
  unnest_tokens(word, text) %>%
  count(line, word, sort = TRUE)

tweet_words22$line <- NULL


tweet_words22

tweet_words22x <- head(tweet_words22, n = 10)

ggplot(tweet_words22x, aes(x=word, y= n)) +
  geom_bar(stat='identity')

# Zipf's law
freq_by_rank22 <- tweet_words22 %>% 
  group_by(n) %>% 
  mutate(rank = row_number(), 
         `term frequency` = n) %>%
  ungroup()

freq_by_rank22 %>% 
  ggplot(aes(rank, `term frequency`, color = n)) + 
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()

rank_subset22 <- freq_by_rank22 %>% 
  filter(rank < 500,
         rank > 10)

lm(log10(`term frequency`) ~ log10(rank), data = rank_subset22)



# bigrams

tweet_bigrams22 <- text6 %>%
  unnest_tokens(bigram, tweet, token = "ngrams", n = 2)

tweet_bigrams22






