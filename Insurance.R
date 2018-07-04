library(tidyverse)
library(tidytext)
library(lubridate)
library(stringr)
library(httr)
library(dplyr)
library(rtweet)
library(RColorBrewer)
library(wordcloud)


insure <- search_tweets(
  "insurance", n = 18000, include_rts = FALSE
)

ins_df <- data_frame(line = 1:13004, text = insure$text)

reg_words <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"

tidy_tweets_ins <- ins_df %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https", "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = reg_words) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]")) %>%
  filter(!word %in% c('beer')) %>%
  anti_join(stop_words)

j <- tidy_tweets_ins %>%
  inner_join(get_sentiments('afinn')) %>%
  group_by(line) %>%
  summarize(sent = mean(score))

ins_mtch <- merge(x = insure, y = ins_df, all = FALSE)
ins_mtch <- inner_join(insure, ins_df)

finmatch <- merge()