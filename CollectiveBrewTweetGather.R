library(tidyverse)
library(tidytext)
library(lubridate)
library(stringr)
library(httr)
library(dplyr)
library(rtweet)
library(RColorBrewer)
library(wordcloud)


#Download all tweets which mention @CollectiveBrew - executed on June 26th

cb <- search_tweets(
  "@CollectiveBrew", n = 18000, include_rts = FALSE
)
cb

#Find the number of distinct users in tweet database
cb %>%
  summarize(n_distinct(screen_name))

#Convert tweets to dataframe, 602 tweets resulted from search_tweets() command
tweets_desc <- data_frame(line = 1:602, text=cb$description)

#String of regular expressions to eliminate
reg_words <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"

#Create trend graph of tweets per day
library(ggthemes)
ggplot(cb, aes(x = created_at)) +
  geom_freqpoly(bins = 20, color = 'blue') +
  ylab("Number of Tweets") + 
  xlab("Date") +
  scale_color_ptol("cyl") +
  theme_minimal()

#Narrow to distinct individuals, clean and parse their descriptions, and creat word cloud
#Much of this code was taken from 'Text Mining with R' by Julia Silge and David Robinson
tidy_tweets_desc <- tweets_desc %>%
  distinct(text) %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https", "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = reg_words) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]")) %>%
  filter(!word %in% c('beer')) %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, colors = 'navy', n, max.words = 100))

