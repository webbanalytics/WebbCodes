library(dplyr)
library(rtweet)
library(ggplot2)

rth <- search_tweets(
  'HamOnt', n = 18000, include_rts = FALSE
)
rt

#Peterborough
#Find the number of distinct users in tweet database
rt %>%
  summarize(n_distinct(screen_name))

#Convert tweets to dataframe, 602 tweets resulted from search_tweets() command
tweets_desc <- data_frame(line = 1:1500, text=rt$description)

#String of regular expressions to eliminate
reg_words <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"

#Create trend graph of tweets per day
library(ggthemes)
ggplot(rt, aes(x = created_at)) +
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
  filter(!word %in% c('peterborough')) %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, colors = 'navy', n, max.words = 100))

##Hamilton
#Find the number of distinct users in tweet database
rth %>%
  summarize(n_distinct(screen_name))

#Convert tweets to dataframe, 602 tweets resulted from search_tweets() command
tweets_desc <- data_frame(line = 1:4452, text=rth$description)

#String of regular expressions to eliminate
reg_words <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"

#Create trend graph of tweets per day
library(ggthemes)
ggplot(rth, aes(x = created_at)) +
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
  filter(!word %in% c('hamilton','#hamont')) %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, colors = 'navy', n, max.words = 100))

rt$city <- "Peterborough"
rth$city <- "Hamilton"

tweets <- rbind(rt,rth)

tweets$text <- tweets$description

library(stringr)
library(tidytext)

remove_reg <- "&amp;|&lt;|&gt;"
tidy_tweets <- tweets %>% 
  distinct() %>%
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_replace(text, remove_reg, "")) %>%
  unnest_tokens(word, text, token = "tweets") %>%
  filter(!word %in% stop_words$word,
         !word %in% str_replace(stop_words$word, "'", ""),
         !word %in% c('hamilton','peterborough', '#ptbo', '#hamont', '#hamilton'),
         str_detect(word, "[a-z]"))


tidy_tweets$city <- as.factor(tidy_tweets$city)

frequency <- tidy_tweets %>% 
  group_by(city) %>% 
  count(word, sort = TRUE) %>% 
  left_join(tidy_tweets %>% 
              group_by(city) %>% 
              summarise(total = n())) %>%
  mutate(freq = n/total)

frequency

library(tidyr)

frequency <- frequency %>% 
  select(city, word, freq) %>% 
  spread(city, freq) %>%
  arrange(Hamilton, Peterborough)

frequency

library(scales)

ggplot(frequency, aes(Hamilton, Peterborough)) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red")
