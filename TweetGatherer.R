library(tidyverse)
library(tidytext)
library(lubridate)
library(stringr)
library(httr)
library(dplyr)
library(rtweet)
library(RColorBrewer)

create_token(
  app = ,
  consumer_key = ,
  consumer_secret = )

cb <- search_tweets(
  "@CollectiveBrew", n = 18000, include_rts = FALSE
)
cb

tweets <- data_frame(line = 1:602, text=cb$text)

reg_words <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"
tidy_tweets <- tweets %>%
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https", "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = reg_words) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]"))
  

library(wordcloud)

wrcld <- tidy_tweets %>%
  filter(!word %in% c('drinking')) %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

#--------------------------------------------------------------------------


tweets_desc <- data_frame(line = 1:602, text=cb$description)

reg_words <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"
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


colorRampPalette(brewer.pal(3,"Blues"))(100)
tweets_desc %>%
  summarize(dist = count(distinct(text)))

library(ggthemes)
ggplot(cb, aes(x = created_at)) +
  geom_freqpoly(bins = 20, color = 'blue') +
  ylab("Number of Tweets") + 
  xlab("Date") +
  scale_color_ptol("cyl") +
  theme_minimal()
  
cb %>%
  summarize(n_distinct(screen_name))







n <- 60 #number of loops
s <- vector("list", n)
since_id <- NULL
for (i in seq_len(n)) {
  s[[i]] <- search_tweets("Beer OR beer", n = 20000, include_rts = FALSE, max_id = since_id, lang = 'en')
  since_id <- tail(s[[i]]$status_id, 1)
  print(Sys.time())
  Sys.sleep(60 * 15)
}
twdf <- do.call("rbind", s)
tweets <- as.vector(rt$text)
print(paste("Finishing Time:", Sys.time()))

library(dplyr)
reg_words <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"
tidy_tweets <- tweets %>%
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https", "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = reg_words) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]"))

text_df <- data_frame(line = 1:length(rt), text = rt$text)
rt %>%
  unnest_tokens(word, text, token = "regex") -> ntest

typeof(tweets$text)
head(twdf)

tw_text <- tweets$text
clean_tweet = gsub("&amp", "", tw_text)
clean_tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", clean_tweet)
clean_tweet = gsub("@\\w+", "", clean_tweet)
clean_tweet = gsub("[[:punct:]]", "", clean_tweet)
clean_tweet = gsub("[[:digit:]]", "", clean_tweet)
clean_tweet = gsub("http\\w+", "", clean_tweet)
clean_tweet = gsub("[ \t]{2,}", "", clean_tweet)
clean_tweet = gsub("^\\s+|\\s+$", "", clean_tweet) 

reg_words <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"
tidy_tweets2 <- tw_text %>%
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https", "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = reg_words) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]"))

head(clean_tweet)
