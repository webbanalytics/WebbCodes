library(tidytext)
library(rtweet)
library(stringr)
library(dplyr)
library(ggplot2)
library(wordcloud)

runners <- search_users(q = 'runner' ,n=1000)

#Lots of mentions of Maze and Blaze Runner
#Remove all usernames and descriptions that include maze and blade
maze_match_desc <- grepl('[Mm]aze',runners$description) 
blade_match_desc <- grepl('[Bb]lade',runners$description) 
maze_match_sn <- grepl('[Mm]aze',runners$screen_name) 
blade_match_sn <- grepl('[Bb]lade',runners$screen_name) 
elims <- maze_match_desc | blade_match_desc | blade_match_sn | maze_match_sn

elimrunners<- runners[elims,]
cleanrunners<- runners[!elims,]

run_twdf <- timeline_gather(cleanrunners)
run_twdf_new$sample <- 1

#----------------------------------------------------------------------------
#Find 'random' sample of other users
others <- search_tweets(q="",n=1000, lang= "en", exclude_replies = TRUE, include_rts = FALSE)
countdown()
oth_twdf <- timeline_gather(others, interv = 180)
oth_twdf$sample <- 0

#-----------------------------------------------------------------------------
#Combine and compare
df <- rbind(run_twdf_new, oth_twdf)
df$sample <- factor(df$sample, levels = c(0,1), labels = c("nonRunners", "Runners"))
table(df$sample)

df$timestamp <- as.POSIXct(df$created_at)
min(df[(df$sample=="nonRunners"),]$timestamp)


df %>%
  filter(timestamp > "2018-01-01") %>%
  ggplot(aes(x=timestamp)) +
    geom_freqpoly(colour="Blue") +
    facet_grid(sample~.) +
    xlab("Date") + ylab("Number of Tweets") +
    theme_minimal()

#geocode_apply<-function(x){
  #geocode(x, source = "google", output = "all", api_key="xxx")
#}
  
df$line <- 1:nrow(df)
  
replace_reg <- "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https"
unnest_reg <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"
tidy_tweets <- df %>% 
  select(line, text, sample) %>%
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_replace_all(text, replace_reg, "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = unnest_reg) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]"))

tidy_tweets %>%
  filter(sample=="nonRunners") %>%
  count(word) %>%
  with(wordcloud(word, colors = 'navy', n, max.words = 100))

tidy_tweets %>%
  filter(sample=="Runners") %>%
  count(word) %>%
  with(wordcloud(word, colors = 'navy', n, max.words = 100))

frequency <- tidy_tweets %>% 
  group_by(sample) %>% 
  count(word, sort = TRUE) %>% 
  left_join(tidy_tweets %>% 
              group_by(sample) %>% 
              summarise(total = n())) %>%
  mutate(freq = n/total)

frequency

library(tidyr)

frequency <- frequency %>% 
  select(sample, word, freq) %>% 
  spread(sample, freq) %>%
  arrange(nonRunners, Runners)

frequency

library(scales)

ggplot(frequency, aes(nonRunners, Runners)) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red")

afin <-  tidy_tweets %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(line) %>% 
  summarise(sentiment = sum(score)) %>%
  inner_join(df)

afin %>%
  group_by(sample) %>%
  summarize(mean_sent = mean(sentiment), sd = sd(sentiment))

write.csv(df[, c("text","screen_name", "sample", "line", "timestamp")], file = "RunnerComparison.csv")
#df <- read.csv("RunnerComparison.csv")

#_____________________FUNCTIONS___________________________________________
#Function to tell you have long before the next gathering of tweets
# (because I'm impatient and need constant updates...)
countdown <- function(minutes=15){
  for (i in 1:minutes){
    print(paste((minutes+1)-i, "minutes left..."))
    Sys.sleep(60)
  }}

#__________________________________________________________________________
timeline_gather <- function (df, interv = 180) {
usersnames <- df$screen_name
i=1
j=1
numrun=length(usersnames)
interv=interv-1
twlist <- list()
#For each array of runners, download 10 most recent tweets then shift to next array
  while (j<numrun){
    print(paste("Starting round", i, "..."))
    if (j+interv<numrun){
      twlist[[i]] <- get_timelines(user=usersnames[j:(j+interv)], n=10, exclude_replies = TRUE, include_rts = FALSE)
      print(paste("Round", i, "finished at", Sys.time(), ". Pausing 15 mins..."))
      Sys.sleep(60*15)
      j = j+interv
      i = i+1
    } else {
      twlist[[i]] <- get_timelines(user=usersnames[j:numrun], n=10, exclude_replies = TRUE, include_rts = FALSE)
      j = numrun}
  }
return(do.call("rbind", twlist))}
#__________________________________________________________________________