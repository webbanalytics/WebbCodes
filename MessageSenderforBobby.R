library(rtweet)
library(tidytext)
library(dplyr)


query <- "#Homeowner OR #Newcar"
people <- search_tweets(
  query, n=18000, include_rts = FALSE, geocode = "45.145144,-80.290842,1573.389mi"
)

#Filter out unwanted users (companies and previously messaged individuals)

#Download unique screen names as vector
namelist <- people$screen_name

#Send message to all names
msg_text <- "What's up, buy some insurance"
for (person in namelist){
  post_message(msg_text, person)
}

