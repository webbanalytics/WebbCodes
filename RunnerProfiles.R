library(tidytext)
library(rtweet)
library(stringr)

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
df$sample <- factor(df$sample, levels = c(0,1), labels = c("Non-Runners", "Runners"))

write.csv(df[, c("text","screen_name", "sample")], file = "RunnerComparison.csv")

#_________________________________________________________________________
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