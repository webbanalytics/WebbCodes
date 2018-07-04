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

runlist <- runners$screen_name
runlist2 <- cleanrunners$screen_name


countdown <- function(minutes=15){
  for (i in 1:minutes){
    print(paste((minutes+1)-i, "minutes left..."))
    Sys.sleep(60)
  }}


i=1
j=1
numrun=length(runlist2)
interv=180-1
twlist <- list()
#countdown(15)
#For each array of runners, download 10 most recent tweets then shift to next array
while (j<numrun){
if (j+interv<numrun){
  twlist[[i]] <- get_timelines(user=runlist2[j:(j+interv)], n=10, exclude_replies = TRUE, include_rts = FALSE)
  j = j+interv
  i = i+1
  countdown(15)
} else {
  twlist[[i]] <- get_timelines(user=runlist2[j:numrun], n=10, exclude_replies = TRUE, include_rts = FALSE)
  j = numrun}
}





for (runner in runlist2){
twlist[i] <- tryCatch({
  get_timelines(user=runner, n=10, exclude_replies = TRUE, include_rts = FALSE)
  }, error = function(e) {
        countdown(15)
        return(get_timelines(user=runner, n=10, exclude_replies = TRUE, include_rts = FALSE))
          },finally = {i <- i+1})
}


countdown(15)

run_tweets <- get_timelines(user=runlist2, n=10, exclude_replies = TRUE, include_rts = FALSE)
