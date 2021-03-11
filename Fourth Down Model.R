## WARNING: I am not very good at this, my comments aren't great. the NFLfastR github has a beginner's
## guide attached to it. I would highly recommend using that to get your hands dirty with NFLfastR
## instead of stumbling around my code.
## However, if you do end up looking around here, and want me to clarify anything I did, please 
## feel free to reach out to me and I can tell you what I did.

## Now, onto my actual code!!
## the packages we'll potentially need (aka the recommended ones when using nflfastR)
library(tidyverse) 
library(ggrepel)
library(ggimage)
library(nflfastR)
library(stringr)
seasons <- 2016:2020  ## all this loads in the data
pbp <- map_df(seasons, function(x) {
  readRDS(
    url(
      paste0("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_",x,".rds")
    )
  )
})
names(pbp)

eagles_data <- pbp %>% ## filter the data so that all we have is the Eagles
  filter(home_team == "PHI" | away_team == "PHI")

## This code gets us all the punting data for Cameron Johnston, the current Eagles punter
## H/T to Ben Baldwin's fourth down model here for help cleaning up the punting data, since
## touchbacks are handled weird in nflfastR (and I didn't know this until I read his model)
## The mutates at the end are adding in the variables necessary to use this data in EP calculations
punts <- eagles_data %>%
  filter(play_type == "punt" & posteam == "PHI",
         stringr::str_detect(desc, "Johnston")) %>%
  select(desc, yardline_100, kick_distance, return_yards, fumble_lost) %>%
  mutate(
    yardline_after = yardline_100 - kick_distance + return_yards,
    yardline_after = if_else(stringr::str_detect(desc, "end zone") & is.na(kick_distance), 
                              20, yardline_after),
    yardline_after = if_else(stringr::str_detect(desc, "BLOCKED") & is.na(yardline_after),
                             yardline_100, yardline_after),
    yardline_after = if_else(yardline_after > 100, 100, yardline_after),
    return_td = if_else(yardline_after == 100, 1, 0)
  ) %>%
  mutate(yardline_before = yardline_100) %>%
  mutate(yardline_100 = yardline_after) %>%
  mutate(season = 2020) %>%
  mutate(roof = "outdoors") %>%
  mutate(half_seconds_remaining = 1020, down = 1, ydstogo = 10, posteam_timeouts_remaining = 3,
         defteam_timeouts_remaining = 3)

## Get all the field goal attempts by Jake Elliott, current Eagles kicker
## the mutations are to get a numerical indicator for field goal result and to group
## the kicks by length
field_goals <- eagles_data %>%
  filter(play_type == "field_goal" & posteam == "PHI", stringr::str_detect(desc, "Elliott")) %>%
  mutate(
    kick_good = if_else(field_goal_result == "made", 1, 0)
    ) %>%
  mutate(
    fg_group = case_when(
      kick_distance <= 29 ~ "Short",
      kick_distance > 29 & kick_distance <= 39 ~ "Medium",
      kick_distance > 39 & kick_distance <= 49 ~ "Long",
      kick_distance > 49 ~ "Very Long"
    )
  ) %>%
  mutate( ## These probabilities were found using the group of code below
    prob_make_kick = case_when(
      fg_group == "Short" ~ 0.958, 
      fg_group == "Medium" ~ 0.882,
      fg_group == "Long" ~ 0.889,
      fg_group == "Very Long" ~ 0.571
    )
  )

fg_probs <- field_goals %>% ## This code for the above probabilities
  group_by(fg_group) %>%
  summarize(prob_make_kick = mean(kick_good))

## get just early down (1st and 2nd) data
early_downs <- eagles_data %>%
  filter(posteam == "PHI", down == 1 | down == 2) %>%
  filter(play_type == "pass" | play_type == "run")

## create a list of every amount of yards gained on an early down snap
early_down_yardage <- rep(0,4141)
for (i in 1:4141) {
  early_down_yardage[i] <- early_downs$yards_gained[i]
}

## same stuff as above but for late down (3rd and 4th) data
late_downs <- eagles_data %>%
  filter(posteam == "PHI", down == 3 | down == 4) %>%
  filter(play_type == "pass" | play_type == "run")

late_down_yardage <- rep(0,1239)
for (i in 1:1239) {
  late_down_yardage[i] <- late_downs$yards_gained[i]
}

## get just offensive plays for the Eagles, mutate the drive starting yard line
## so I can get just the yard line
offense <- eagles_data %>%
  filter(posteam == "PHI") %>%
  mutate(drive_start_yard_line = str_split(drive_start_yard_line," "))

## create a list of the starting yard line for every drive
## the frequencies are partially determined by length of drive which isn't ideal
## Note that these are listed 1-99, where 1 is the opposing 1 yard line and 99 is your own
## 1 yard line (so 1 is good)
for (i in 1:6986) {
  if (length(offense$drive_start_yard_line[[i]]) == 1) {
    offense$drive_start_yard_line[[i]] <- c("PHI", "50")
  } 
  if (offense$drive_start_yard_line[[i]][[1]] == "PHI") {
    offense$drive_start_100[i] <- 100 - as.numeric(offense$drive_start_yard_line[[i]][[2]])
  } else if (offense$drive_start_yard_line[[i]][[1]] != "PHI") {
    offense$drive_start_100[i] <- as.numeric(offense$drive_start_yard_line[[i]][[2]])
  }
}
yard_line_start <- rep(0,6986)
for (i in 1:6986) {
  yard_line_start[i] <- offense$drive_start_100[i]
}

### Actual Simulation Stuff
home_team <- "PHI" ## set home team
## modify the punt data for expected point calculations
punts <- punts %>%
  mutate(home_team = home_team, posteam = "PIT") %>% ##adjust home team as needed
  mutate(kick_length = case_when(
    !is.na(kick_distance) ~ kick_distance,
    is.na(kick_distance) ~ yardline_before
  ))
## actually do those calculations and save, depending on if offensive team is home or away
punts <- nflfastR::calculate_expected_points(punts)
home_punts_ep <- -mean(punts$ep) ## these values are negative because they're the likelihood that
away_punts_ep <- -mean(punts$ep) ## the next score is Pittsburgh, which is bad for Philly

## the code below is the function to simulate a single drive. Change the line at the top 
## to make Philly home or away. Everything above here needs to be run only when opening R
## this is for the "aggressive coach," the "conservative coach" is below this
drive_aggressive <- function() {
  ## create two dataframes from EP calculations, one if the offense converts and
  ## one if the offense doesn't convert)
converted_play <- data.frame(season = 2020, home_team = home_team, posteam = "PHI",
                               roof = "outdoors", half_seconds_remaining = 1350, 
                               yardline_100 = 0, down = 1, ydstogo = 10,
                               posteam_timeouts_remaining = 3, defteam_timeouts_remaining = 3)

failed_play <- data.frame(season = 2020, home_team = home_team, posteam = "PIT",
                          roof = "outdoors", half_seconds_remaining = 1350, 
                          yardline_100 = 0, down = 1, ydstogo = 10,
                          posteam_timeouts_remaining = 3, defteam_timeouts_remaining = 3)
## create a new drive starting from a randomly selected yard line
yards_to_endzone <- as.numeric(sample(yard_line_start, 1))
down <- 1
yards_needed_1st_down <- 10
## adjust for rare case that the drive starts inside the opposing 10
if (yards_to_endzone <= 10) {
  yards_needed_1st_down <- yards_to_endzone
} 

## tell the user where the drive is starting from
cat("You are", yards_to_endzone, "yards from the endzone \n")
while (down <= 4) { ## 4 downs per series
  if (down <= 2) { ## pick the length of play based on down
    play <- sample(early_down_yardage, 1)
  } else {
    play <- sample(late_down_yardage, 1)
  }
  if (play >= yards_to_endzone) { ## immediately check if a touchdown was scored
    cat("The previous play went for", play, "yards, which was enough for a touchdown! 
        You scored 7 points. \n")
    points_on_drive <- 7
    break
  }
  if (play >= yards_needed_1st_down) { ## if we picked up a first down, reset the downs
    down <- 1
    yards_needed_1st_down <- 10
    yards_to_endzone <- yards_to_endzone - play
    if (yards_to_endzone <= 10) { ## again adjust if we're inside the 10 yard line
      yards_needed_1st_down <- yards_to_endzone
    }
  } else { ## we didn't pick up a first down, so add a down and adjust the yard lines
    down <- down + 1
    yards_needed_1st_down <- yards_needed_1st_down - play
    yards_to_endzone <- yards_to_endzone - play
  }
  cat("The previous play went for", play, "yards \n") ## tell the user how long the play was
  if (down == 5) { ## this is my method of stopping the loop after a failed 4th down
    print("You turned the ball over on downs")
    break
  }
  cat("It is now", down, "down \n") ## tell the user what the new situation is
  cat("You need", yards_needed_1st_down, "yards to pick up a first down \n")
  cat("You are", yards_to_endzone, "yards from the endzone \n")
  if (yards_to_endzone >= 100) { ## check if a safety happened
    print("The other team scored a safety. This is very bad")
    points_on_drive <- -2
  }
 
  if (down == 4){ ## the fun part: it's now fourth down, what do we do
    if (yards_to_endzone > 43) { ## my way around the "are all three options available" issue
                                 ## if the kick is longer than 60 yards, its go or punt
      pick_up <- 0 ## counter for the number of times we'd pick up the first down
      for (x in 1:1239) { ## go through every play on a late down, and see how many of them
        if (late_down_yardage[x] >= yards_needed_1st_down) { ## would result in a conversion
          pick_up <- pick_up + 1
        }
      }
      percent_convert <- pick_up/1239 ## then turn it into a percentage
      converted_play <- converted_play %>% ## mutate the converted and failed play data frames
        mutate(yardline_100 = yards_to_endzone + yards_needed_1st_down) ## to adjust them for
      failed_play <- failed_play %>% ## the current situation
        mutate(yardline_100 = 100 - yards_to_endzone)
      converted_play <- nflfastR::calculate_expected_points(converted_play) ## get the EP for both
      failed_play <- nflfastR::calculate_expected_points(failed_play) ## data frames
      ## get the weighted mean of the expected points
      ep_go_for_it <- percent_convert*converted_play$ep - (1-percent_convert)*failed_play$ep
      if (ep_go_for_it < home_punts_ep) { ## and here's the model calculations
        print("You punted. You scored 0 points") ## this is a punt, drive ends
        break
      } else { ## we go for it, so we treat it as a regular play
        play <- sample(late_down_yardage, 1)
        if (play >= yards_needed_1st_down) { ## we picked up enough yards
          cat("The previous play gained", play, "yards, which was enough for a first down. \n")
          ## The next two blocks of code are used to recreate the dataframes, since running
          ## the calculate_expected_points function twice breaks the sim
          converted_play <- data.frame(season = 2020, home_team = home_team, posteam = "PHI",
                                       roof = "outdoors", half_seconds_remaining = 1350, 
                                       yardline_100 = 0, down = 1, ydstogo = 10,
                                       posteam_timeouts_remaining = 3, defteam_timeouts_remaining = 3)
          failed_play <- data.frame(season = 2020, home_team = home_team, posteam = "PIT",
                                    roof = "outdoors", half_seconds_remaining = 1350, 
                                    yardline_100 = 0, down = 1, ydstogo = 10,
                                    posteam_timeouts_remaining = 3, defteam_timeouts_remaining = 3)
          down <- 1 ## reset the downs and move the yard lines
          yards_needed_1st_down <- 10
          yards_to_endzone <- yards_to_endzone - play
          if (yards_to_endzone <= 10) { ## same goal to go adjustment
            yards_needed_1st_down <- yards_to_endzone
          }
        } else { ## we didn't pick it up, the drive is over
          cat("The previous play gained", play, "yards, which was not enough for a first down.
              The drive is over. You scored 0 points.")
          break
        }
      }
    } else { ## this is the go or field goal section, a lot of it is the same as above
      ## I'll keep the comments light because of this
      pick_up <- 0
      for (x in 1:1239) {
        if (late_down_yardage[x] >= yards_needed_1st_down) {
          pick_up <- pick_up + 1
        }
      }
      percent_convert <- pick_up/1239
      converted_play <- converted_play %>%
        mutate(yardline_100 = yards_to_endzone + yards_needed_1st_down)
      failed_play <- failed_play %>%
        mutate(yardline_100 = 100 - yards_to_endzone)
      converted_play <- nflfastR::calculate_expected_points(converted_play)
      failed_play <- nflfastR::calculate_expected_points(failed_play)
      ep_go_for_it <- percent_convert*converted_play$ep - (1-percent_convert)*failed_play$ep
      length_of_kick <- yards_to_endzone + 17 ## length of field goal is yard line + 17
                                              ## 7 for the snap, 10 for the endzone
      if (length_of_kick <= 29 ) { ## based on kick length, get probability of made kick
        kick_prob <- 0.958
      } else if (length_of_kick >= 30 & length_of_kick <= 39 ) {
        kick_prob <- 0.882
      } else if (length_of_kick >= 40 & length_of_kick <= 49 ) {
        kick_prob <- 0.889
      } else if (length_of_kick >= 50 ) {
        kick_prob <- 0.571
      }
      ep_kick <- kick_prob * 3 ## expected points for a field goal is easy
      if (ep_go_for_it < ep_kick) { ## if model says kick the field goal
        kick_chance <- runif(1, 0, 1) ## this is my way of simulating the field goal
        if (kick_chance <= kick_prob) { ## if the random number is less than the probability
          print("The field goal was good! You scored 3 points") ## the kick is good
          break
        } else { ## if it isn't, kick is no good
          print("The field goal missed. You scored 0 points")
          break
        }
      } else { ## we go for it, treat it like a normal play
        play <- sample(late_down_yardage, 1)
        if (play >= yards_needed_1st_down) { ## we picked up a first down
          cat("The previous play gained", play, "yards, which was enough for a first down. \n")
          ## The next two blocks of code are used to recreate the dataframes, since running
          ## the calculate_expected_points function twice breaks the sim
          converted_play <- data.frame(season = 2020, home_team = home_team, posteam = "PHI",
                                       roof = "outdoors", half_seconds_remaining = 1350, 
                                       yardline_100 = 0, down = 1, ydstogo = 10,
                                       posteam_timeouts_remaining = 3, defteam_timeouts_remaining = 3)
          failed_play <- data.frame(season = 2020, home_team = home_team, posteam = "PIT",
                                    roof = "outdoors", half_seconds_remaining = 1350, 
                                    yardline_100 = 0, down = 1, ydstogo = 10,
                                    posteam_timeouts_remaining = 3, defteam_timeouts_remaining = 3)
          down <- 1 ## reset the chains
          yards_needed_1st_down <- 10
          yards_to_endzone <- yards_to_endzone - play
          if (yards_to_endzone <= 10) {
            yards_needed_1st_down <- yards_to_endzone
          }
        } else { ## didn't convert, drive is over
          cat("The previous play gained", play, "yards, which was not enough for a first down.
              The drive is over. You scored 0 points.")
          break
      } 
      }
   }
   
}
}
}

drive_aggressive()
## And that's it for the aggressive simulation The conservative one is similar and way more
## straightforward, so comments will be a lot lighter in this one

home_team <- "PIT"
## the code below is the function to simulate a single drive. Change the line above this 
## to make Philly home or away. Everything above here needs to be run only when opening R
drive_conservative <- function() {
  yards_to_endzone <- as.numeric(sample(yard_line_start, 1))
  down <- 1
  yards_needed_1st_down <- 10
  if (yards_to_endzone <= 10) {
    yards_needed_1st_down <- yards_to_endzone
  } 
  cat("You are", yards_to_endzone, "yards from the endzone \n")
  while (down <= 4) {
    if (down <= 2) {
      play <- sample(early_down_yardage, 1)
    } else if (down == 3) {
      play <- sample(late_down_yardage, 1)
    } else if (down == 4) { ## I found that for this sim I need to put fourth downs here, since
                            ## they're a separate entity (unlike the other sim, where 4th downs can
                            ## be treated like third downs on occasion)
      if (yards_to_endzone > 43) { ## We're using the same logic as before for kick vs punt
        print("You punted. You scorec zero points")
        break
      } else {
        length_of_kick <- yards_to_endzone + 17
        if (length_of_kick <= 29 ) {
          kick_prob <- 0.958
        } else if (length_of_kick >= 30 & length_of_kick <= 39 ) {
          kick_prob <- 0.882
        } else if (length_of_kick >= 40 & length_of_kick <= 49 ) {
          kick_prob <- 0.889
        } else if (length_of_kick >= 50 ) {
          kick_prob <- 0.571
        }
        kick_chance <- runif(1, 0, 1)
        if (kick_chance <= kick_prob) {
          print("The field goal was good! You scored 3 points")
          break
        } else {
          print("The field goal missed. You scored 0 points")
          break
        }
      }
    }
    if (play >= yards_to_endzone) {
      cat("The previous play went for", play, "yards, which was enough for a touchdown! You scored 7 points. \n")
      break
    }
    if (play >= yards_needed_1st_down) {
      down <- 1
      yards_needed_1st_down <- 10
      yards_to_endzone <- yards_to_endzone - play
      if (yards_to_endzone <= 10) {
        yards_needed_1st_down <- yards_to_endzone
      }
    } else {
      down <- down + 1
      yards_needed_1st_down <- yards_needed_1st_down - play
      yards_to_endzone <- yards_to_endzone - play
    }
    cat("The previous play went for", play, "yards \n")
    cat("It is now", down, "down \n")
    cat("You need", yards_needed_1st_down, "yards to pick up a first down \n")
    cat("You are", yards_to_endzone, "yards from the endzone \n")
    if (yards_to_endzone >= 100) {
      print("The other team scored a safety. This is very bad")
      break
    }
  }
}

drive_conservative()
## And that's the end of both simulations, and thus the end of the code for this project
## This was a lot of fun, even though the process has significant flaws, as I discuss in my paper
## In no way should this method be used to actually advise Doug Pederson, but it was a good way to 
## combine what we've learned in this class and how to simulate things with my wanting to get to know
## the nflfastR dataset.
## Lastly, I'm pretty sure this is the longest R file I've ever made
