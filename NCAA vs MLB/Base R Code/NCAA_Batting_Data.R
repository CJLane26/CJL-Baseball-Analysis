library(baseballr)
library(dplyr)
library(ggplot2)
library(tidyr)
source("H:\\Coding\\R Workspace\\Studio Phillies\\NCAA vs MLB\\NCAA_schedule_to_record.R")


get_NCAA_batting_data <- function(major_conference_teams) {
  cat("This data call takes a while (~3-4 minutes) because it is calling and parsing batting data for 90+ teams.\n\nThere should be 3 error messages for teams whose data is not available, but the code will continue to execute as expected.\n\nPlease be patient. :)\n")
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # LOOP FOR FUNCTION TO IMPORT AND FILTER BATTING DATA FOR SELECTED TEAM
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  NCAA_batters <- tibble()
  for (team in major_conference_teams$team_id) {
    batters <- append_lineup(team)
    if (inherits(batters, "tbl")) {
      NCAA_batters <- rbind(NCAA_batters, batters)
    } else {
      next
    }
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # PARSING BATTER DATA FOR STARTING 9
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  NCAA_starters <- NCAA_batters %>% group_by(team_id) %>%
    arrange(desc(GS)) %>% slice(1:9)
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # CLEANING DATA FOR CALCULATING OPS AND wOBA
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Column names '2B' and '3B' are automatically read in as 'X2B' and 'X3B', respectively
  NCAA_starters <- NCAA_starters %>% rename(SO = K,
                                            uBB = BB,
                                            X2B = `2B`,
                                            X3B = `3B`,
                                            OBP = OBPct,
                                            SLG = SlgPct,
                                            season = year)
  
  #Clean data by replacing 'NA' values with 0
  NCAA_starters <- NCAA_starters %>% mutate(across(c(SH, X2B, X3B, HR, SF, HBP), ~ifelse(is.na(.), 0, .)))
  # Calculate Singles by subtracting Extra Base Hits from Hits
  NCAA_starters <- NCAA_starters %>% mutate(X1B = H-(X2B+X3B+HR))
  # Calculate OPS
  NCAA_starters_OPS <- NCAA_starters %>% mutate(OPS = OBP+SLG)
  # Select relevant data
  NCAA_starters_OPS <- NCAA_starters_OPS %>% select(season, team_name, team_id, conference_id, conference, player_name, GS, BA, OBP, SLG, OPS, AB, X1B, X2B, X3B, HR, uBB, HBP, SF, SH, SO)
  # Caculate player wOBA
  NCAA_starters_OPS_wOBA <- woba_plus(NCAA_starters_OPS)
  # Calculate team average wOBA & OPS
  NCAA_team_averages <- NCAA_starters_OPS_wOBA %>% group_by(team_id, season, team_name, conference) %>% 
    summarize(avg_wOBA = mean(wOBA, na.rm=TRUE),
              avg_OPS = mean(OPS, na.rm=TRUE))
  
  return(NCAA_team_averages)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FUNCTION TO IMPORT TEAM BATTER DATA AND CATCH ERRORS IN API
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
append_lineup <- function (team_id) {
  tryCatch({
    team_batters <- ncaa_team_player_stats(team_id=team_id, 2023, type = "batting")
    return (team_batters)
  }, error = function(err) {return(Null)})
  
  return (team_batters)
}