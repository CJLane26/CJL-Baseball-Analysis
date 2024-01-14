library(baseballr)
library(dplyr)
library(ggplot2)
library(tidyr)
source("H:\\Coding\\R Workspace\\Studio Phillies\\NCAA vs MLB\\NCAA_schedule_to_record.R")


get_NCAA_teams <- function() {
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # IMPORTING TEAMS DATA AND PARSING FOR DESIRED TEAMS BY CONFERENCE
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  college_teams <- load_ncaa_baseball_teams()
  college_teams <- college_teams %>% group_by(team_name) %>% filter(year == max(year)) %>%
    ungroup() %>% select(team_id,team_name, division, conference, year)
  division_1_teams <- college_teams %>% filter(division == 1)
  conferences <- unique(division_1_teams$conference)
  major_conference_teams <- division_1_teams %>% filter(conference %in% c("Big 12", "ACC", "Pac-12", "SEC", "Big Ten", "Mountain West", "Atlantic 10", "Sun Belt"))
  return(major_conference_teams)  
}

get_NCAA_win_loss <- function(NCAA_team_averages) {
  cat("This data call takes a while (~2-3 minutes) because it is calling and parsing schedule data for 90+ teams.\n\nPlease be patient. :)\n")
  NCAA_team_records <- tibble()
  for (i in 1:nrow(NCAA_team_averages)) {
    team_id <- NCAA_team_averages$team_id[i]
    team_name <- NCAA_team_averages$team_name[i]
    team_record <- get_record(team_id, team_name)
    if (inherits(team_record, "tbl")) {
      NCAA_team_records <- rbind(NCAA_team_records, team_record)
    } else {
      next
    }
  }
  NCAA_records <- NCAA_team_records %>% select(team_id, win_loss)
  return(NCAA_records)
}