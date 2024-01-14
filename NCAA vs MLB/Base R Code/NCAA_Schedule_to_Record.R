library(baseballr)
library(dplyr)
library(tidyr)


end_date <- as.Date("2023-05-22")

get_record <- function(teamid, teamname) {
  schedule <- retrieve_schedule(teamid)
  home_schedule <- get_home_schedule(teamid, teamname, schedule)
  away_schedule <- get_away_schedule(teamid, teamname, schedule)
  parsed_schedule <- parse_schedule(home_schedule, away_schedule)
  
  team_record <- parsed_schedule %>% group_by(team_id, team_name) %>%
    summarize(
      wins = sum(Result=='W'),
      losses = sum(Result=='L'),
      win_loss = wins/(wins+losses),
      total_games = wins+losses
    ) %>%
    mutate(win_loss = win_loss, total_games=total_games)
  return(team_record)
}


parse_schedule <- function(homeschedule, awayschedule) {
  combined_schedule <- rbind(homeschedule, awayschedule)
  combined_schedule$date <- as.Date(combined_schedule$date, format = "%m/%d/%Y")
  combined_schedule <- combined_schedule %>% arrange(date) %>% filter(date <= end_date)%>% 
    mutate(Result = ifelse(Team_Score>Opponent_Score, 'W', 'L'))
  return(combined_schedule)
}

# Get team schedule by team ID
retrieve_schedule <- function(teamid) {
  Team_schedule <- ncaa_schedule_info(team_id = teamid, year = 2023, pbp_links = FALSE)
  return(Team_schedule)
}


get_home_schedule <- function(teamid, teamname, teamschedule) {
  home_schedule <- teamschedule %>% filter(home_team_id == teamid) %>% select(date, home_team_id, home_team, home_team_score, away_team, away_team_score, away_team_conference)
  home_schedule <- home_schedule %>% rename(team_name = home_team, team_id = home_team_id, "Team_Score" = home_team_score, "Opponent" = away_team, "Opponent_Score" = away_team_score, "Opponent_conference" = away_team_conference) %>%
    mutate(HA = 'H')
  return(home_schedule)
  
}

get_away_schedule <- function(teamid, teamname, teamschedule) {
  away_schedule <- teamschedule %>% filter(away_team_id==teamid) %>% select(date, home_team, home_team_score, home_team_conference, away_team_id, away_team, away_team_score)
  away_schedule <- away_schedule %>% rename(team_name = away_team, team_id = away_team_id, "Team_Score" = away_team_score, "Opponent" = home_team, "Opponent_Score" = home_team_score, "Opponent_conference" = home_team_conference) %>%
    mutate(HA = 'A')
  return(away_schedule)
}
