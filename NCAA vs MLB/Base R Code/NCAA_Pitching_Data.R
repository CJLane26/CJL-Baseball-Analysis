library(baseballr)
library(dplyr)
library(ggplot2)
library(tidyr)

get_NCAA_pitching_data <- function(major_conference_teams) {
  cat("This data call takes a while (~3-4 minutes) because it is calling and parsing pitching data for 90+ teams.\n\nThere should be 3 error messages for teams whose data is not available, but the code will continue to execute as expected.\n\nPlease be patient. :)\n")
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # LOOP FOR FUNCTION TO IMPORT AND FILTER Pitching DATA FOR SELECTED TEAM
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  NCAA_pitchers <- tibble()
  for (team in major_conference_teams$team_id) {
    pitchers <- append_pitchers(team)
    if (inherits(pitchers, "tbl")) {
      NCAA_pitchers <- rbind(NCAA_pitchers, pitchers)
    } else {
      next
    }
  }
  NCAA_pitchers <- NCAA_pitchers %>% filter(App != "NA")

  # Columns '2B-A', '3B-A','HR-A', and ''P-OAB' are read in as 'X2B.A', 'X3B.A','HR.A', and 'P.OAB' from .csv
  # Columns are called from API with original labels
  NCAA_pitchers <- NCAA_pitchers %>% rename(season = year,
                                            HR = `HR-A`,
                                            X2B = `2B-A`,
                                            X3B = `3B-A`,
                                            uBB = BB,
                                            SH = SHA,
                                            HBP = HB,
                                            SF = SFA,
                                            AB = `P-OAB`)
  
  
  NA_to_Zero <- c('GS', 'IP', 'H', 'R', 'ER', 'SO', 'SHO', 'AB', 'uBB', 'X2B', 'X3B', 'HR', 'SH', 'SF', 'HBP', 'WP', 'GO', 'FO', 'W', 'L', 'KL', 'SV')
  
  NCAA_pitchers_parsedzero <- NCAA_pitchers %>% mutate(across(all_of(NA_to_Zero), ~ifelse(is.na(.), 0, .)))
  
  relevent_cols <- c('season', 'team_name', 'team_id', 'conference_id', 'conference', 'player_name', 'GP', 'App', 'GS', 'ERA', 'IP', 'H', 'R', 'ER', 'uBB', 'SO', 'BF', 'X2B', 'X3B', 'HR', 'HBP', 'SH', 'AB')
  
  NCAA_pitchers_rel <- NCAA_pitchers_parsedzero %>% select(all_of(relevent_cols)) %>% group_by(team_id)
  
  NCAA_pitchers_rel <- NCAA_pitchers_rel %>% mutate(X1B = H - (X2B+X3B+HR))
  
  NCAA_team_pitching <- NCAA_pitchers_rel %>% group_by(team_id, season, team_name, conference) %>% 
    summarize(IP = sum(IP),
              ER = sum(ER),
              BF = sum(BF),
              AB = sum(AB),
              H = sum(H),
              X1B = sum(X1B),
              X2B = sum(X2B),
              X3B = sum(X3B),
              HR = sum(HR),
              uBB = sum(uBB),
              SO = sum(SO),
              HBP = sum(HBP),
              SH = sum(SH))
  
  NCAA_team_ERA <- NCAA_team_pitching %>% mutate(ERA = 9*ER/IP)
  NCAA_team_ERA_FIP <- fip_plus(NCAA_team_ERA)
  NCAA_team_ERA_FIP_WHIP <- NCAA_team_ERA_FIP %>% mutate(WHIP = (uBB+H)/IP)
  NCAA_pitching_rel <- NCAA_team_ERA_FIP_WHIP %>% select(team_id, season, team_name, conference, ERA, WHIP, FIP, wOBA_against)
  
  return(NCAA_pitching_rel)
}

append_pitchers <- function (team_id) {
  tryCatch({
    team_pitchers <- ncaa_team_player_stats(team_id=team_id, 2023, type = "pitching")
    return (team_pitchers)
  }, error = function(err) {return(NULL)})
}