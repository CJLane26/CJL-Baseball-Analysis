library(baseballr)
library(dplyr)
library(ggplot2)
library(tidyr)

get_MLB_pitching_data <- function(Teams) {
  
  Team_Pitching_Stats <- tibble()
  for (team in Teams$id) {
    team_id <- team
    League <- Teams$League[Teams$id == team_id]
    abbv <- Teams$abbreviation[Teams$id == team_id]
    team_pitching <- mlb_team_stats(team_id = team_id, stat_type = 'season', stat_group = 'pitching', season = 2023)
    results <- cbind(tibble(id=team_id, abbreviation = abbv, League = League), team_pitching)
    Team_Pitching_Stats <- rbind(Team_Pitching_Stats, results)
  }
  
  Team_Pitching_Stats_Parsed <- Team_Pitching_Stats %>% rename(
    R = runs,
    X2B = doubles,
    X3B = triples,
    HR = home_runs,
    SO = strike_outs,
    uBB = base_on_balls,
    H = hits,
    HBP = hit_by_pitch,
    OPS = ops,
    ERA = era,
    IP = innings_pitched,
    WHIP = whip,
    BF = batters_faced,
    SH = sac_bunts,
    AB = at_bats
  )
  
  Team_Pitching_Stats_Parsed <- Team_Pitching_Stats_Parsed %>% mutate(X1B = H - (X2B+X3B+HR))
  rel_data <- c('season', 'id', 'abbreviation', 'League', 'ERA', 'OPS','WHIP', 'IP', 'BF', 'AB', 'X1B', 'X2B',
                'X3B', 'HR', 'uBB', 'SO', 'HBP', 'SH','R')
  Team_Pitching_Stats_Parsed <- Team_Pitching_Stats_Parsed %>% select(all_of(rel_data))
  Team_Pitching_Stats_Parsed <- Team_Pitching_Stats_Parsed %>% mutate(season = (as.integer(season)))
  Team_Pitching_Stats_Parsed <- Team_Pitching_Stats_Parsed %>% mutate(IP = (as.numeric(IP)))
  Team_Pitching_Stats_Parsed <- Team_Pitching_Stats_Parsed %>% mutate(WHIP = (as.numeric(WHIP)))
  Team_Pitching_Stats_Parsed <- Team_Pitching_Stats_Parsed %>% mutate(OPS = (as.numeric(OPS)))
  Team_Pitching_Stats_Parsed <- Team_Pitching_Stats_Parsed %>% mutate(ERA = (as.numeric(ERA)))
  
  MLB_Pitching_FIP <- fip_plus(Team_Pitching_Stats_Parsed)
  MLB_Pitching_rel <- MLB_Pitching_FIP %>% select(id, abbreviation, League, ERA, WHIP, FIP, wOBA_against)
  
  return(MLB_Pitching_rel)
}