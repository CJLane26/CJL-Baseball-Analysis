library(baseballr)
library(dplyr)
library(tidyr)

get_MLB_batting_data <- function(Teams) {
  
  Team_Batting_Stats <- tibble()
  for (team in Teams$id) {
    team_id <- team
    League <- Teams$League[Teams$id == team_id]
    abbv <- Teams$abbreviation[Teams$id == team_id]
    team_batting <- mlb_team_stats(team_id = team_id, stat_type = 'season', stat_group = 'hitting', season = 2023)
    results <- cbind(tibble(id=team_id, abbreviation = abbv, League = League), team_batting)
    Team_Batting_Stats <- rbind(Team_Batting_Stats, results)
  }

    Team_Batting_Stats_Parsed <- Team_Batting_Stats %>% rename(
    R = runs,
    
    X2B = doubles,
    X3B = triples,
    HR = home_runs,
    SO = strike_outs,
    uBB = base_on_balls,
    H = hits,
    HBP = hit_by_pitch,
    OPS = ops,
    SH = sac_bunts,
    AB = at_bats,
    SF = sac_flies
  )
  
  Team_Batting_Stats_Parsed <- Team_Batting_Stats_Parsed %>% mutate(X1B = H - (X2B+X3B+HR))
  rel_data <- c('season', 'id', 'abbreviation', 'League', 'OPS', 'AB', 'H', 'X1B', 'X2B',
                'X3B', 'HR', 'uBB', 'SO', 'HBP', 'SH', 'R', 'SF')
  
  Team_Batting_Stats_Parsed <- Team_Batting_Stats_Parsed %>% select(all_of(rel_data))
  Team_Batting_Stats_Parsed <- Team_Batting_Stats_Parsed %>% mutate(season = (as.integer(season)))
  Team_Batting_Stats_Parsed <- Team_Batting_Stats_Parsed %>% mutate(OPS = (as.numeric(OPS)))
  
  MLB_Batting_wOBA <- woba_plus(Team_Batting_Stats_Parsed)
  MLB_Batting_wOBA <- woba_plus(MLB_Batting_wOBA) %>% select(id, abbreviation, League, OPS, wOBA)
  
  return(MLB_Batting_wOBA)
}
