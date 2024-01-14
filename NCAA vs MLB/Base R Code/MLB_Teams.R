library(baseballr)
library(dplyr)
library(ggplot2)
library(tidyr)

MLB_2023_start <- as.Date("2023-03-28")
MLB_2023_end <- as.Date("2023-10-01")

get_MLB_teams <- function() {
  mlb_teams <- head(teams_lu_table, 30)
  
  mlb_teams_rel <- mlb_teams %>% select(id, name, abbreviation, locationName, league.id,league.name) #CWS, KC, SD, SF, TB, Anaheim, Bronx, Arlington
  
  mlb_teams_rel <- mlb_teams_rel %>% mutate(abbreviation=ifelse(name=="Chicago White Sox", "CHW", abbreviation)) %>%
    mutate(abbreviation=ifelse(locationName=="San Francisco", "SFG", abbreviation)) %>%
    mutate(abbreviation=ifelse(locationName=="San Diego", "SDP", abbreviation)) %>%
    mutate(abbreviation=ifelse(locationName=="Tampa Bay", "TBR", abbreviation)) %>%
    mutate(abbreviation=ifelse(locationName=="Kansas City", "KCR", abbreviation)) %>%
    mutate(locationName=ifelse(abbreviation=="LAA", "Los Angeles", locationName)) %>%
    mutate(locationName=ifelse(abbreviation=="TEX", "Texas", locationName)) %>%
    mutate(locationName=ifelse(abbreviation=="NYY", "New York", locationName))%>%
    mutate(locationName=ifelse(abbreviation=="MIN", "Minnesota", locationName)) %>%
    mutate(locationName=ifelse(abbreviation=="ARI", "Arizona", locationName)) %>%
    mutate(abbreviation=ifelse(locationName=="Washington", "WSN", abbreviation)) %>%
    mutate(locationName=ifelse(abbreviation=="COL", "Colorado", locationName))
  
  mlb_teams_rel <- mlb_teams_rel %>%
    mutate(League = case_when(
      league.name == "American League" ~ "AL",
      league.name == "National League" ~ "NL",
      TRUE ~ NA_character_
    ))
  
  AL_standings <- bref_standings_on_date(MLB_2023_end+1, division= "AL Overall")
  NL_standings <- bref_standings_on_date(MLB_2023_end+1, division= "NL Overall")
  MLB_standings <- rbind(AL_standings,NL_standings)
  WinPer_2023 <- MLB_standings %>% select(Tm, 'W-L%')
  WinPer_2023 <- WinPer_2023 %>% rename("win_loss" = "W-L%")
  
  WinPer_2023_id <- merge(mlb_teams_rel, WinPer_2023, by.x = 'abbreviation', by.y = 'Tm', all.x=TRUE)
  WinPer_2023_id <- WinPer_2023_id %>% select(id, abbreviation, League, win_loss)

  return(WinPer_2023_id)
}