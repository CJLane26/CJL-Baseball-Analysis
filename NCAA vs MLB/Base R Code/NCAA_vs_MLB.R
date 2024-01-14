library(baseballr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(gridExtra)
source("MLB_Teams.R")
source("MLB_Batting_Data.R")
source("MLB_Pitching_Data.R")
source("NCAA_Major_Teams.R")
source("NCAA_Schedule_to_Record.R")
source("NCAA_Batting_Data.R")
source("NCAA_Pitching_Data.R")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CALL FUNCTIONS TO RETRIEVE AND CALCULATE MLB & NCAA AVERAGES AND WIN% DATA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Use baseballr library through functions in separate files to pull MLB & NCAA data from Baseball-Reference.com
MLB_Teams <- get_MLB_teams()
MLB_Batting_Data <- get_MLB_batting_data(MLB_Teams)
MLB_Pitching_Data <- get_MLB_pitching_data(MLB_Teams)


NCAA_Teams <- get_NCAA_teams()
NCAA_Batting_Data <- get_NCAA_batting_data(NCAA_Teams)
NCAA_win_loss <- get_NCAA_win_loss(NCAA_Batting_Data)
NCAA_Pitching_Data <- get_NCAA_pitching_data(NCAA_Teams)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# MERGE NCAA & MLB WIN%, Batting Data, and Pitching Data by Team
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
MLB_Batting_vs_WPer <- merge(MLB_Teams, MLB_Batting_Data)
MLB_Averages_vs_WPer <- merge(MLB_Batting_vs_WPer, MLB_Pitching_Data)

NCAA_Batting_vs_WPer <- merge(NCAA_Batting_Data, NCAA_win_loss)
NCAA_Averages_vs_WPer <- merge(NCAA_Batting_vs_WPer, NCAA_Pitching_Data)
NCAA_Averages_vs_WPer <- NCAA_Averages_vs_WPer %>% rename(Conference = conference)
NCAA_Averages_vs_WPer <- NCAA_Averages_vs_WPer %>% mutate(Conference = case_when(
  Conference == "Mountain West" ~ "MW",
  Conference == "Atlantic 10" ~ "Atl 10",
  Conference == "Pac-12" ~ "PAC 12",
  Conference == "Sun Belt" ~ "SBC",
  TRUE ~ Conference
))
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CALCULATE CORRELATION BETWEEN wOBA & OPS and Win% for MLB & NCAA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
MLB_OPS_cor <- cor(MLB_Averages_vs_WPer$OPS, MLB_Averages_vs_WPer$win_loss)
MLB_wOBA_cor <- cor(MLB_Averages_vs_WPer$wOBA, MLB_Averages_vs_WPer$win_loss)
MLB_ERA_cor <- cor(MLB_Averages_vs_WPer$ERA, MLB_Averages_vs_WPer$win_loss)
MLB_FIP_cor <- cor(MLB_Averages_vs_WPer$FIP, MLB_Averages_vs_WPer$win_loss)
MLB_wOBA_against_cor <- cor(MLB_Averages_vs_WPer$wOBA_against, MLB_Averages_vs_WPer$win_loss)
MLB_WHIP_cor <- cor(MLB_Averages_vs_WPer$WHIP, MLB_Averages_vs_WPer$win_loss)

NCAA_OPS_cor <- cor(NCAA_Averages_vs_WPer$avg_OPS, NCAA_Averages_vs_WPer$win_loss)
NCAA_wOBA_cor <- cor(NCAA_Averages_vs_WPer$avg_wOBA, NCAA_Averages_vs_WPer$win_loss)
NCAA_ERA_cor <- cor(NCAA_Averages_vs_WPer$ERA, NCAA_Averages_vs_WPer$win_loss)
NCAA_FIP_cor <- cor(NCAA_Averages_vs_WPer$FIP, NCAA_Averages_vs_WPer$win_loss)
NCAA_wOBA_against_cor <- cor(NCAA_Averages_vs_WPer$wOBA_against, NCAA_Averages_vs_WPer$win_loss)
NCAA_WHIP_cor <- cor(NCAA_Averages_vs_WPer$WHIP, NCAA_Averages_vs_WPer$win_loss)

correlation_table <- tibble(
  Level = c("MLB", "NCAA"),
  OPS = c(MLB_OPS_cor, NCAA_OPS_cor),
  wOBA = c(MLB_wOBA_cor, NCAA_wOBA_cor),
  ERA = c(MLB_ERA_cor, NCAA_ERA_cor),
  wOBA_Against = c(MLB_wOBA_against_cor, NCAA_wOBA_against_cor),
  FIP = c(MLB_FIP_cor, NCAA_FIP_cor),
  WHIP = c(MLB_WHIP_cor, NCAA_WHIP_cor)
)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create Plots for Team Stats vs Win% for MLB & NCAA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

MLB_plot_OPS <- ggplot(MLB_Averages_vs_WPer, aes(x=OPS, y=win_loss, color = League))+
  geom_point()+
  geom_smooth(method='lm', formula = y ~ x, color='red')+
  labs(title = paste("MLB OPS vs Win % | r =", round(MLB_OPS_cor, 4)), x="Team OPS",y="Win%")+
  theme_minimal()

MLB_plot_wOBA <- ggplot(MLB_Averages_vs_WPer, aes(x=wOBA, y=win_loss, color = League))+
  geom_point()+
  geom_smooth(method='lm', formula = y ~ x, color='red')+
  labs(title = paste("MLB wOBA vs Win % | r =", round(MLB_wOBA_cor, 4)), x="Team wOBA",y="Win%")+
  theme_minimal()

MLB_plot_ERA <- ggplot(MLB_Averages_vs_WPer, aes(x=ERA, y=win_loss, color = League))+
  geom_point()+
  geom_smooth(method='lm', formula = y ~ x, color='red')+
  labs(title = paste("MLB ERA vs Win % | r =", round(MLB_ERA_cor, 4)), x="Team ERA",y="Win%")+
  theme_minimal()

MLB_plot_FIP <- ggplot(MLB_Averages_vs_WPer, aes(x=FIP, y=win_loss, color = League))+
  geom_point()+
  geom_smooth(method='lm', formula = y ~ x, color='red')+
  labs(title = paste("MLB FIP vs Win % | r =", round(MLB_FIP_cor, 4)), x="Team FIP",y="Win%")+
  theme_minimal()

MLB_plot_wOBA_against <- ggplot(MLB_Averages_vs_WPer, aes(x=wOBA_against, y=win_loss, color = League))+
  geom_point()+
  geom_smooth(method='lm', formula = y ~ x, color='red')+
  labs(title = paste("MLB wOBA Against vs Win % | r =", round(MLB_wOBA_against_cor, 4)), x="Team wOBA Against",y="Win%")+
  theme_minimal()

MLB_plot_WHIP <- ggplot(MLB_Averages_vs_WPer, aes(x=WHIP, y=win_loss, color = League))+
  geom_point()+
  geom_smooth(method='lm', formula = y ~ x, color='red')+
  labs(title = paste("MLB WHIP vs Win % | r =", round(MLB_WHIP_cor, 4)), x="Team WHIP",y="Win%")+
  theme_minimal()

NCAA_plot_OPS <- ggplot(NCAA_Averages_vs_WPer, aes(x=avg_OPS, y=win_loss, color = Conference))+
  geom_point()+
  geom_smooth(method='lm', formula = y ~ x, color='red')+
  labs(title = paste("NCAA OPS vs Win % | r =", round(NCAA_OPS_cor, 4)), x="Starting 9 Average OPS",y="Win%")+
  theme_minimal()

NCAA_plot_wOBA <- ggplot(NCAA_Averages_vs_WPer, aes(x=avg_wOBA, y=win_loss, color = Conference))+
  geom_point()+
  geom_smooth(method='lm', formula = y ~ x, color='red')+
  labs(title = paste("NCAA wOBA vs Win % | r = ", round(NCAA_wOBA_cor, 4)), x="Starting 9 Average wOBA",y="Win%")+
  theme_minimal()

NCAA_plot_ERA <- ggplot(NCAA_Averages_vs_WPer, aes(x=ERA, y=win_loss, color = Conference))+
  geom_point()+
  geom_smooth(method='lm', formula = y ~ x, color='red')+
  labs(title = paste("NCAA ERA vs Win % | r =", round(NCAA_ERA_cor, 4)), x="Team ERA",y="Win%")+
  theme_minimal()

NCAA_plot_FIP <- ggplot(NCAA_Averages_vs_WPer, aes(x=FIP, y=win_loss, color = Conference))+
  geom_point()+
  geom_smooth(method='lm', formula = y ~ x, color='red')+
  labs(title = paste("NCAA FIP vs Win % | r =", round(NCAA_FIP_cor, 4)), x="Team FIP",y="Win%")+
  theme_minimal()

NCAA_plot_wOBA_against <- ggplot(NCAA_Averages_vs_WPer, aes(x=wOBA_against, y=win_loss, color = Conference))+
  geom_point()+
  geom_smooth(method='lm', formula = y ~ x, color='red')+
  labs(title = paste("NCAA wOBA Against vs Win % | r =", round(NCAA_wOBA_against_cor, 4)), x="Team wOBA Against",y="Win%")+
  theme_minimal()

NCAA_plot_WHIP <- ggplot(NCAA_Averages_vs_WPer, aes(x=WHIP, y=win_loss, color = Conference))+
  geom_point()+
  geom_smooth(method='lm', formula = y ~ x, color='red')+
  labs(title = paste("NCAA WHIP vs Win % | r =", round(NCAA_WHIP_cor, 4)), x="Team WHIP",y="Win%")+
  theme_minimal()


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Print Plots and Correlation Table for Team Stats Win% for MLB & NCAA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

grid.arrange(MLB_plot_OPS, MLB_plot_wOBA, NCAA_plot_OPS, NCAA_plot_wOBA, ncol=2)
grid.arrange(MLB_plot_ERA, MLB_plot_WHIP, NCAA_plot_ERA, NCAA_plot_WHIP, ncol=2)
grid.arrange(MLB_plot_FIP, MLB_plot_wOBA_against, NCAA_plot_FIP, NCAA_plot_wOBA_against, ncol=2)

print(correlation_table)
