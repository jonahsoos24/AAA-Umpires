library(tidyverse)
library(baseballr)
library(lubridate)

# Getting a vector of possible week start dates

Dates <- seq.Date(
  from = as.Date("2023-03-31"),
  to = as.Date("2023-04-07"),
  by = 1
)

# Cutting out dates outside of baseball season
Dates <- Dates[between(month(Dates), 3, 10)]

# Creating object for data to be binded into
game_pks <- NULL

for(StartDate in 1:length(Dates)){
  
  game_pks <- game_pks %>%
    rbind(
      suppressMessages(
        get_game_pks_mlb(
          date = Dates[StartDate],
          level_ids = c(11)
        )
      ), use.names = TRUE, fill = TRUE
    )
  
  # printing loop progress
  print(StartDate/length(Dates))
}
game_pks <- game_pks %>%
  filter(!is.na(game_pk))

write_csv(game_pks, "AA_MLB_Game_PKs.csv")
game_pks <- read_csv("AA_MLB_Game_PKs.csv")

game_pks_nocancel <- game_pks %>%
  filter(status.detailedState != "Cancelled" & status.detailedState != "Postponed")
game_pks_nocancel <- distinct(game_pks_nocancel)

game_pks_nocancel$game_pk <- as.numeric(game_pks_nocancel$game_pk)

team <- teams_lu_table

game_pks_nocancel <- game_pks_nocancel %>%
  left_join(team, by=c("teams.home.team.id" = "id"))

MLB <- game_pks_nocancel %>%
  filter(sport.id == 1)
write_csv(MLB, "game_pk_MLB.csv")

AA <- game_pks_nocancel %>%
  filter(sport.id == 12)

pbp_MLB <- NULL

pbp_MLB <- map_df(.x = MLB$game_pk[1:5440], 
                  ~mlb_pbp(.x), 
                  .progress = TRUE)

write_csv(pbp_MLB, "pbp_MLB.csv")

pbp_AA <- NULL

pbp_AA <- map_df(.x = AA$game_pk[1:3747], 
                     ~mlb_pbp(.x), 
                     .progress = TRUE)

write_csv(pbp_AA, "pbp_AA.csv")

data <- rbind(pbp_MLB ,pbp_AA)

write_csv(data, "pbp_AA_MLB.csv")
pbp_test <- stats_api_live_empty_df

stats_api_live_empty_df <- map_df(.x = game_pks_nocancel$game_pk[1:97], 
                   ~mlb_pbp(.x), 
                   .progress = TRUE)

mlb_game_context_metrics(game_pk = 722652, timecode = 100)

umpires <- load_umpire_ids()

pbp_test <- pbp_test %>% left_join(umpires, by=c("game_pk" = "game_pk"))

devtools::install_github("keberwein/mlbgameday")
library(mlbgameday)

mlbgameday::get_payload(start = "2017-03-31", end = "2017-04-07", league = "aaa")
