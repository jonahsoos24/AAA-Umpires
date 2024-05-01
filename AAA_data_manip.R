library(tidyverse)
library(ggplot2)
library(baseballr)

data <- read_csv("AAA_pbp.csv")
game_pks_AAA <- read_csv("AAA_Game_PKs.csv")

test_review <- data %>%
  filter(details.hasReview == TRUE)

team_ids <- game_pks_AAA %>%
  select(teams.home.team.id, teams.home.team.name) %>%
  distinct()
colnames(team_ids)[colnames(team_ids) == "teams.home.team.id"] <- "id"

data <- data %>%
  left_join(team_ids, by=c("home_team" = "teams.home.team.name")) %>%
  left_join(team_ids, by=c("away_team" = "teams.home.team.name"))

data_mc <- data %>%
  mutate(
    challenge_dummy = ifelse(!is.na(reviewDetails.inProgress.x) |
                             !is.na(reviewDetails.inProgress.y) |
                             !is.na(reviewDetails.inProgress), 1, 0),
    Is_Overturned = case_when(
      challenge_dummy == 1 & reviewDetails.isOverturned.x == FALSE ~ 0,
      challenge_dummy == 1 & reviewDetails.isOverturned == FALSE ~ 0,
      challenge_dummy == 1 & reviewDetails.isOverturned.y == FALSE ~ 0,
      challenge_dummy == 1 & reviewDetails.isOverturned.x == TRUE ~ 1,
      challenge_dummy == 1 & reviewDetails.isOverturned == TRUE ~ 1,
      challenge_dummy == 1 & reviewDetails.isOverturned.y == TRUE ~ 1,
      TRUE ~ NA
    ),
    Challenge_Team = ifelse(details.hasReview == TRUE & !is.na(reviewDetails.challengeTeamId.x) & reviewDetails.challengeTeamId.x == id.x, home_team,
      ifelse(details.hasReview == TRUE & !is.na(reviewDetails.challengeTeamId.x) & reviewDetails.challengeTeamId.x == id.y, away_team,
      ifelse(details.hasReview == TRUE & !is.na(reviewDetails.challengeTeamId) & reviewDetails.challengeTeamId == id.x, home_team,
      ifelse(details.hasReview == TRUE & !is.na(reviewDetails.challengeTeamId) & reviewDetails.challengeTeamId == id.y, away_team,
      ifelse(!is.na(reviewDetails.challengeTeamId.y) & reviewDetails.challengeTeamId.y == id.x, home_team,
      ifelse(!is.na(reviewDetails.challengeTeamId.y) & reviewDetails.challengeTeamId.y == id.y, away_team,
      NA)))))
    ),
    Batting_Team_Challenge = 
      ifelse(Challenge_Team == home_team & about.halfInning == "bottom", 1,
      ifelse(Challenge_Team == away_team & about.halfInning == "top", 1,
      ifelse(!is.na(challenge_dummy), 0, NA)
      ))
  )

sum(data_mc$challenge_dummy) #6605 Challenges

cleaned_data <- data_mc %>%
  filter(isPitch == TRUE) %>%
  select(1:2, 8:10, 21:25, 28:29, 46, 48, 50, 52:53, 64:66, 68, 70, 73, 75, 81, 87:88, 83, 95:96, 101:102, 104, 110:113, 121:125, 165, 172:177) %>%
  select(31, 1:2, 26, 28, 44, 25, 45, 26, 30, 23:24, 29, 21:22, 3:6, 8:10, 13:15, 16:20, 11:12, 36:37, 32:35, 38:42, 46:49)

pk_merge <- game_pks_AAA %>%
  select(1, 9, 11, 16, 20, 21)
pk_merge <- pk_merge[-1,]

cleaned_data$game_pk <- as.numeric(cleaned_data$game_pk)
pk_merge$game_pk <- as.numeric(pk_merge$game_pk)

cleaned_data <- cleaned_data %>%
  left_join(pk_merge, by=c("game_pk" = "game_pk"))

pbp_2023 <- cleaned_data %>%
  filter(game_date >= "2023-03-31") #719350 Observations

##Removing Duplicates from the data (because of seriesGameNumber & gameNumber)
pbp_2023 <- pbp_2023 %>%
  group_by(game_pk) %>%
  filter((gameNumber == first(gameNumber) &
          seriesGameNumber == first(seriesGameNumber) &
          gamesInSeries == first(gamesInSeries))) %>%
  ungroup() %>%
  distinct()

challenge_test <- pbp_2023 %>%
  group_by(game_pk) %>%
  summarise(
    Number_Of_Challenges = sum(challenge_dummy, na.rm = TRUE)
  )

pbp_2023 <- pbp_2023 %>%
  full_join(challenge_test, by=c("game_pk" = "game_pk"))

pbp_2023 <- distinct(pbp_2023) #690,905 Observations

pbp_2023 <- pbp_2023 %>%
  arrange(game_pk, atBatIndex, pitchNumber) %>%
  group_by(game_pk, atBatIndex) %>%
  mutate(Actual_call = case_when(
    details.description == "Ball" & Is_Overturned == 1 ~ "Called Strike",
    details.description == "Called Strike" & Is_Overturned == 1 ~ "Ball",
    TRUE ~ details.description
  )) %>%
  mutate(
    balls = lag(cumsum(
      details.description == "Ball" |
        details.description == "Ball In Dirt" |
        details.description == "Pitchout" | 
        details.description == "Intent Ball"
    ), default = 0)
  ) %>%
  mutate(
    strikes = lag(pmin(2, cumsum(
      details.description == "Called Strike" |
        details.description == "Swinging Strike" |
        details.description == "Foul" |
        details.description == "Foul Tip" |
        details.description == "Swinging Strike (Blocked)" |
        details.description == "Foul Bunt" |
        details.description == "Missed Bunt" |
        details.description == "Foul Pitchout"
    )), default = 0)
  ) %>%
  ungroup() %>%
  mutate(count = paste(balls, strikes, sep = "-")) %>%
  mutate(Correct_Call = ifelse(pitchData.coordinates.pZ < (pitchData.strikeZoneTop + (1.45/12)) & 
                               pitchData.coordinates.pZ > (pitchData.strikeZoneBottom - (1.45/12)) & 
                               pitchData.coordinates.pX > -(((1.45*2 + 17) / 12) / 2) & 
                               pitchData.coordinates.pX < (((1.45*2 + 17) / 12) / 2), "Strike",
                        ifelse(pitchData.coordinates.pZ > (pitchData.strikeZoneTop + (1.45/12)) | 
                               pitchData.coordinates.pZ < (pitchData.strikeZoneBottom - (1.45/12)) | 
                               pitchData.coordinates.pX > -(((1.45*2 + 17) / 12) / 2) | 
                               pitchData.coordinates.pX < (((1.45*2 + 17) / 12) / 2), "Ball", "NA"))) %>%
  mutate(Is.Buffer.zone = ifelse(pitchData.coordinates.pZ < (pitchData.strikeZoneTop + 1.5*(1.45/12)) & pitchData.coordinates.pX > -((17 / 12) / 2) - 1.5*(1.45/12) & pitchData.coordinates.pX < ((17 / 12) / 2) + 1.5*(1.45/12) &
                                 pitchData.coordinates.pZ > (pitchData.strikeZoneTop + 0.5*(1.45/12)) & pitchData.coordinates.pX > -((17 / 12) / 2) - 1.5*(1.45/12) & pitchData.coordinates.pX < ((17 / 12) / 2) + 1.5*(1.45/12) |
                                 pitchData.coordinates.pZ > (pitchData.strikeZoneBottom - 1.5*(1.45/12)) & pitchData.coordinates.pX > -((17 / 12) / 2) - 1.5*(1.45/12) & pitchData.coordinates.pX < ((17 / 12) / 2) + 1.5*(1.45/12) &
                                 pitchData.coordinates.pZ < (pitchData.strikeZoneBottom - 0.5*(1.45/12)) & pitchData.coordinates.pX > -((17 / 12) / 2) - 1.5*(1.45/12) & pitchData.coordinates.pX < ((17 / 12) / 2) + 1.5*(1.45/12) |
                                 pitchData.coordinates.pX < -((17 / 12) / 2) - 0.5*(1.45/12) & pitchData.coordinates.pZ < (pitchData.strikeZoneTop + 1.5*(1.45/12)) & pitchData.coordinates.pZ > (pitchData.strikeZoneBottom - 1.5*(1.45/12)) &
                                 pitchData.coordinates.pX > -((17 / 12) / 2) - 1.5*(1.45/12) & pitchData.coordinates.pZ < (pitchData.strikeZoneTop + 1.5*(1.45/12)) & pitchData.coordinates.pZ > (pitchData.strikeZoneBottom - 1.5*(1.45/12))  |
                                 pitchData.coordinates.pX > ((17 / 12) / 2) + 0.5*(1.45/12) & pitchData.coordinates.pZ < (pitchData.strikeZoneTop + 1.5*(1.45/12)) & pitchData.coordinates.pZ > (pitchData.strikeZoneBottom - 1.5*(1.45/12))  & 
                                 pitchData.coordinates.pX < ((17 / 12) / 2) + 1.5*(1.45/12) & pitchData.coordinates.pZ < (pitchData.strikeZoneTop + 1.5*(1.45/12)) & pitchData.coordinates.pZ > (pitchData.strikeZoneBottom - 1.5*(1.45/12)) , 1, 0)
  ) %>%
  mutate(Is.Incorrect = ifelse(Correct_Call == "Strike" & Actual_call == "Ball" & Is.Buffer.zone == 0 |
                               Correct_Call == "Ball" & Actual_call == "Called Strike" & Is.Buffer.zone == 0, 1, 0
  )) %>%
  mutate(Incorrect_Ball = ifelse(
    Is.Incorrect == 1 & Actual_call == "Ball", 1,
    ifelse(Is.Incorrect == 1, 0,
           NA
           ))) %>%
  mutate(Incorrect_Strike = ifelse(
    Is.Incorrect == 1 & Actual_call == "Called Strike", 1,
    ifelse(Is.Incorrect == 1, 0,
           NA
    ))) %>%
  mutate(Weekday = wday(pbp_2023$game_date, week_start = "Monday")) %>%
  mutate(Is_Using_ABS = ifelse(home_league_name == "Pacific Coast League" & seriesGameNumber == 1 & gamesInSeries == 6|
                               home_league_name == "Pacific Coast League" & seriesGameNumber == 2 & gamesInSeries == 6|
                               home_league_name == "Pacific Coast League" & seriesGameNumber == 3 & gamesInSeries == 6|
                               home_league_name == "International League" & game_date >= "2023-04-25" & seriesGameNumber == 1 & gamesInSeries == 6|
                               home_league_name == "International League" & game_date >= "2023-04-25" & seriesGameNumber == 2 & gamesInSeries == 6|
                               home_league_name == "International League" & game_date >= "2023-04-25" & seriesGameNumber == 3 & gamesInSeries == 6, 1, 0
  )) %>%
  mutate(Is_Using_Challenge = ifelse(home_league_name == "Pacific Coast League" & seriesGameNumber == 4 & gamesInSeries == 6|
                                     home_league_name == "Pacific Coast League" & seriesGameNumber == 5 & gamesInSeries == 6|
                                     home_league_name == "Pacific Coast League" & seriesGameNumber == 6 & gamesInSeries == 6|
                                     home_league_name == "International League" & game_date >= "2023-04-25" & seriesGameNumber == 4 & gamesInSeries == 6|
                                     home_league_name == "International League" & game_date >= "2023-04-25" & seriesGameNumber == 5 & gamesInSeries == 6|
                                     home_league_name == "International League" & game_date >= "2023-04-25" & seriesGameNumber == 6 & gamesInSeries == 6|
                                     Number_Of_Challenges > 0, 1, 0
  )) %>%
  mutate(adjusted_strike_zone = ifelse(game_date >= "2023-09-05", 1, 0
  )) %>%
  mutate(Is_control_group = ifelse(home_league_name == "International League" & game_date < "2023-04-25", 1, 0)) %>%
  select(-1) %>%
  filter(!is.na(pitchData.coordinates.pZ))

sum(pbp_2023$Is.Buffer.zone) #62,406 pitches (roughly 10%)
sum(pbp_2023$Is.Incorrect) #11290 pitches (roughly 50% less)

##Strike Zone Height changed 9/5, with the bottom of the zone now being "5½ inches above the midpoint of the
##measurements of a player's left and right hips. MLB projects that top to be one baseball above the bottom of a batter's belt." ~ESPN

excluded_data_points <- pbp_2023 %>%
  filter(Is_Using_ABS == 0 &
         Is_Using_Challenge == 0 &
         home_league_name == "International League" & game_date < "2023-04-25" |
         Is_Using_ABS == 0 &
         Is_Using_Challenge == 0 &
         home_league_name == "Pacific Coast League") #69,154 Excluded Data Points out of 690,905 ~9% of data points

control_group <- pbp_2023 %>%
  filter(Is_Using_ABS == 0 &
           Is_Using_Challenge == 0 &
           home_league_name == "International League" & game_date < "2023-04-25")

treatment_one <- pbp_2023 %>%
  filter(Is_Using_ABS == 0 &
           Is_Using_Challenge == 1 &
           adjusted_strike_zone == 0)

treatment_two <- pbp_2023 %>%
  filter(Is_Using_ABS == 1 &
           Is_Using_Challenge == 0 &
           adjusted_strike_zone == 0)

treatment_four <- pbp_2023 %>%
  filter(Is_Using_ABS == 1 &
           Is_Using_Challenge == 0 &
           adjusted_strike_zone == 1)

treatment_three <- pbp_2023 %>%
  filter(Is_Using_ABS == 0 &
           Is_Using_Challenge == 1 &
           adjusted_strike_zone == 1)
##Summary Statistics

Summary_Statistics <- pbp_2023 %>%
  filter(Actual_call == "Ball" | Actual_call == "Called Strike") %>%
  group_by(count, Is_Using_ABS, Is_Using_Challenge, adjusted_strike_zone, Is_control_group) %>%
  summarise(
    Correct_Call_Percentage = 1 - mean(Is.Incorrect, na.rm = TRUE),
    Incorrect_Call_Percentage = mean(Is.Incorrect, na.rm = TRUE)
  )
Summary_Count <- pbp_2023 %>%
  filter(Actual_call == "Ball" | Actual_call == "Called Strike") %>%
  group_by(count, Is_Using_ABS, Is_Using_Challenge, adjusted_strike_zone, Is_control_group) %>%
  count(name = "quantity_pitches")
Summary_Statistics <- Summary_Statistics %>%
  left_join(Summary_Count, by=c("count" = "count", "Is_Using_ABS" = "Is_Using_ABS", "Is_Using_Challenge" = "Is_Using_Challenge", "adjusted_strike_zone" = "adjusted_strike_zone", "Is_control_group" = "Is_control_group")) %>%
  filter(!(Is_Using_ABS == 0 & Is_Using_Challenge == 0 & adjusted_strike_zone == 0 & Is_control_group == 0))

Summary_Count_mc <- pbp_2023 %>%
  filter(Actual_call == "Ball" | Actual_call == "Called Strike") %>%
  group_by(count, Is_Using_ABS, Is_Using_Challenge, adjusted_strike_zone, Is_control_group) %>%
  summarise(
    Incorrect_Ball_Percentage = mean(Incorrect_Ball, na.rm = TRUE),
    Incorrect_Strike_Percentage = mean(Incorrect_Strike, na.rm = TRUE)
  )
Summary_Statistics_mc <- pbp_2023 %>%
  filter(Actual_call == "Ball" | Actual_call == "Called Strike") %>%
  group_by(count, Is_Using_ABS, Is_Using_Challenge, adjusted_strike_zone, Is_control_group) %>%
  count(name = "quantity_pitches")
Summary_Statistics_mc <- Summary_Statistics %>%
  left_join(Summary_Count_mc, by=c("count" = "count", "Is_Using_ABS" = "Is_Using_ABS", "Is_Using_Challenge" = "Is_Using_Challenge", "adjusted_strike_zone" = "adjusted_strike_zone", "Is_control_group" = "Is_control_group")) %>%
  filter(!(Is_Using_ABS == 0 & Is_Using_Challenge == 0 & adjusted_strike_zone == 0 & Is_control_group == 0))



##----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

##Comparison to MLB

pbp_MLB <- read_csv("pbp_MLB.csv")

cleaned_data_mlb <- pbp_MLB %>%
  filter(isPitch == TRUE) %>%
  select(1:2, 8:9, 10, 15:16, 23:25, 28:29, 31:32, 46, 48, 52:53, 64:66, 68, 70, 73, 75, 81, 87:88, 83, 95:96, 101, 104, 112:113, 124,163:167) %>%
  select(41, 1:2, 26:28, 29, 30, 24:25, 28, 22:23, 3:5, 7:10, 19:21, 11:16, 17:18, 31:35, 6, 39:40)

game_pks_MLB <- read_csv("game_pk_MLB.csv")

pk_merge_MLB <- game_pks_MLB %>%
  select(1, 9, 11, 16, 20, 21)

cleaned_data_mlb$game_pk <- as.numeric(cleaned_data_mlb$game_pk)
pk_merge_MLB$game_pk <- as.numeric(pk_merge_MLB$game_pk)

cleaned_data_mlb <- cleaned_data_mlb %>%
  left_join(pk_merge_MLB, by=c("game_pk" = "game_pk"))

pbp_2023_MLB <- cleaned_data_mlb %>%
  filter(game_date >= "2023-03-31") #730818 Observations

pbp_2023_MLB <- distinct(pbp_2023_MLB) #726866 Observations

pbp_2023_MLB <- pbp_2023_MLB %>%
  filter(!is.na(pitchData.coordinates.pZ)) %>%
  filter(game_date < "2023-04-25") %>%
  arrange(game_pk, gameNumber, seriesGameNumber, atBatIndex, pitchNumber) %>%
  group_by(game_pk, gameNumber, seriesGameNumber, atBatIndex) %>%
  mutate(
    balls = lag(cumsum(
      details.description == "Ball" |
        details.description == "Ball In Dirt" |
        details.description == "Pitchout" | 
        details.description == "Intent Ball"
    ), default = 0)
  ) %>%
  mutate(
    strikes = lag(pmin(2, cumsum(
      details.description == "Called Strike" |
        details.description == "Swinging Strike" |
        details.description == "Foul" |
        details.description == "Foul Tip" |
        details.description == "Swinging Strike (Blocked)" |
        details.description == "Foul Bunt" |
        details.description == "Missed Bunt" |
        details.description == "Foul Pitchout"
    )), default = 0)
  ) %>%
  mutate(
    balls_ac = lag(cumsum(
      Actual_Call == "Ball" |
        Actual_Call == "Ball In Dirt" |
        Actual_Call == "Pitchout" | 
        Actual_Call == "Intent Ball"
    ), default = 0)
  ) %>%
  mutate(
    strikes_ac = lag(pmin(2, cumsum(
      Actual_Call == "Called Strike" |
        Actual_Call == "Swinging Strike" |
        Actual_Call == "Foul" |
        Actual_Call == "Foul Tip" |
        Actual_Call == "Swinging Strike (Blocked)" |
        Actual_Call == "Foul Bunt" |
        Actual_Call == "Missed Bunt" |
        Actual_Call == "Foul Pitchout"
    )), default = 0)
  )
ungroup() %>%
  mutate(count = paste(balls, strikes, sep = "-")) %>%
  mutate(count = paste(balls_ac, strikes_ac, sep = "-")) %>%
  mutate(Correct_Call = ifelse(pitchData.coordinates.pZ < (pitchData.strikeZoneTop + (1.45/12)) & 
                                 pitchData.coordinates.pZ > (pitchData.strikeZoneBottom - (1.45/12)) & 
                                 pitchData.coordinates.pX > -(((1.45*2 + 17) / 12) / 2) & 
                                 pitchData.coordinates.pX < (((1.45*2 + 17) / 12) / 2), "Strike",
                               ifelse(pitchData.coordinates.pZ > (pitchData.strikeZoneTop  + (1.45/12)) | 
                                        pitchData.coordinates.pZ < (pitchData.strikeZoneBottom - (1.45/12)) | 
                                        pitchData.coordinates.pX > -(((1.45*2 + 17) / 12) / 2) | 
                                        pitchData.coordinates.pX < (((1.45*2 + 17) / 12) / 2), "Ball", "NA"))) %>%
  mutate(Is.Incorrect = ifelse(Correct_Call == "Strike" & Actual_call == "Ball" |
                                 Correct_Call == "Ball" & Actual_call == "Called Strike", 1, 0
  )) %>%
  mutate(Weekday = wday(pbp_2023_MLB$game_date, week_start = "Monday")) %>%
  select(-1)

##Summary Statistics

Summary_Statistics_MLB <- pbp_2023_MLB %>%
  filter(Actual_call == "Ball" | Actual_call == "Called Strike") %>%
  group_by(count) %>%
  summarise(
    Correct_Call_Percentage_MLB = 1 - mean(Is.Incorrect, na.rm = TRUE),
    SD_MLB = sd(Is.Incorrect, na.rm = TRUE),
    Var_MLB = var(Is.Incorrect, na.rm = TRUE),
    Incorrect_Call_Percentage_MLB = mean(Is.Incorrect, na.rm = TRUE)
  )
Summary_Count_MLB <- pbp_2023_MLB %>%
  filter(Actual_call == "Ball" | Actual_call == "Called Strike") %>%
  group_by(count) %>%
  count(name = "quantity_pitches_MLB")
Summary_Statistics_MLB <- Summary_Statistics_MLB %>%
  left_join(Summary_Count_MLB, by=c("count" = "count"))

##Summary Statistics for ICL pre 4/26

Summary_Statistics_ICL <- pbp_2023 %>%
  filter(Actual_call == "Ball" & home_league_name == "International League" & game_date < "2023-04-25"| 
           Actual_call == "Called Strike" & home_league_name == "International League" & game_date < "2023-04-25") %>% 
  group_by(count) %>%
  summarise(
    Correct_Call_Percentage_ICL = 1 - mean(Is.Incorrect, na.rm = TRUE),
    SD_ICL = sd(Is.Incorrect, na.rm = TRUE),
    Var_ICL = var(Is.Incorrect, na.rm = TRUE),
    Incorrect_Call_Percentage_ICL = mean(Is.Incorrect, na.rm = TRUE)
  )
Summary_Count_ICL <- pbp_2023 %>%
  filter(Actual_call == "Ball" & home_league_name == "International League" & game_date < "2023-04-25"| 
           Actual_call == "Called Strike" & home_league_name == "International League" & game_date < "2023-04-25") %>% 
  group_by(count) %>%
  count(name = "quantity_pitches_ICL")
Summary_Statistics_ICL <- Summary_Statistics_ICL %>%
  left_join(Summary_Count_ICL, by=c("count" = "count"))


##Create dataframe with both
AAA_MLB_Comparison <- Summary_Statistics_MLB %>%
  left_join(Summary_Statistics_ICL, by=c("count" = "count")) %>%
  mutate(z_score = (Correct_Call_Percentage_MLB - Correct_Call_Percentage_ICL)/sqrt((Correct_Call_Percentage_ICL * Incorrect_Call_Percentage_ICL)/quantity_pitches_ICL),
         p_value = formatC(1-pnorm(z_score), digits = 5, format = "f")
  )

##----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


##----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

##Reengineer balls and strikes and create visualizations w percentages
##Look at Double-A umpires in comparison to AAA as a control group. Look at call success rate in non-challenge system games (ICL < 4/26) compared to AA or MLB Umpires
##If neccesary (we dont get what we want), can look and see if umpires improve over time 
##Look back at data lost because of non 6 game series

##----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

##10/31
##Look at the same plot for similar pitches. Close pitches (1 ball out, 2 ball out, etc.)'
##When challenges are called. Look through pbp dataset
##1 plot for each of the 4 lines. Different line for each. X axis being where you are in the season. Week or month span.

##Creating Shadow Zones

sz_width <- (((1.45*2 + 17)/ 12)/ 2)

pbp_2023_sz <- pbp_2023 %>%
  mutate(one_ball = ifelse(
    pitchData.coordinates.pX < -sz_width + (1*(1.45/12)) & pitchData.coordinates.pX > -sz_width - (1*(1.45/12)) & pitchData.coordinates.pZ < (pitchData.strikeZoneTop + (1.45/12)) & pitchData.coordinates.pZ > (pitchData.strikeZoneBottom - (1.45/12)) |
      pitchData.coordinates.pX < sz_width + (1*(1.45/12)) & pitchData.coordinates.pX > sz_width - (1*(1.45/12)) & pitchData.coordinates.pZ < (pitchData.strikeZoneTop + (1.45/12)) & pitchData.coordinates.pZ > (pitchData.strikeZoneBottom - (1.45/12)) |
      pitchData.coordinates.pX < sz_width + (1.45/12) & pitchData.coordinates.pX > -sz_width - (1.45/12) & pitchData.coordinates.pZ < pitchData.strikeZoneTop + (1*(1.45/12)) & pitchData.coordinates.pZ > pitchData.strikeZoneTop - (1*(1.45/12)) |
      pitchData.coordinates.pX < sz_width + (1.45/12) & pitchData.coordinates.pX > -sz_width - (1.45/12) & pitchData.coordinates.pZ < pitchData.strikeZoneBottom + (1*(1.45/12)) & pitchData.coordinates.pZ > pitchData.strikeZoneBottom - (1*(1.45/12)), 1, 0
  ),
  two_balls = ifelse(
    pitchData.coordinates.pX < -sz_width + (2*(1.45/12)) & pitchData.coordinates.pX > -sz_width - (2*(1.45/12)) & pitchData.coordinates.pZ < (pitchData.strikeZoneTop + (1.45/12)) & pitchData.coordinates.pZ > (pitchData.strikeZoneBottom - (1.45/12)) |
      pitchData.coordinates.pX < sz_width + (2*(1.45/12)) & pitchData.coordinates.pX > sz_width - (2*(1.45/12)) & pitchData.coordinates.pZ < (pitchData.strikeZoneTop + (1.45/12)) & pitchData.coordinates.pZ > (pitchData.strikeZoneBottom - (1.45/12)) |
      pitchData.coordinates.pX < sz_width + (1.45/12) & pitchData.coordinates.pX > -sz_width - (1.45/12) & pitchData.coordinates.pZ < pitchData.strikeZoneTop + (2*(1.45/12)) & pitchData.coordinates.pZ > pitchData.strikeZoneTop - (2*(1.45/12)) |
      pitchData.coordinates.pX < sz_width + (1.45/12) & pitchData.coordinates.pX > -sz_width - (1.45/12) & pitchData.coordinates.pZ < pitchData.strikeZoneBottom + (2*(1.45/12)) & pitchData.coordinates.pZ > pitchData.strikeZoneBottom - (2*(1.45/12)), 1, 0
  ),
  three_balls = ifelse(
    pitchData.coordinates.pX < -sz_width + (3*(1.45/12)) & pitchData.coordinates.pX > -sz_width - (3*(1.45/12)) & pitchData.coordinates.pZ < (pitchData.strikeZoneTop + (1.45/12)) & pitchData.coordinates.pZ > (pitchData.strikeZoneBottom - (1.45/12)) |
      pitchData.coordinates.pX < sz_width + (3*(1.45/12)) & pitchData.coordinates.pX > sz_width - (3*(1.45/12)) & pitchData.coordinates.pZ < (pitchData.strikeZoneTop + (1.45/12)) & pitchData.coordinates.pZ > (pitchData.strikeZoneBottom - (1.45/12)) |
      pitchData.coordinates.pX < sz_width + (1.45/12) & pitchData.coordinates.pX > -sz_width - (1.45/12) & pitchData.coordinates.pZ < pitchData.strikeZoneTop + (3*(1.45/12)) & pitchData.coordinates.pZ > pitchData.strikeZoneTop - (3*(1.45/12)) |
      pitchData.coordinates.pX < sz_width + (1.45/12) & pitchData.coordinates.pX > -sz_width - (1.45/12) & pitchData.coordinates.pZ < pitchData.strikeZoneBottom + (3*(1.45/12)) & pitchData.coordinates.pZ > pitchData.strikeZoneBottom - (3*(1.45/12)), 1, 0
  ),
  four_balls = ifelse(
    pitchData.coordinates.pX < -sz_width + (4*(1.45/12)) & pitchData.coordinates.pX > -sz_width - (4*(1.45/12)) & pitchData.coordinates.pZ < (pitchData.strikeZoneTop + (1.45/12)) & pitchData.coordinates.pZ > (pitchData.strikeZoneBottom - (1.45/12)) |
      pitchData.coordinates.pX < sz_width + (4*(1.45/12)) & pitchData.coordinates.pX > sz_width - (4*(1.45/12)) & pitchData.coordinates.pZ < (pitchData.strikeZoneTop + (1.45/12)) & pitchData.coordinates.pZ > (pitchData.strikeZoneBottom - (1.45/12)) |
      pitchData.coordinates.pX < sz_width + (1.45/12) & pitchData.coordinates.pX > -sz_width - (1.45/12) & pitchData.coordinates.pZ < pitchData.strikeZoneTop + (4*(1.45/12)) & pitchData.coordinates.pZ > pitchData.strikeZoneTop - (4*(1.45/12)) |
      pitchData.coordinates.pX < sz_width + (1.45/12) & pitchData.coordinates.pX > -sz_width - (1.45/12) & pitchData.coordinates.pZ < pitchData.strikeZoneBottom + (4*(1.45/12)) & pitchData.coordinates.pZ > pitchData.strikeZoneBottom - (4*(1.45/12)), 1, 0
  )
  )

pbp_2023_sz <- pbp_2023_sz %>%
  mutate(h_half_ball = ifelse(
    pitchData.coordinates.pX < -sz_width + (1*(1.45/12)) & pitchData.coordinates.pX > -sz_width - (1*(1.45/12)) & pitchData.coordinates.pZ < (pitchData.strikeZoneTop + (1.45/12)) & pitchData.coordinates.pZ > (pitchData.strikeZoneBottom - (1.45/12)) |
    pitchData.coordinates.pX < sz_width + (1*(1.45/12)) & pitchData.coordinates.pX > sz_width - (1*(1.45/12)) & pitchData.coordinates.pZ < (pitchData.strikeZoneTop + (1.45/12)) & pitchData.coordinates.pZ > (pitchData.strikeZoneBottom - (1.45/12)), 1, 0
  ),
  v_half_ball = ifelse(
    pitchData.coordinates.pX < sz_width + (1.45/12) & pitchData.coordinates.pX > -sz_width - (1.45/12) & pitchData.coordinates.pZ < (pitchData.strikeZoneTop + 1.45/12) + (1*(1.45/12)) & pitchData.coordinates.pZ > (pitchData.strikeZoneTop + 1.45/12) - (1*(1.45/12)) |
    pitchData.coordinates.pX < sz_width + (1.45/12) & pitchData.coordinates.pX > -sz_width - (1.45/12) & pitchData.coordinates.pZ < (pitchData.strikeZoneBottom - 1.45/12) + (1*(1.45/12)) & pitchData.coordinates.pZ > (pitchData.strikeZoneBottom - 1.45/12) - (1*(1.45/12)), 1, 0
  ),
  h_one_ball = ifelse(
    pitchData.coordinates.pX < -sz_width + (2*(1.45/12)) & pitchData.coordinates.pX > -sz_width - (2*(1.45/12)) & pitchData.coordinates.pZ < (pitchData.strikeZoneTop + (1.45/12)) & pitchData.coordinates.pZ > (pitchData.strikeZoneBottom - (1.45/12)) |
      pitchData.coordinates.pX < sz_width + (2*(1.45/12)) & pitchData.coordinates.pX > sz_width - (2*(1.45/12)) & pitchData.coordinates.pZ < (pitchData.strikeZoneTop + (1.45/12)) & pitchData.coordinates.pZ > (pitchData.strikeZoneBottom - (1.45/12)), 1, 0
  ),
  v_one_ball = ifelse(
    pitchData.coordinates.pX < sz_width + (1.45/12) & pitchData.coordinates.pX > -sz_width - (1.45/12) & pitchData.coordinates.pZ < (pitchData.strikeZoneTop + 1.45/12) + (2*(1.45/12)) & pitchData.coordinates.pZ > (pitchData.strikeZoneTop + 1.45/12) - (2*(1.45/12)) |
    pitchData.coordinates.pX < sz_width + (1.45/12) & pitchData.coordinates.pX > -sz_width - (1.45/12) & pitchData.coordinates.pZ < (pitchData.strikeZoneBottom - 1.45/12) + (2*(1.45/12)) & pitchData.coordinates.pZ > (pitchData.strikeZoneBottom - 1.45/12) - (2*(1.45/12)), 1, 0
  ),
  h_onehalf_ball = ifelse(
    pitchData.coordinates.pX < -sz_width + (3*(1.45/12)) & pitchData.coordinates.pX > -sz_width - (3*(1.45/12)) & pitchData.coordinates.pZ < (pitchData.strikeZoneTop + (1.45/12)) & pitchData.coordinates.pZ > (pitchData.strikeZoneBottom - (1.45/12)) |
      pitchData.coordinates.pX < sz_width + (3*(1.45/12)) & pitchData.coordinates.pX > sz_width - (3*(1.45/12)) & pitchData.coordinates.pZ < (pitchData.strikeZoneTop + (1.45/12)) & pitchData.coordinates.pZ > (pitchData.strikeZoneBottom - (1.45/12)), 1, 0
  ),
  v_onehalf_ball = ifelse(
    pitchData.coordinates.pX < sz_width + (1.45/12) & pitchData.coordinates.pX > -sz_width - (1.45/12) & pitchData.coordinates.pZ < (pitchData.strikeZoneTop + 1.45/12) + (3*(1.45/12)) & pitchData.coordinates.pZ > (pitchData.strikeZoneTop + 1.45/12) - (3*(1.45/12)) |
    pitchData.coordinates.pX < sz_width + (1.45/12) & pitchData.coordinates.pX > -sz_width - (1.45/12) & pitchData.coordinates.pZ < (pitchData.strikeZoneBottom - 1.45/12) + (3*(1.45/12)) & pitchData.coordinates.pZ > (pitchData.strikeZoneBottom - 1.45/12) - (3*(1.45/12)), 1, 0
  ),
  h_two_ball = ifelse(
    pitchData.coordinates.pX < -sz_width + (2*(1.45/12)) & pitchData.coordinates.pX > -sz_width - (4*(1.45/12)) & pitchData.coordinates.pZ < (pitchData.strikeZoneTop + (1.45/12)) & pitchData.coordinates.pZ > (pitchData.strikeZoneBottom - (1.45/12)) |
      pitchData.coordinates.pX < sz_width + (2*(1.45/12)) & pitchData.coordinates.pX > sz_width - (4*(1.45/12)) & pitchData.coordinates.pZ < (pitchData.strikeZoneTop + (1.45/12)) & pitchData.coordinates.pZ > (pitchData.strikeZoneBottom - (1.45/12)), 1, 0
  ),
  v_two_ball = ifelse(
    pitchData.coordinates.pX < sz_width + (1.45/12) & pitchData.coordinates.pX > -sz_width - (1.45/12) & pitchData.coordinates.pZ < (pitchData.strikeZoneTop + 1.45/12) + (4*(1.45/12)) & pitchData.coordinates.pZ > (pitchData.strikeZoneTop + 1.45/12) - (4*(1.45/12)) |
    pitchData.coordinates.pX < sz_width + (1.45/12) & pitchData.coordinates.pX > -sz_width - (1.45/12) & pitchData.coordinates.pZ < (pitchData.strikeZoneBottom - 1.45/12) + (4*(1.45/12)) & pitchData.coordinates.pZ > (pitchData.strikeZoneBottom - 1.45/12) - (4*(1.45/12)), 1, 0
  )
)

Summary_Statistics_sz <- pbp_2023_sz %>%
  filter(Actual_call == "Ball" | Actual_call == "Called Strike") %>%
  group_by(count, Is_Using_ABS, Is_Using_Challenge, adjusted_strike_zone, Is_control_group, v_half_ball, h_half_ball, v_one_ball, h_one_ball, v_onehalf_ball, h_onehalf_ball, v_two_ball, h_two_ball) %>%
  summarise(
    Correct_Call_Percentage = 1 - mean(Is.Incorrect, na.rm = TRUE),
    Incorrect_Call_Percentage = mean(Is.Incorrect, na.rm = TRUE)
  )
Summary_Count_sz <- pbp_2023_sz %>%
  filter(Actual_call == "Ball" | Actual_call == "Called Strike") %>%
  group_by(count, Is_Using_ABS, Is_Using_Challenge, adjusted_strike_zone, Is_control_group, v_half_ball, h_half_ball, v_one_ball, h_one_ball, v_onehalf_ball, h_onehalf_ball, v_two_ball, h_two_ball) %>%
  count(name = "quantity_pitches")
Summary_Statistics_sz <- Summary_Statistics_sz %>%
  left_join(Summary_Count_sz, by=c("count" = "count", "Is_Using_ABS" = "Is_Using_ABS", "Is_Using_Challenge" = "Is_Using_Challenge", "adjusted_strike_zone" = "adjusted_strike_zone", "Is_control_group" = "Is_control_group",
                                   "v_half_ball" = "v_half_ball", "h_half_ball" = "h_half_ball", "v_one_ball" = "v_one_ball", "h_one_ball" = "h_one_ball",
                                   "v_onehalf_ball" = "v_onehalf_ball", "h_onehalf_ball" = "h_onehalf_ball", "v_two_ball" = "v_two_ball", "h_two_ball" = "h_two_ball")) %>%
  filter(!(Is_Using_ABS == 0 & Is_Using_Challenge == 0 & Is_control_group == 0))

Summary_Statistics_h <- pbp_2023_sz %>%
  filter(Actual_call == "Ball" | Actual_call == "Called Strike") %>%
  group_by(count, Is_Using_ABS, Is_Using_Challenge, adjusted_strike_zone, Is_control_group, h_half_ball, h_one_ball, h_onehalf_ball, h_two_ball) %>%
  summarise(
    Correct_Call_Percentage = 1 - mean(Is.Incorrect, na.rm = TRUE),
    Incorrect_Call_Percentage = mean(Is.Incorrect, na.rm = TRUE)
  )
Summary_Count_h <- pbp_2023_sz %>%
  filter(Actual_call == "Ball" | Actual_call == "Called Strike") %>%
  group_by(count, Is_Using_ABS, Is_Using_Challenge, adjusted_strike_zone, Is_control_group, h_half_ball, h_one_ball, h_onehalf_ball, h_two_ball) %>%
  count(name = "quantity_pitches")
Summary_Statistics_h <- Summary_Statistics_h %>%
  left_join(Summary_Count_h, by=c("count" = "count", "Is_Using_ABS" = "Is_Using_ABS", "Is_Using_Challenge" = "Is_Using_Challenge", "adjusted_strike_zone" = "adjusted_strike_zone", "Is_control_group" = "Is_control_group",
                                   "h_half_ball" = "h_half_ball", "h_one_ball" = "h_one_ball", "h_onehalf_ball" = "h_onehalf_ball", "h_two_ball" = "h_two_ball")) %>%
  filter(!(Is_Using_ABS == 0 & Is_Using_Challenge == 0 & Is_control_group == 0)) %>%
  distinct()

Summary_Statistics_v <- pbp_2023_sz %>%
  filter(Actual_call == "Ball" | Actual_call == "Called Strike") %>%
  group_by(count, Is_Using_ABS, Is_Using_Challenge, adjusted_strike_zone, Is_control_group, v_half_ball, v_one_ball, v_onehalf_ball, v_two_ball) %>%
  summarise(
    Correct_Call_Percentage = 1 - mean(Is.Incorrect, na.rm = TRUE),
    Incorrect_Call_Percentage = mean(Is.Incorrect, na.rm = TRUE)
  )
Summary_Count_v <- pbp_2023_sz %>%
  filter(Actual_call == "Ball" | Actual_call == "Called Strike") %>%
  group_by(count, Is_Using_ABS, Is_Using_Challenge, adjusted_strike_zone, Is_control_group, v_half_ball, v_one_ball, v_onehalf_ball, v_two_ball) %>%
  count(name = "quantity_pitches")
Summary_Statistics_v <- Summary_Statistics_v %>%
  left_join(Summary_Count_v, by=c("count" = "count", "Is_Using_ABS" = "Is_Using_ABS", "Is_Using_Challenge" = "Is_Using_Challenge", "adjusted_strike_zone" = "adjusted_strike_zone", "Is_control_group" = "Is_control_group",
                                  "v_half_ball" = "v_half_ball", "v_one_ball" = "v_one_ball", "v_onehalf_ball" = "v_onehalf_ball", "v_two_ball" = "v_two_ball")) %>%
  filter(!(Is_Using_ABS == 0 & Is_Using_Challenge == 0 & Is_control_group == 0)) %>%
  distinct()

Summary_Statistics_sz_mc <- pbp_2023_sz %>%
  filter(Actual_call == "Ball" | Actual_call == "Called Strike") %>%
  group_by(Is_Using_ABS, Is_Using_Challenge, adjusted_strike_zone, Is_control_group, v_half_ball, h_half_ball, v_one_ball, h_one_ball, v_onehalf_ball, h_onehalf_ball, v_two_ball, h_two_ball) %>%
  summarise(
    Incorrect_Ball_Percentage = mean(Incorrect_Ball, na.rm = TRUE),
    Incorrect_Strike_Percentage = mean(Incorrect_Strike, na.rm = TRUE)
  )
Summary_Count_sz_mc <- pbp_2023_sz %>%
  filter(Actual_call == "Ball" | Actual_call == "Called Strike") %>%
  group_by(Is_Using_ABS, Is_Using_Challenge, adjusted_strike_zone, Is_control_group, v_half_ball, h_half_ball, v_one_ball, h_one_ball, v_onehalf_ball, h_onehalf_ball, v_two_ball, h_two_ball) %>%
  count(name = "quantity_pitches")

Summary_Statistics_sz_mc <- Summary_Statistics_sz_mc %>%
  left_join(Summary_Count_sz_mc, by=c("Is_Using_ABS" = "Is_Using_ABS", "Is_Using_Challenge" = "Is_Using_Challenge", "adjusted_strike_zone" = "adjusted_strike_zone", "Is_control_group" = "Is_control_group",
                                      "v_half_ball" = "v_half_ball", "h_half_ball" = "h_half_ball", "v_one_ball" = "v_one_ball", "h_one_ball" = "h_one_ball",
                                      "v_onehalf_ball" = "v_onehalf_ball", "h_onehalf_ball" = "h_onehalf_ball", "v_two_ball" = "v_two_ball", "h_two_ball" = "h_two_ball")) %>%
  filter(!(Is_Using_ABS == 0 & Is_Using_Challenge == 0 & Is_control_group == 0)) %>%
  distinct()

##-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

##Evaluating Accuracy over a season

pbp_2023_months <- pbp_2023 %>%
  mutate(month = month(game_date, label = TRUE))

Summary_Statistics_months <- pbp_2023_months %>%
  filter(Actual_call == "Ball" | Actual_call == "Called Strike") %>%
  group_by(month, Is_Using_ABS, Is_Using_Challenge, adjusted_strike_zone, Is_control_group) %>%
  summarise(
    Correct_Call_Percentage = 1 - mean(Is.Incorrect, na.rm = TRUE),
    Incorrect_Call_Percentage = mean(Is.Incorrect, na.rm = TRUE)
  )
Summary_Count_months <- pbp_2023_months %>%
  filter(Actual_call == "Ball" | Actual_call == "Called Strike") %>%
  group_by(month, Is_Using_ABS, Is_Using_Challenge, adjusted_strike_zone, Is_control_group) %>%
  count(name = "quantity_pitches")
Summary_Statistics_months <- Summary_Statistics_months %>%
  left_join(Summary_Count_months, by=c("month" = "month", "Is_Using_ABS" = "Is_Using_ABS", "Is_Using_Challenge" = "Is_Using_Challenge", "adjusted_strike_zone" = "adjusted_strike_zone", "Is_control_group" = "Is_control_group")) %>%
  filter(!(Is_Using_ABS == 0 & Is_Using_Challenge == 0 & adjusted_strike_zone == 0 & Is_control_group == 0))

##----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

##Looking at challenges

challenges <- pbp_2023 %>%
  group_by(challenge_dummy, Challenge_Team) %>%
  summarise(
    Number_Of_Challenges = n(),
    Overturn_PCT = mean(Is_Overturned)
  ) %>%
  filter(challenge_dummy == 1)

challenges_count <- pbp_2023 %>%
  group_by(challenge_dummy, count) %>%
  summarise(
    Number_Of_Challenges = n()
  ) %>%
  filter(challenge_dummy == 1)

## Add ball/strike and ball/strike overturn percentage

challenges_bs <- pbp_2023 %>%
  filter(Actual_call == "Ball" |
         Actual_call == "Called Strike") %>%
  group_by(challenge_dummy, count, Actual_call) %>%
  summarise(
    Number_Of_Pitches = n(),
    Overturn_Accuracy = mean(Is_Overturned)
  ) %>%
  filter(challenge_dummy == 1)

number_pitches <- pbp_2023 %>%
  filter(Is_Using_Challenge == 1) %>%
  group_by(count) %>%
  summarise(
    Number_Of_Pitches = n()
  )

challenges_count <- challenges_count %>%
  left_join(number_pitches, by=c("count" = "count")) %>%
  mutate(
    challenge_pct = (Number_Of_Challenges / Number_Of_Pitches) * 100
  )
##----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

##11/14
##Clean up Challenges and Double Headers with same PK?. Look into gameseriesnumber
##Look at how challenges are recorded in the data. Is it overwritten?
##Want the data represent what is called by the umpire!!!
##Break down the 4 plot and look at percentages missed stikes and missed balls
##fangraphs baseball perspectus baseball america for AAA Umpire tendencies. Need a unique argument if anyone has done this.

pbp_2023_final <- pbp_2023_sz %>%
  mutate(
    Is_Strike = ifelse(Actual_call == "Called Strike", 1, 0),
    Is_Ball = ifelse(Actual_call == "Ball", 1, 0),
    matchup.pitchHand.code = ifelse(matchup.pitchHand.code == "L", 1, 0),
    matchup.batSide.code = ifelse(matchup.batSide.code == "L", 1, 0),
    break_is_armside = ifelse(pitchData.coordinates.pfxX > 0, 1, 0),
    control_group = ifelse(Is_Using_ABS == 0 &
                             Is_Using_Challenge == 0 &
                             home_league_name == "International League" & game_date < "2023-04-25", 1, 0),
    treatment_one = ifelse(Is_Using_ABS == 0 &
                             Is_Using_Challenge == 1 &
                             adjusted_strike_zone == 0, 1, 0),
    treatment_two = ifelse(Is_Using_ABS == 1 &
                             Is_Using_Challenge == 0 &
                             adjusted_strike_zone == 0, 1, 0),
    treatment_three = ifelse(Is_Using_ABS == 1 &
                               Is_Using_Challenge == 0 &
                               adjusted_strike_zone == 1, 1, 0),
    treatment_four = ifelse(Is_Using_ABS == 0 &
                              Is_Using_Challenge == 1 &
                              adjusted_strike_zone == 1, 1, 0),
    `Count_3-0` = ifelse(count == "3-0", 1, 0),
    `Count_0-2` = ifelse(count == "0-2", 1, 0),
    `Count_3-Balls` = ifelse(balls == 3, 1, 0),
    `Count-2-Strikes` = ifelse(strikes == 2, 1, 0),
    dayNight = ifelse(dayNight == "day", 1, 0),
    sz_height_dif = pitchData.strikeZoneTop - pitchData.strikeZoneBottom
  )

write_csv(pbp_2023_final, "pbp_2023_final.csv")

only_calls <- pbp_2023_final %>%
  filter(details.description == "Ball" | details.description == "Called Strike")
sum(only_calls$Is.Incorrect)
sum(only_calls$Is.Buffer.zone)


## Analyzing sz top and sz bottom
graph <- pbp_2023_final %>%
  select(game_pk, pitchData.strikeZoneTop, pitchData.strikeZoneBottom, Is.Incorrect, Is.Buffer.zone, atBatIndex, Is_Using_ABS, Is_Using_Challenge) %>%
  distinct()

theme_set(theme_bw())
ggplot(graph, aes(x = pitchData.strikeZoneTop)) +
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "white") +
  geom_density(adjust = 3, lwd = 1, colour = 4,
               fill = 4, alpha = 0.25) +
  labs(x = "Top of the Strike Zone", y = "Density", title = "Density Plot of the Calculated Top of the Strike Zone") -> sz_top
sz_top

ggplot(graph, aes(x = pitchData.strikeZoneBottom)) +
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "white", bins = 25) +
  geom_density(adjust = 3, lwd = 1, colour = 4,
               fill = 4, alpha = 0.25) +
  labs(x = "Bottom of the Strike Zone", y = "Density", title = "Density Plot of the Calculated Bottom of the Strike Zone") -> sz_bot
sz_bot

graph <- graph %>%
  mutate(sz_length = pitchData.strikeZoneTop - pitchData.strikeZoneBottom)

ggplot(graph, aes(x = sz_length)) +
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "white", bins = 25) +
  geom_density(adjust = 3, lwd = 1, colour = 4,
               fill = 4, alpha = 0.25) +
  labs(x = "Strike Zone Length", y = "Density", title = "Density Plot of the Vertical Strike Zone Length") -> sz_length
sz_length

sz_effects <- glm(Is.Incorrect ~ pitchData.strikeZoneTop + pitchData.strikeZoneBottom, data = graph)
summary(sz_effects)

sz_effects_2 <- glm(Is.Incorrect ~ sz_length + Is_Using_ABS + Is_Using_Challenge, data = graph)
summary(sz_effects_2)

ggplot(graph, aes(x = sz_length, y = pitchData.strikeZoneTop)) + 
  geom_point() + 
  geom_smooth(method = "lm") -> length_top
length_top

ggplot(graph, aes(x = sz_length, y = Is.Incorrect)) + 
  geom_point() + 
  geom_smooth(method = "glm") -> length_graph
length_graph

incorrect <- pbp_2023_final %>%
  group_by(control_group, Is_Using_ABS, Is_Using_Challenge) %>%
  summarise(
    incorrect = mean(Is.Incorrect ,na.rm = TRUE),
    count = n()
  )
