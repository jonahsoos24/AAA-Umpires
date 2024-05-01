library(tidyverse)
library(ggplot2)
library(patchwork)
theme_set(theme_bw())

##----

##Plotting AAA data with and without ABS
ggplot(Summary_Statistics, aes(x = count, y = Incorrect_Call_Percentage)) +
  geom_line(data = Summary_Statistics %>%
              filter(Is_Using_ABS == 1, adjusted_strike_zone == 1),
            aes(x = count, y = Incorrect_Call_Percentage, color = "ABS - Strike Zone 1", group = "ABS - Strike Zone 1"), size = 1) +
  geom_line(data = Summary_Statistics %>%
              filter(Is_Using_ABS == 1, adjusted_strike_zone == 0),
            aes(x = count, y = Incorrect_Call_Percentage, color = "ABS - Strike Zone 0", group = "ABS - Strike Zone 0"), size = 1) +
  geom_line(data = Summary_Statistics %>%
              filter(Is_Using_Challenge == 1, adjusted_strike_zone == 1),
            aes(x = count, y = Incorrect_Call_Percentage, color = "Challenge - Strike Zone 1", group = "Challenge - Strike Zone 1"), size = 1) +
  geom_line(data = Summary_Statistics %>%
              filter(Is_Using_Challenge == 1, adjusted_strike_zone == 0),
            aes(x = count, y = Incorrect_Call_Percentage, color = "Challenge - Strike Zone 0", group = "Challenge - Strike Zone 0"), size = 1) +
  geom_line(data = Summary_Statistics %>%
              filter(Is_control_group == 1),
            aes(x = count, y = Incorrect_Call_Percentage, color = "control", group = "control"), size = 1) +
  labs(title = "Incorrect Call Percentage by Different Conditions",
       x = "Count", y = "Incorrect Call Percentage") +
  scale_color_manual(values = c("ABS - Strike Zone 1" = "red", "ABS - Strike Zone 0" = "blue",
                                "Challenge - Strike Zone 1" = "orange", "Challenge - Strike Zone 0" = "purple", 
                                "control" = "forestgreen")) +
  theme(legend.position = "right") +
  theme(strip.placement = "outside", strip.background = element_blank()) + 
  guides(color = guide_legend(override.aes = list(size = 1))) +
  theme(plot.margin = unit(c(.1, .1, .1, .1), "cm"))

##----
# Plot by Incorrect Call PCT
plot_data_1 <- Summary_Statistics_v %>%
  filter(v_half_ball == 1) %>%
  select(-7:-9)

plot_data_2 <- Summary_Statistics_v %>%
  filter(v_half_ball == 0, v_one_ball == 1 )%>%
  select(-6, -8:-9)

plot_data_3 <- Summary_Statistics_v %>%
  filter(v_half_ball == 0, v_one_ball == 0, v_onehalf_ball == 1) %>%
  select(-6:-7, -9)

plot_data_4 <- Summary_Statistics_v %>%
  filter(v_half_ball == 0, v_one_ball == 0, v_onehalf_ball == 0, v_two_ball == 1) %>%
  select(-6:-8)

plot_data_5 <- Summary_Statistics_h %>%
  filter(h_half_ball == 1) %>%
  select(-7:-9)

plot_data_6 <- Summary_Statistics_h %>%
  filter(h_half_ball == 0, h_one_ball == 1 )%>%
  select(-6, -8:-9)

plot_data_7 <- Summary_Statistics_h %>%
  filter(h_half_ball == 0, h_one_ball == 0, h_onehalf_ball == 1) %>%
  select(-6:-7, -9)

plot_data_8 <- Summary_Statistics_h %>%
  filter(h_half_ball == 0, h_one_ball == 0, h_onehalf_ball == 0, h_two_ball == 1) %>%
  select(-6:-8)

create_line_plot <- function(data, title) {
  ggplot(data, aes(x = count, y = Incorrect_Call_Percentage)) +
    geom_line(data = data %>%
                filter(Is_Using_ABS == 1, adjusted_strike_zone == 1),
              aes(x = count, y = Incorrect_Call_Percentage, color = "ABS - Strike Zone 1", group = "ABS - Strike Zone 1"), size = 1) +
    geom_line(data = data %>%
                filter(Is_Using_ABS == 1, adjusted_strike_zone == 0),
              aes(x = count, y = Incorrect_Call_Percentage, color = "ABS - Strike Zone 0", group = "ABS - Strike Zone 0"), size = 1) +
    geom_line(data = data %>%
                filter(Is_Using_Challenge == 1, adjusted_strike_zone == 1),
              aes(x = count, y = Incorrect_Call_Percentage, color = "Challenge - Strike Zone 1", group = "Challenge - Strike Zone 1"), size = 1) +
    geom_line(data = data %>%
                filter(Is_Using_Challenge == 1, adjusted_strike_zone == 0),
              aes(x = count, y = Incorrect_Call_Percentage, color = "Challenge - Strike Zone 0", group = "Challenge - Strike Zone 0"), size = 1) +
    geom_line(data = data %>%
                filter(Is_control_group == 1),
              aes(x = count, y = Incorrect_Call_Percentage, color = "control", group = "control"), size = 1) +
    labs(title = title, x = "Count", y = "Incorrect Call Percentage") +
    scale_color_manual(values = c("1" = "red", "0" = "blue",
                                  "ABS - Strike Zone 1" = "red", "ABS - Strike Zone 0" = "blue",
                                  "Challenge - Strike Zone 1" = "orange", "Challenge - Strike Zone 0" = "purple", 
                                  "control" = "forestgreen")) +
    theme(legend.position = "right") +
    theme(strip.placement = "outside", strip.background = element_blank()) + 
    guides(color = guide_legend(override.aes = list(size = 1))) +
    theme(plot.margin = unit(c(.1, .1, .1, .1), "cm"))
}

# Create plots
plot_1 <- create_line_plot(plot_data_1, "Vertically Within a Half Ball of the Edge")
plot_2 <- create_line_plot(plot_data_2, "Vertically Within One Ball of the Edge")
plot_3 <- create_line_plot(plot_data_3, "Vertically Within One and a Half Ball of the Edge")
plot_4 <- create_line_plot(plot_data_4, "Vertically Within Two Balls of the Edge")
plot_5 <- create_line_plot(plot_data_5, "Horizontally Within a Half Ball of the Edge")
plot_6 <- create_line_plot(plot_data_6, "Horizontally Within One Ball of the Edge")
plot_7 <- create_line_plot(plot_data_7, "Horizontally Within One and a Half Ball of the Edge")
plot_8 <- create_line_plot(plot_data_8, "Horizontally Within Two Balls of the Edge")

# Arrange the plots using patchwork
v_plot <- plot_1 + plot_2 + plot_3 + plot_4
h_plot <- plot_5 + plot_6 + plot_7 + plot_8

# Display the final plot
final_plot <- plot_1 + plot_2 + plot_3 + plot_4 + plot_5 + plot_6 + plot_7 + plot_8
final_plot

##----

##Plot by Incorrect Strike/Ball PCT

plot_data_5 <- Summary_Statistics_sz_mc %>%
  filter(one_ball == 1)

plot_data_6 <- Summary_Statistics_sz_mc %>%
  filter(one_ball == 0, two_balls == 1)

plot_data_7 <- Summary_Statistics_sz_mc %>%
  filter(one_ball == 0, two_balls == 0, three_balls == 1)

plot_data_8 <- Summary_Statistics_sz_mc %>%
  filter(one_ball == 0, two_balls == 0, three_balls == 0, four_balls == 1)

create_line_plot <- function(data, title) {
  ggplot(data, aes(x = count, y = Incorrect_Strike_Percentage)) +
    geom_line(data = data %>%
                filter(Is_Using_ABS == 1, adjusted_strike_zone == 1),
              aes(x = count, y = Incorrect_Strike_Percentage, color = "ABS - Strike Zone 1", group = "ABS - Strike Zone 1"), size = 1) +
    geom_line(data = data %>%
                filter(Is_Using_ABS == 1, adjusted_strike_zone == 0),
              aes(x = count, y = Incorrect_Strike_Percentage, color = "ABS - Strike Zone 0", group = "ABS - Strike Zone 0"), size = 1) +
    geom_line(data = data %>%
                filter(Is_Using_Challenge == 1, adjusted_strike_zone == 1),
              aes(x = count, y = Incorrect_Strike_Percentage, color = "Challenge - Strike Zone 1", group = "Challenge - Strike Zone 1"), size = 1) +
    geom_line(data = data %>%
                filter(Is_Using_Challenge == 1, adjusted_strike_zone == 0),
              aes(x = count, y = Incorrect_Strike_Percentage, color = "Challenge - Strike Zone 0", group = "Challenge - Strike Zone 0"), size = 1) +
    geom_line(data = data %>%
                filter(Is_control_group == 1),
              aes(x = count, y = Incorrect_Strike_Percentage, color = "control", group = "control"), size = 1) +
    labs(title = title, x = "Count", y = "Incorrect Ball Percentage") +
    scale_color_manual(values = c("1" = "red", "0" = "blue",
                                  "ABS - Strike Zone 1" = "red", "ABS - Strike Zone 0" = "blue",
                                  "Challenge - Strike Zone 1" = "orange", "Challenge - Strike Zone 0" = "purple", 
                                  "control" = "forestgreen")) +
    theme(legend.position = "right") +
    theme(strip.placement = "outside", strip.background = element_blank()) + 
    guides(color = guide_legend(override.aes = list(size = 1))) +
    theme(plot.margin = unit(c(.1, .1, .1, .1), "cm"))
}

# Create plots
plot_5 <- create_line_plot(plot_data_5, "Within One Ball of the Edge")
plot_6 <- create_line_plot(plot_data_6, "Within Two Balls of the Edge")
plot_7 <- create_line_plot(plot_data_7, "Within Three Balls of the Edge")
plot_8 <- create_line_plot(plot_data_8, "Within Four Balls of the Edge")

# Arrange the plots using patchwork
final_plot <- plot_5 + plot_6 + plot_7 + plot_8

# Display the final plot
final_plot

##------------------------------------------------------------------------------------------------------------------------------------------------

##plot by months
library(forcats)

# Define the desired order of months
desired_order <- c("Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct")

# Convert "month" to a factor with desired levels using forcats
Summary_Statistics_months$month <- factor(Summary_Statistics_months$month, levels = desired_order)

# Check the data type and levels of "month"
str(Summary_Statistics_months$month)

ggplot(Summary_Statistics_months, aes(x = month, y = Incorrect_Call_Percentage)) +
  geom_point(show.legend = TRUE) +
  geom_line(data = Summary_Statistics_months %>%
              filter(Is_Using_ABS == 1, adjusted_strike_zone == 1),
            aes(x = month, y = Incorrect_Call_Percentage, color = "ABS - Strike Zone 1", group = "ABS - Strike Zone 1"), size = 1) +
  geom_line(data = Summary_Statistics_months %>%
              filter(Is_Using_ABS == 1, adjusted_strike_zone == 0),
            aes(x = month, y = Incorrect_Call_Percentage, color = "ABS - Strike Zone 0", group = "ABS - Strike Zone 0"), size = 1) +
  geom_line(data = Summary_Statistics_months %>%
              filter(Is_Using_Challenge == 1, adjusted_strike_zone == 1),
            aes(x = month, y = Incorrect_Call_Percentage, color = "Challenge - Strike Zone 1", group = "Challenge - Strike Zone 1"), size = 1) +
  geom_line(data = Summary_Statistics_months %>%
              filter(Is_Using_Challenge == 1, adjusted_strike_zone == 0),
            aes(x = month, y = Incorrect_Call_Percentage, color = "Challenge - Strike Zone 0", group = "Challenge - Strike Zone 0"), size = 1) +
  geom_line(data = Summary_Statistics_months %>%
              filter(Is_control_group == 1),
            aes(x = month, y = Incorrect_Call_Percentage, color = "control", group = "control"), size = 1) +
  labs(title = "Incorrect Call Percentage by Months",
       x = "Month", y = "Incorrect Call Percentage") +
  scale_color_manual(values = c("ABS - Strike Zone 1" = "red", "ABS - Strike Zone 0" = "blue",
                                "Challenge - Strike Zone 1" = "orange", "Challenge - Strike Zone 0" = "purple", 
                                "control" = "forestgreen")) +
  theme(legend.position = "right") +
  theme(strip.placement = "outside", strip.background = element_blank()) + 
  guides(color = guide_legend(override.aes = list(size = 1))) +
  theme(plot.margin = unit(c(.1, .1, .1, .1), "cm"))

##----

##Test for Coords data
test <- cleaned_data %>%
  filter(home_team == "Syracuse Mets") %>%
  filter(game_pk == 722775) %>%
  filter(atBatIndex >= 40 & atBatIndex <= 45)
ggplot() +
  geom_point(data = test, aes(x = pitchData.coordinates.x , y = pitchData.coordinates.pX))

cor(test$pitchData.coordinates.x, test$pitchData.coordinates.pX)


x <- c(-.95,.95,.95,-.95,-.95)
z <- c(1.6,1.6,3.5,3.5,1.6)
sz <- data.frame(x,z)

geom_path(data = sz, aes(x=x, y=z)) +
  coord_equal()
scale_size(range = c(-1.0,2.5)) 
ggplot() +
  geom_point(data = cleaned_data, aes(x = pitchData.coordinates.x , y = pitchData.coordinates.y, shape = details.description, color = details.type.description)) +
  scale_color_discrete() +
  labs(size = "Speed",
       color = "Pitch Type",
       title = "Missed Calls from 2022",
       subtitle = "Based on PitchFX Data",
       y = "Feet Above Homeplate",
       x = "Feet From Homeplate")
