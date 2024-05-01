library(tidyverse)
library(xgboost)
library(glue)

set.seed(20240226)

pbp <- read_csv("Data/pbp_2023_final.csv")

calls <- pbp %>% 
  filter(details.description == "Called Strike" |
           details.description == "Ball",
         !is.na(Is.Incorrect),
         Is.Buffer.zone == 0)

predictors <- calls %>% 
  transmute(pitch_hand = matchup.pitchHand.code,
            bat_hand = matchup.batSide.code,
            pitch_num = pitchNumber,
            top_sz = pitchData.strikeZoneTop + (1.45 / 12),
            bottom_sz = pitchData.strikeZoneBottom - (1.45 / 12),
            loc_x = pitchData.coordinates.pX,
            loc_y = pitchData.coordinates.pZ,
            break_x = pitchData.coordinates.pfxX / 12,
            break_y = pitchData.coordinates.pfxZ / 12,
            game_num = gameNumber,
            day_game = dayNight,
            double_header = doubleHeader == "Y",
            balls,
            strikes,
            control_group,
            treatment_one,
            treatment_two) %>% 
  rowwise() %>% 
  mutate(dist_from_edge_vert = c(loc_y - top_sz, bottom_sz - loc_y)[which.min(c(abs(loc_y - top_sz), abs(bottom_sz - loc_y)))],
         dist_from_edge_horz = c(loc_x - (17/24 + 1.45/12), -(17/24 + 1.45/12) - loc_x)[which.min(c(abs(loc_x - (17/24 + 1.45/12)), abs(-(17/24 + 1.45/12) - loc_x)))]) %>%
  ungroup() %>% 
  select(-c(loc_y, top_sz, bottom_sz, loc_x))

build_ind <- sample(1:nrow(predictors), nrow(predictors) * .7)
predictors_build <- predictors[build_ind, ]
predictors_eval <- predictors[-build_ind, ]
calls_build <- calls[build_ind, ]
calls_eval <- calls[-build_ind, ]
train_ind <- sample(1:nrow(predictors_build), nrow(predictors_build) * .65)
predictors_train <- predictors_build[train_ind, ]
predictors_test <- predictors_build[-train_ind, ]
calls_train <- calls_build[train_ind, ]
calls_test <- calls_build[-train_ind, ]

dtrain <- xgb.DMatrix(as.matrix(predictors_train),
                      label = calls_train$Is.Incorrect)
dtest <- xgb.DMatrix(as.matrix(predictors_test),
                     label = calls_test$Is.Incorrect)

myobjective <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  grad <- preds - labels
  hess <- rep(1, length(labels))
  list(grad = grad, hess = hess)
}

eval_brier <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  brier <- (preds - labels)^2
  list(metric = "Brier", value = mean(brier))
}

find_rounds <- function(rounds) {
  xgb.train(
    params = list(
      max_depth = 10,
      eta = .1,
      objective = myobjective,
      eval_metric = eval_brier
    ),
    dtrain,
    nrounds = floor(rounds),
    watchlist = list(train = dtrain,
                     test = dtest)
  ) %>% 
    pluck("evaluation_log") %>% 
    slice_tail(n = 1) %>% 
    pluck("test_Brier")
}
n_rounds <- optimize(find_rounds, c(1, 50), tol = 1) %>% 
  pluck("minimum") %>% 
  floor()


find_depth <- function(depth) {
  xgb.train(
    params = list(
      max_depth = floor(depth),
      eta = .1,
      objective = myobjective,
      eval_metric = eval_brier
    ),
    dtrain,
    nrounds = n_rounds,
    watchlist = list(train = dtrain,
                     test = dtest)
  ) %>% 
    pluck("evaluation_log") %>% 
    slice_tail(n = 1) %>% 
    pluck("test_Brier")
}
depth <- optimize(find_depth, c(1, 50), tol = 1) %>% 
  pluck("minimum") %>% 
  floor()

learning_vals <- data.frame(
  eta = 1 / (100 * (1:10)),
  rounds = n_rounds * 10 * (1:10)
)
expand_mod <- function(eta, rounds) {
  xgb.train(
    params = list(
      max_depth = depth,
      eta = eta,
      objective = myobjective,
      eval_metric = eval_brier
    ),
    dtrain,
    nrounds = rounds,
    watchlist = list(train = dtrain,
                     test = dtest)
  ) %>% 
    pluck("evaluation_log") %>% 
    slice_tail(n = 1) %>% 
    pluck("test_Brier")
}
results <- pmap(learning_vals, expand_mod)

dmat <- xgb.DMatrix(as.matrix(predictors), 
                    label = calls$Is.Incorrect)

bst <- xgb.train(
  params = list(
    max_depth = depth,
    eta = learning_vals$eta[7],
    objective = myobjective,
    eval_metric = eval_brier
  ),
  dtrain,
  nrounds = learning_vals$rounds[7],
  watchlist = list(train = dmat)
)





predictors %>% 
  mutate(preds = predict(bst, as.matrix(predictors)),
         count = glue("{balls}-{strikes}"),
         count = case_when(
           count == "3-2" ~ "3-2",
           count == "0-2" ~ "0-2",
           count == "3-0" ~ "3-0",
           TRUE  ~ "other"
         ),
         pitch = calls$Correct_Call,
         group = case_when(
           calls$control_group == 1 ~ "Ump",
           calls$treatment_one == 1 ~ "Challenge",
           calls$treatment_two == 1 ~ "ABS",
           TRUE ~ "Other"
         )) %>% 
  filter(group != "Other",
         pitch == "Ball",
         dist_from_edge_horz >= .055) -> final
final %>% 
  ggplot(aes(dist_from_edge_horz, preds, color = count)) +
  #geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "quasibinomial"), formula = y ~ splines::ns(x, 3), se = FALSE) +
  facet_grid(cols = vars(group)) +
  scale_y_continuous("Prob of Missed Call", limits = c(0, NA)) +
  scale_x_continuous(limits = c(0, 1))

final %>% 
  ggplot(aes(dist_from_edge_horz, preds, color = group)) +
  #geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "quasibinomial"), formula = y ~ splines::ns(x, 3), se = FALSE) +
  scale_y_continuous("Prob of Missed Call", limits = c(0, NA)) +
  scale_x_continuous(limits = c(0, 1))
  #facet_grid(cols = vars(group))



map(1:100, ~
{
  print(.)
  train_ind <- sample(1:nrow(calls), .6 * nrow(calls))
  calls_train <- calls[train_ind, ]
  calls_test <- calls[-train_ind, ]
  predictors_train <- predictors[train_ind, ]
  predictors_test <- predictors[-train_ind, ]
  
  dtrain <- xgb.DMatrix(as.matrix(predictors_train),
                        label = calls_train$Is.Incorrect)
  bst <- xgb.train(
    params = list(
      max_depth = 8,
      eta = 1 / 700,
      objective = myobjective,
      eval_metric = eval_brier
    ),
    dtrain,
    nrounds = 3360
  )
  
  predictors_test %>% 
    mutate(preds = predict(bst, as.matrix(predictors_test)),
           count = glue("{balls}-{strikes}"),
           count = case_when(
             count == "3-2" ~ "3-2",
             count == "0-2" ~ "0-2",
             count == "3-0" ~ "3-0",
             TRUE  ~ "other"
           ),
           pitch = calls_test$Correct_Call,
           group = case_when(
             calls_test$control_group == 1 ~ "Ump",
             calls_test$treatment_one == 1 ~ "Challenge",
             calls_test$treatment_two == 1 ~ "ABS",
             TRUE ~ "Other"
           )) %>% 
    filter(group != "Other") %>% 
    mutate(ball_dist_vert = cut(dist_from_edge_vert, 
                                breaks = c(-Inf, -1.45 / 3, -1.45 / 6, -1.45 / 12, 1.45 / 12, 1.45 / 6, 1.45 / 3, Inf),
                                labels = c("Two balls in", "One ball in", "Half ball in", "Very close", "Half ball out", "One ball out", "Two balls out")),
           ball_dist_horz = cut(dist_from_edge_horz, 
                                breaks = c(-Inf, -1.45 / 3, -1.45 / 6, -1.45 / 12, 1.45 / 12, 1.45 / 6, 1.45 / 3, Inf),
                                labels = c("Two balls in", "One ball in", "Half ball in", "Very close", "Half ball out", "One ball out", "Two balls out"))) -> final
  list(vert = final %>% 
         group_by(ball_dist_vert, group, count) %>% 
         summarise(preds = mean(preds)),
       horz = final %>% 
         group_by(ball_dist_horz, group, count) %>% 
         summarise(preds = mean(preds)))
}
) -> sim_avgs

sim_avgs %>% 
  transpose() %>% 
  pluck("horz") %>% 
  bind_rows() -> vdist
sim_avgs %>% 
  transpose() %>% 
  pluck("vert") %>% 
  bind_rows() -> hdist

cbind(vdist, hdist) %>%
  select(-5:-7) -> graph
names(graph) = c("dist", "group", "count", "h_preds", "v_preds")

offset_width <- 0.15

# Facet by Group w Distance Bottom

filtered_graph <- graph %>%
  filter(group != "ABS")

ggplot(filtered_graph, aes(dist, color = count)) +
  stat_summary(aes(y = h_preds), fun = mean,
               position = position_nudge(x = -offset_width),
               fun.min = ~quantile(., 0.05), fun.max = ~quantile(., 0.95)) +
  stat_summary(aes(y = v_preds), fun = mean,
               position = position_nudge(x = offset_width),
               fun.min = ~quantile(., 0.05), fun.max = ~quantile(., 0.95)) +
  facet_grid(cols = vars(group)) +
  scale_y_continuous(limits = c(0, NA)) +
  theme(axis.text.x = element_text(size = rel(0.75), angle = 90)) + 
  theme_bw() +
  xlab("Distance from the Edge") +
  ylab("Predicted Incorrect Call %")

# Facet by Distance w Group Bottom

ggplot(filtered_graph, aes(group, color = count)) +
  stat_summary(aes(y = h_preds), fun = mean,
               position = position_nudge(x = -offset_width),
               fun.min = ~quantile(., 0.05), fun.max = ~quantile(., 0.95)) +
  stat_summary(aes(y = v_preds), fun = mean,
               position = position_nudge(x = offset_width),
               fun.min = ~quantile(., 0.05), fun.max = ~quantile(., 0.95)) +
  facet_grid(cols = vars(dist)) +
  scale_y_continuous(limits = c(0, NA)) +
  theme(axis.text.x = element_text(size = rel(0.75), angle = 90)) + 
  theme_bw() +
  xlab("Treatment Group") +
  ylab("Predicted Incorrect Call %")

# Facet by Distance w Count Bottom

my_colors <- c("Challenge" = "#00ba38", "Ump" = "#00bfc4", "ABS" = "#f8766d")

ggplot(graph, aes(count, color = group)) +
  stat_summary(aes(y = h_preds), fun = mean,
               position = position_nudge(x = -offset_width),
               fun.min = ~quantile(., 0.05), fun.max = ~quantile(., 0.95)) +
  stat_summary(aes(y = v_preds), fun = mean,
               position = position_nudge(x = offset_width),
               fun.min = ~quantile(., 0.05), fun.max = ~quantile(., 0.95)) +
  facet_grid(cols = vars(dist)) +
  scale_y_continuous(limits = c(0, NA)) +
  scale_color_manual(values = my_colors) +  # Set manual colors
  theme_bw() +
  xlab("Treatment Group") +
  ylab("Predicted Incorrect Call %")


# No Facets

ggplot(graph, aes(dist, color = group)) +
  stat_summary(aes(y = h_preds), fun = mean,
               position = position_nudge(x = -offset_width),
               fun.min = ~quantile(., 0.05), fun.max = ~quantile(., 0.95)) +
  stat_summary(aes(y = v_preds), fun = mean,
               position = position_nudge(x = offset_width),
               fun.min = ~quantile(., 0.05), fun.max = ~quantile(., 0.95)) +
  scale_y_continuous(limits = c(0, NA)) +
  theme(axis.text.x = element_text(size = rel(0.75), angle = 90)) + 
  theme_bw() +
  xlab("Distance from the Edge") +
  ylab("Predicted Incorrect Call %")


# Ideal parameter values: 
#   tree depth of 8
#   eta of 1 / 700
#   rounds of 3360

load("post_100_bootstrap.RData.RDataTmp")

# Combine and jitter the data
combined_data <- bind_rows(
  sim_avgs %>% 
    transpose() %>% 
    pluck("horz") %>% 
    mutate(type = "horizontal"),
  sim_avgs %>% 
    transpose() %>% 
    pluck("vert") %>% 
    mutate(type = "vertical")
)

# Combine and jitter the data
combined_data <- map_dfr(sim_avgs, ~bind_rows(.x$horz, .x$vert, .id = "group")) %>%
  mutate(type = ifelse(row_number() <= nrow(sim_avgs[[1]]$horz), "horizontal", "vertical"))
