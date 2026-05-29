# Read in libraries
library(dplyr)
library(stringr)
library(tidyr)
library(cbbdata)
library(randomForest)
library(caret)
library(scales)

# Read in 2023 data
hdi23 <- read.csv("C:/Users/bwdea/OneDrive/Documents/STAT 3274/Final Project/Data/HDI Ratings 2023-24.csv")
hdi23 <- hdi23 %>%
  rename(Name = Full.Name, Team = X2023.2024.School) %>%
  select(Name, Team, Rating)
barttorvik_2023 <- cbd_torvik_player_season(year = 2024)
barttorvik_2023 <- barttorvik_2023 %>% rename(Name = player, Team = team)
source("C:/Users/bwdea/OneDrive/Documents/STAT 3274/Final Project/hdi_name_matcher.R")
ratings_23 <- join_hdi_rating(barttorvik_2023, hdi23) %>%
  select(Name, pos, exp, hgt, Team, conf,mpg, ppg, rpg, apg, spg, bpg,
    two_pct, three_pct, ft_pct,tov, ast_to, efg, ts, usg, ortg, drtg,
    rim_pct, mid_pct, obpm, dbpm, bpm, ftr, pfr,HDI_Rating, year) %>%
  rename(Rating = HDI_Rating) %>%
  filter(!is.na(Rating))

# Read in 2024 data
hdi24 <- read.csv("C:/Users/bwdea/OneDrive/Documents/STAT 3274/Final Project/Data/HDI Ratings 2024-25.csv")
hdi24 <- hdi24 %>%
  rename(Name = Full.Name, Team = X2024.2025.School) %>%
  select(Name, Team, Rating)
barttorvik_2024 <- cbd_torvik_player_season(year = 2025)
barttorvik_2024 <- barttorvik_2024 %>% rename(Name = player, Team = team)
ratings_24 <- join_hdi_rating(barttorvik_2024, hdi24) %>%
  select(Name, pos, exp, hgt, Team, conf,mpg, ppg, rpg, apg, spg, bpg,
    two_pct, three_pct, ft_pct,tov, ast_to, efg, ts, usg, ortg, drtg,
    rim_pct, mid_pct, obpm, dbpm, bpm, ftr, pfr, HDI_Rating, year) %>%
  rename(Rating = HDI_Rating) %>%
  filter(!is.na(Rating))

# Read in women's data and mutate to match men's format
women <- read.csv("C:/Users/bwdea/OneDrive/Documents/STAT 3274/Final Project/Data/wcbb_players_2025_D1.csv")
women_model_base <- women %>%
  mutate(
    mpg = minutes / games_played,
    ppg = points / games_played,
    rpg = rebounds / games_played,
    apg = assists / games_played,
    spg = steals / games_played,
    bpg = blocks / games_played,
    tov = turnovers / games_played,
    two_fg_made = field_goals_made - three_point_made,
    two_fg_att  = field_goals_attempted - three_point_attempted,
    two_pct = ifelse(two_fg_att > 0, two_fg_made / two_fg_att, NA_real_),
    ast_to = ifelse(turnovers > 0, assists / turnovers, NA_real_),
    efg = ifelse(field_goals_attempted > 0, (field_goals_made + 0.5 * three_point_made) / field_goals_attempted, NA_real_),
    ts = ifelse(field_goals_attempted + 0.44 * free_throws_attempted > 0, points / (2 * (field_goals_attempted + 0.44 * free_throws_attempted)), NA_real_),
    ftr = ifelse(field_goals_attempted > 0, free_throws_attempted / field_goals_attempted, NA_real_),
    pfr = personal_fouls / games_played) %>%
  select(
    Name   = player_name,
    School = team_name,
    mpg, ppg, rpg, apg, spg, bpg,two_pct,
    three_pct = three_point_percent,
    ft_pct    = free_throw_percent,
    tov, ast_to, efg, ts, ftr, pfr) %>%
  na.omit()

# Train a random forest model based on the HDI ratings for the 2024-25 season

ratings <- rbind(ratings_23, ratings_24)

ratings_model_data <- ratings %>% select(mpg, ppg, rpg, apg, spg, bpg, two_pct, three_pct, ft_pct, tov, ast_to, efg, ts, ftr, pfr, Rating) %>% na.omit()

set.seed(3274)
index <- sample(nrow(ratings_model_data), size = floor(0.8 * nrow(ratings_model_data)))
train <- ratings_model_data[index, ]
test  <- ratings_model_data[-index, ]

ratings_model <- randomForest(Rating ~ mpg+ppg+rpg+apg+spg+bpg+two_pct+three_pct+ft_pct+tov+ast_to+efg+ts+ftr+pfr,data=train,ntree=1000,importance=TRUE)

# Print model evaluations
print(ratings_model)
importance(ratings_model)

predictions <- predict(ratings_model, newdata = test)
actual      <- test$Rating
rmse        <- sqrt(mean((predictions - actual)^2))
r_squared   <- cor(predictions, actual)^2
cat(" RMSE:", rmse, "\n", "R-squared:", r_squared, "\n")

# Create ratings adjustment for the men
calc_creation <- function(df) {0.50 * df$ppg + 0.40 * df$apg + 0.35 * df$spg + 0.25 * df$three_pct + 0.20 * df$ts}

# Create ratings adjustment for the women
calc_bigscore <- function(df) {0.50 * df$bpg + 0.35 * df$rpg + 0.25 * df$two_pct + 0.20 * df$efg}

# Define a function to apply the ratings adjustments
apply_role_adjustment <- function(df,creation_gain,bigman_penalty) {
  df %>% mutate(
      creation_score = calc_creation(cur_data_all()),
      big_score      = calc_bigscore(cur_data_all()),
      creation_z     = rescale(creation_score, to = c(0, 1)),
      big_z          = rescale(big_score,      to = c(0, 1)),
      Rating_adj     = predicted_rating +
        creation_gain  * creation_z -
        bigman_penalty * big_z
    )
}

# Men: stronger correction factors
men_creation_gain  <- 3.5
men_bigman_penalty <- 3.0

# Women: lighter correction factors
women_creation_gain  <- 2.0
women_bigman_penalty <- 1.5

# Create raw and adjusted ratings for the men
men_model_raw <- ratings_24 %>% select(Name, Team, mpg, ppg, rpg, apg, spg, bpg, two_pct, three_pct, ft_pct, tov, ast_to, efg, ts, ftr, pfr) %>% na.omit()
men_model_raw$predicted_rating <- as.numeric(predict(ratings_model, newdata = men_model_raw))
men_model_adj <- apply_role_adjustment(men_model_raw,creation_gain  = men_creation_gain,bigman_penalty = men_bigman_penalty)
men_raw <- men_model_raw %>% select(Name, School = Team, Rating = predicted_rating) %>% arrange(desc(Rating))
men_adjusted <- men_model_adj %>% select(Name, School = Team, Rating = Rating_adj) %>% arrange(desc(Rating))

# Create raw and adjusted ratings for the women
women_model_raw <- women_model_base
women_model_raw$predicted_rating <- as.numeric(predict(ratings_model,newdata = women_model_raw %>%
      select(mpg, ppg, rpg, apg, spg, bpg, two_pct, three_pct, ft_pct, tov, ast_to, efg, ts, ftr, pfr)))
women_model_adj <- apply_role_adjustment(women_model_raw,creation_gain=women_creation_gain,bigman_penalty=women_bigman_penalty)
women_raw <- women_model_raw %>% select(Name, School, Rating = predicted_rating) %>% arrange(desc(Rating))
women_adjusted <- women_model_adj %>% select(Name, School, Rating = Rating_adj) %>% arrange(desc(Rating))

# Optional: write results to csv
write.csv(men_raw,"men_raw.csv",row.names = FALSE)
write.csv(men_adjusted,"men_adjusted.csv",row.names = FALSE)
write.csv(women_raw,"women_raw.csv",row.names = FALSE)
write.csv(women_adjusted,"women_adjusted.csv",row.names = FALSE)