library(dplyr)
library(readr)
library(stringr)
library(scales)

# SET THE WORKING DIRECTORY TO THE FOLDER WHERE YOU DOWNLOADED THE FILES
setwd("C:/Users/bwdea/OneDrive/Documents/STAT 3274/Final Project")

# Helper function to clean names
clean_name <- function(x) {x %>% tolower() %>% str_replace_all("[^a-z0-9]", "")}

# Read in files
men_adj <- read_csv("men_adjusted.csv",show_col_types = FALSE)
men_conf_z <- read_csv("men_conf_z_scores_only.csv",show_col_types = FALSE)
hdi_raw <- read_csv("HDI Ratings 2024-25.csv",show_col_types = FALSE)
women_adj <- read_csv("women_adjusted.csv",show_col_types = FALSE)
conf_w <- read_csv("wbb_conf_z_2025.csv",show_col_types = FALSE)
team_w <- read_csv("wbb_team_conf_zkeys_2025.csv",show_col_types = FALSE) %>%
  mutate(team_key = clean_name(team_key))
conf_map_m <- read.table("conf_map_men.txt",sep="=",strip.white=TRUE,col.names=c("School_raw","Conf"),comment.char="",quote="") %>%
  mutate(School_key = clean_name(School_raw))

# Attach conference to the men's adjusted data
men_adj_conf_base <- men_adj %>%
  mutate(School_key = clean_name(School)) %>%
  left_join(conf_map_m %>% select(School_key, Conf), by = "School_key") %>%
  left_join(men_conf_z, by = "Conf") %>%
  mutate(conf_z = coalesce(conf_z, 0))

# Attach conference to the women's adjusted data
women_adj_conf_base <- women_adj %>%
  mutate(team_key = clean_name(School)) %>%
  left_join(team_w %>% select(team_key, team_id, conference_id), by = "team_key") %>%
  left_join(conf_w %>% select(conference_id, conf_z), by = "conference_id") %>%
  mutate(conf_z = coalesce(conf_z, 0))

# Define a function to adjust ratings based on conference z-score using an exponential function
conf_adjust_rating <- function(rating,conf_z,power=3.0,alpha_pos=0.12,alpha_neg=0.49,conf_clip_sd=2.0,rating_floor=40,rating_span=60){
  rating_norm <- (rating - rating_floor) / rating_span
  rating_norm <- pmax(pmin(rating_norm, 1), 0)
  conf_std <- as.numeric(scale(conf_z))
  conf_std <- pmax(pmin(conf_std, conf_clip_sd), -conf_clip_sd)
  w <- rating_norm^power
  conf_pos <- pmax(conf_std, 0)
  conf_neg <- pmin(conf_std, 0)
  effect <- alpha_pos * conf_pos + alpha_neg * conf_neg
  mult   <- exp(w * effect)
  rating * mult
}

# Apply this new conference adjuster function to the men's adjusted stats
men_adjusted_conf <- men_adj_conf_base %>%
  mutate(Rating_conf = conf_adjust_rating(Rating, conf_z)) %>%
  mutate(Rating_conf = round(rescale(Rating_conf, to = c(40, 99)))) %>%
  arrange(desc(Rating_conf)) %>%
  select(Name, School, Rating_conf) %>%
  rename(Rating = Rating_conf)

# Apply this new conference adjuster function to the women's adjusted stats
women_adjusted_conf <- women_adj_conf_base %>%
  mutate(Rating = as.numeric(Rating),Rating_conf = conf_adjust_rating(Rating, conf_z)) %>%
  mutate(Rating_conf = round(rescale(Rating_conf, to = c(40, 99)))) %>%
  arrange(desc(Rating_conf)) %>% 
  select(Name, School, Rating_conf) %>%
  rename(Rating = Rating_conf)

# Optional: write the resulting rankings to a csv
write_csv(men_adjusted_conf,"men_adjusted_conf_adjusted.csv")
write_csv(women_adjusted_conf,"women_adjusted_conf_adjusted.csv")

# Clean the HDI data to evaluate how closely the men match as a realism check
hdi_small <- hdi_raw %>%
  mutate(name_key = clean_name(`Full Name...2`), school_key = clean_name(`2024-2025 School`)) %>%
  select(name_key, school_key, hdi_rating = Rating) %>%
  filter(!is.na(hdi_rating))

# Function to evaluate any set of ratings vs HDI
evaluate_vs_hdi <- function(df_conf, label = "MODEL") {
  df_eval <- df_conf %>%
    mutate(name_key=clean_name(Name),school_key=clean_name(School)) %>%
    inner_join(hdi_small, by = c("name_key", "school_key")) %>%
    select(Name, School, Rating, hdi_rating) %>%
    arrange(desc(Rating)) %>%
    mutate(rank_model = row_number()) %>%
    arrange(desc(hdi_rating)) %>%
    mutate(rank_hdi = row_number()) %>%
    mutate(rank_diff = rank_model - rank_hdi,abs_rank_diff=abs(rank_diff))
  
  rho <- cor(df_eval$Rating, df_eval$hdi_rating, method = "spearman", use = "complete.obs")
  mean_abs <- mean(df_eval$abs_rank_diff, na.rm = TRUE)
  top_k <- 100
  overlap <- length(intersect(
    df_eval %>% arrange(rank_model) %>% slice_head(n = top_k) %>% pull(Name),
    df_eval %>% arrange(rank_hdi)   %>% slice_head(n = top_k) %>% pull(Name)
  )) / top_k
  
  cat("\n=============================================\n")
  cat("MEN vs HDI DIAGNOSTICS:", label, "\n")
  cat("Matched players  :", nrow(df_eval), "\n")
  cat("Spearman rho     :", round(rho, 4), "\n")
  cat("Mean abs diff    :", round(mean_abs, 1), "\n")
  cat("Top-100 overlap  :", round(100*overlap, 1), "%\n")
  cat("=============================================\n")
  
  list(
    summary = tibble(
      matched_players = nrow(df_eval),
      spearman_rho = rho,
      mean_abs_rank_diff = mean_abs,
      top100_overlap_frac = overlap
    ),
    worst = df_eval %>% arrange(desc(abs_rank_diff)) %>% slice_head(n = 20),
    best  = df_eval %>% arrange(abs_rank_diff) %>% slice_head(n = 20),
    data  = df_eval
  )
}

# Evaluate the stats-adjusted, conference-adjusted ratings
res_adj  <- evaluate_vs_hdi(men_adjusted_conf, label = "MEN ADJUSTED + CONF")
