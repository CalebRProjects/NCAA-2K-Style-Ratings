# Men's D1 Team Strength & SOS
# Built using ESPN data via hoopR
# Produces z-scored team strength and SOS values for 2025 season 
# Author: Caleb Ramsey & Baker Dean, STAT 3274 Group Project

library(dplyr); library(tidyr); library(hoopR); library(readr); library(stringr)

season = getOption("mbb_season", 2025)
out_dir = getOption("mbb_outdir", ".")

pick_exact = function(df, candidates)
  {
  ln = tolower(names(df))
  for(c in tolower(candidates))
    {
    i = match(c, ln, nomatch = 0)
    if(i > 0)return(names(df)[i])
    }
  NULL
  }

valid_conf_ids = 1:32

sched = hoopR::load_mbb_schedule(seasons = season, season_types = "Regular Season") %>%
  filter(
    home_conference_id %in% valid_conf_ids,
    away_conference_id %in% valid_conf_ids
    )

gid = pick_exact(sched, c("game_id","id"))
hid = pick_exact(sched, c("home_team_id","home_id"))
aid = pick_exact(sched, c("away_team_id","away_id"))
hs = pick_exact(sched, c("home_team_score","home_score","home_points"))
ascol = pick_exact(sched, c("away_team_score","away_score","away_points"))
hname = pick_exact(sched, c("home_team_display_name","home_display_name","home_team_name"))
aname = pick_exact(sched, c("away_team_display_name","away_display_name","away_team_name"))

stopifnot(!is.null(gid), !is.null(hid), !is.null(aid), !is.null(hs), !is.null(ascol))

scores = sched %>%
  transmute(
    game_id = as.character(.data[[gid]]),
    home_id = as.character(.data[[hid]]),
    away_id = as.character(.data[[aid]]),
    home_name = if(!is.null(hname)) as.character(.data[[hname]])
      else(NA_character_),
    away_name = if(!is.null(aname)) as.character(.data[[aname]])
      else(NA_character_),
    hs = suppressWarnings(as.numeric(.data[[hs]])),
    `as` = suppressWarnings(as.numeric(.data[[ascol]]))
    ) %>% filter(is.finite(hs), is.finite(`as`))

compute_team_strengths = function(scores, lam = 1e-2)
  {
  wide = scores %>% transmute(home = home_id, away = away_id, margin = hs - `as`)
  teams = sort(unique(c(wide$home, wide$away)))
  Tn = length(teams)
  idx = setNames(seq_len(Tn), teams)
  
  A = matrix(0, nrow = nrow(wide), ncol = Tn + 1)
  A[cbind(seq_len(nrow(wide)), idx[wide$home])] = 1
  A[cbind(seq_len(nrow(wide)), idx[wide$away])] = -1
  A[, Tn+1] = 1
  y = wide$margin
  
  coef = solve(t(A) %*% A + diag(lam, Tn+1), t(A) %*% y)
  s = as.numeric(coef[1:Tn])
  s = s - mean(s)
  z = as.numeric(scale(s))
  names(z) = teams
  z
  }

team_z_map = compute_team_strengths(scores, lam = 1e-2)

team_sos = bind_rows(
  scores %>% transmute(team_id = home_id, opp_id = away_id, wt = 0.90),
  scores %>% transmute(team_id = away_id, opp_id = home_id, wt = 1.10)
  ) %>%
  mutate(opp_z = unname(replace_na(team_z_map[opp_id], 0))) %>%
  group_by(team_id) %>%
  summarise(sos_raw = mean(opp_z * wt, na.rm = T), .groups = "drop") %>%
  mutate(sos_z = as.numeric(scale(sos_raw)))

id_to_name = bind_rows(
  scores %>% transmute(team_id = home_id, team_name = home_name),
  scores %>% transmute(team_id = away_id, team_name = away_name)
  ) %>%
  filter(!is.na(team_id)) %>%
  distinct()

team_strength = tibble(
  team_id = names(team_z_map),
  team_z = unname(team_z_map)
  ) %>%
  left_join(id_to_name, by = "team_id") %>%
  relocate(team_id, team_name, team_z)

team_strength_sos = team_strength %>%
  left_join(team_sos, by = "team_id")

team_conf = bind_rows(
  sched %>% transmute(team_id = as.character(.data[[hid]]), conference_id = home_conference_id),
  sched %>% transmute(team_id = as.character(.data[[aid]]), conference_id = away_conference_id)
  ) %>% distinct(team_id, .keep_all = T)

conf_strength = team_strength_sos %>%
  left_join(team_conf, by = "team_id") %>%
  group_by(conference_id) %>%
  summarise(
    n_teams = dplyr::n(),
    avg_team_strength = mean(team_z, na.rm = T),
    avg_sos = mean(sos_z, na.rm = T),
    .groups = "drop") %>%
  mutate(
    conf_strength_z = as.numeric(scale(avg_team_strength)),
    conf_sos_z = as.numeric(scale(avg_sos)))

team_conf_names = bind_rows(
  sched %>% transmute(team_id = as.character(.data[[hid]]), conference_id = home_conference_id),
  sched %>% transmute(team_id = as.character(.data[[aid]]), conference_id = away_conference_id)) %>%
  distinct(team_id, .keep_all = T) %>%
  left_join(id_to_name, by = "team_id") %>%
  group_by(conference_id) %>%
  summarise(
    teams = list(sort(unique(na.omit(team_name)))),
    team_count = length(teams[[1]]),
    teams_str = paste(teams[[1]], collapse = " | "),
    .groups = "drop"
    )

conf_strength_named = conf_strength %>%
  left_join(team_conf_names %>% select(conference_id, team_count, teams_str), by = "conference_id") %>%
  arrange(desc(conf_strength_z))

readr::write_csv(conf_strength_named, file.path(out_dir, "mbb_conf_strength_named.csv"))
