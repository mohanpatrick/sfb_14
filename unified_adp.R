library(ffscrapr)
library(dplyr)
library(data.table)
library(tidyr)
library(purrr)
library(stringr)
library(piggyback)
library(cli)
library(readr)
library(lubridate)


# Read Draft picks from both
############### Delete LATER


mode = "ONLINE"

if(mode == "OFFLINE") {
draft_picks_sleeper <- read_csv("draft_picks_sleeper.csv")
draft_picks_mfl <- read_csv("draft_picks_mfl.csv") |>
  filter(!is.na(player_id))
}else{
  draft_picks_sleeper <- read_csv("https://github.com/mohanpatrick/sfb_14/releases/download/data_sleeper/draft_picks_sleeper.csv")
  draft_picks_mfl <- read_csv("https://github.com/mohanpatrick/sfb_14/releases/download/data_mfl/draft_picks_mfl.csv")|>
    filter(!is.na(player_id))
}


# Get master player info, Note, this takes a sec, cache it

master_player_ids <- dp_playerids() |>
  select(mfl_id, sleeper_id, name, merge_name, position, team) |>
  filter(position %in% c("QB","WR", "TE", "RB", "K", "PK", "FB")) |>
  mutate(
    cross_mfl_sleep_id = paste0(mfl_id, "-", sleeper_id),
    sleeper_id = as.double(sleeper_id),
    mfl_id = as.double(mfl_id),
    clean_team = team
  )

n_sleeper_picks <- nrow(draft_picks_sleeper)
n_mfl_picks <- nrow(draft_picks_mfl)

if (n_sleeper_picks == "0" & n_mfl_picks == "0") {

  cli::cli_progress_message("No picks in either data set. Exiting script")
  quit(status = 0)
}





cli::cli_progress_message("Woo-hoo picks found! Calculating ADP")

# Join back for sleeper, mfl, skip for now as they are there on file. maybe add back to gather?
draft_picks_sleeper <- draft_picks_sleeper |>
  mutate(pos = ifelse(pos == "K", "PK", pos))|>
  left_join(master_player_ids, by=c("player_id" = "sleeper_id", "pos" = "position"))

draft_picks_mfl <- draft_picks_mfl |>
  left_join(master_player_ids, by=c("player_id" = "mfl_id"))
  
wut <- draft_picks_sleeper |> filter (is.na(name))


mfl_for_adp <- draft_picks_mfl |>
  select(league_id,cross_mfl_sleep_id, pos, clean_team, overall, name)

sleeper_for_adp <- draft_picks_sleeper |>
  select(league_id, cross_mfl_sleep_id, pos, clean_team, overall, player_name, name)|>
  mutate(name =ifelse(!is.na(name), name, player_name))|>
  select(-player_name)


# Issues -- there are different team abbr. Might need to take from dp_player set
# Lots of NAs, why only 31 n for some


mfl_only_adp <- mfl_for_adp |>
  group_by(league_id,pos) |>
  mutate(pos_rank = rank(overall)) |>
  group_by(cross_mfl_sleep_id, name, pos) |>
  summarise(
    n = n(),
    overall_avg = mean(overall, na.rm = TRUE) |> round(2),
    overall_sd = sd(overall, na.rm = TRUE) |> round(2),
    pos_avg = mean(pos_rank, na.rm = TRUE) |> round(2),
    pos_sd = sd(pos_rank, na.rm = TRUE) |> round(2),
    overall_min = min(overall, na.rm = TRUE),
    overall_max = max(overall, na.rm = TRUE),
    pos_min = min(pos_rank, na.rm = TRUE),
    pos_max = max(pos_rank, na.rm = TRUE)
  ) |>
  ungroup() |>
  arrange(overall_avg,-n)

sleeper_only_adp <- sleeper_for_adp |>
  group_by(league_id,pos) |>
  mutate(pos_rank = rank(overall)) |>
  group_by(cross_mfl_sleep_id, name, pos) |>
  summarise(
    n = n(),
    overall_avg = mean(overall, na.rm = TRUE) |> round(2),
    overall_sd = sd(overall, na.rm = TRUE) |> round(2),
    pos_avg = mean(pos_rank, na.rm = TRUE) |> round(2),
    pos_sd = sd(pos_rank, na.rm = TRUE) |> round(2),
    overall_min = min(overall, na.rm = TRUE),
    overall_max = max(overall, na.rm = TRUE),
    pos_min = min(pos_rank, na.rm = TRUE),
    pos_max = max(pos_rank, na.rm = TRUE)
  ) |>
  ungroup() |>
  arrange(overall_avg,-n)



#sleeper has picks, so modify so it doesn't barf until MFL does

if(n_mfl_picks >0 ){
# Need to retain mfl id for my app
comb_adp <- mfl_for_adp |>
  union_all(sleeper_for_adp) |>
  group_by(league_id,pos) |>
  mutate(pos_rank = rank(overall)) |>
  group_by(cross_mfl_sleep_id, name, pos) |>
  summarise(
    n = n(),
    overall_avg = mean(overall, na.rm = TRUE) |> round(2),
    overall_sd = sd(overall, na.rm = TRUE) |> round(2),
    pos_avg = mean(pos_rank, na.rm = TRUE) |> round(2),
    pos_sd = sd(pos_rank, na.rm = TRUE) |> round(2),
    overall_min = min(overall, na.rm = TRUE),
    overall_max = max(overall, na.rm = TRUE),
    pos_min = min(pos_rank, na.rm = TRUE),
    pos_max = max(pos_rank, na.rm = TRUE)
  ) |>
  ungroup() |>
  arrange(overall_avg,-n)
}else{
  comb_adp <- sleeper_only_adp
  
}

# Add back mfl id for app
write_csv(mfl_only_adp, "mfl_only_adp.csv")
write_csv(sleeper_only_adp, "sleeper_only_adp.csv")
write_csv(comb_adp, "combined_adp.csv")


pb_upload("sleeper_only_adp.csv",
          repo = "mohanpatrick/sfb_14",
          tag = "data_sleeper")
cli::cli_alert_success("Successfully uploaded to Git")


pb_upload("combined_adp.csv",
          repo = "mohanpatrick/sfb_14",
          tag = "data_mfl")
cli::cli_alert_success("Successfully uploaded to Git")



pb_upload("mfl_only_adp.csv",
          repo = "mohanpatrick/sfb_14",
          tag = "data_mfl")
cli::cli_alert_success("Successfully uploaded to Git")

