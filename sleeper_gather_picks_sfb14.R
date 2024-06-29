library(ffscrapr)
library(dplyr)
library(data.table)
library(tidyr)
library(purrr)
library(stringr)
library(piggyback)
library(cli)
library(readr)
library(httr2)
library(jsonlite)
library(lubridate)

#Disable scientific notation
options(scipen = 999)


# looks like the ff_connect doesn't return all of the avail draft vars?

search_year = "2024"
sfb_sleeper_string = "\\#SFB14"

#### DELETE AFTER TESTING ########
#GITHUB_PAT <- Sys.setenv("GITHUB_PAT")


sleeper_leagues <- sleeper_userleagues("ScottFishBowl", search_year) |>
  #select(league_id, league_name) |>
  filter(str_detect(league_name,sfb_sleeper_string)) |>
  mutate(league_id = as.character(league_id)) |>
  filter(!grepl("Satellite|Mirror", league_name))

fwrite(sleeper_leagues,"league_ids_sleeper.csv")

get_sleeper_draft <- function(league_id){
  cli::cli_alert("League ID: {league_id}")
  Sys.sleep(1)
  conn <- sleeper_connect(search_year , league_id, rate_limit = F)
  suppressWarnings(ff_draft(conn))
}

#draft <- get_sleeper_draft("1107125222018228224")

#draft_metadata <- get_sleeper_draft_meta("1107125222018228224")


get_sleeper_draft_meta <- function(league_id) {

  cli::cli_alert("Metadata for League ID: {league_id}")
  Sys.sleep(1)
  api_url <-glue::glue("https://api.sleeper.app/v1/league/{league_id}/drafts")
  draft_json <- fromJSON(api_url, flatten =TRUE) |>
    as_tibble() |>
    select( settings.pick_timer, settings.autopause_start_time,start_time, settings.autopause_end_time, metadata.name,draft_id, last_picked)


}


# FOR TESTING
#sleeper_leagues <- sleeper_leagues |>
#  slice_head(n=25)

#Hmmmmm -- might need to write a function to directly get drafts too
# simply for timestamps, however good news, last picked is a valid rep of last picked
#test_draft <- get_draft("970484659941421056")

# Metadata works for in progress
#test_draft_meta <- get_sleeper_draft_meta("970484659941421056")


sleeper_drafts <- sleeper_leagues |>
  select(league_id, league_name) |>
  mutate(drafts = map(league_id, possibly(get_sleeper_draft, otherwise = tibble()))) |>
  unnest(drafts) |>
  group_by(league_id) |>
  mutate(overall = row_number()) |>
  ungroup()


sleeper_draft_metadata <- sleeper_leagues |>
  select(league_id, league_name) |>
  mutate(drafts_meta = map(league_id, possibly(get_sleeper_draft_meta, otherwise = tibble()))) |>
  unnest(drafts_meta) 


sleeper_draft_metadata <- sleeper_draft_metadata |>
  # We need this oddness because the sleeper timestamp appears to have 3 extra chars
  # unknown what this is is. Milliseconds?
  # For drafts that haven't started, start time is not returned which is annyoing
  # Add a check if it's there.
  mutate(start_time = ifelse(start_time %in% colnames(sleeper_draft_metadata),substr(format(start_time, scientific= FALSE),1,10),NA),
         last_picked = substr(format(last_picked, scientific= FALSE),1,10)

  )
# start_time throwing an error.. why? It doesn't exist yet

# Since sleeper doesn't return the draft picks before they are made
# check if we have any results
if(nrow(sleeper_drafts) == 0){
sleeper_drafts <- read_csv("https://github.com/mohanpatrick/sfb_13/files/11644363/draft_picks_sleeper.csv") |>
  filter(overall ==0)

  
}

sleeper_current_picks <- sleeper_drafts |>
  filter(!is.na(player_name)) |>
  group_by(league_id) |>
  summarize(
    last_pick = max(overall)
  ) |>
  mutate(otc = last_pick +1,
         league_id = as.double(league_id))


sleeper_draft_metadata <- sleeper_draft_metadata |>
  mutate(
  
      start_time_dt = as.POSIXct(as.numeric(start_time), origin = "1970-01-01"),
        last_picked_dt = as_datetime(as.numeric(last_picked),tz = "America/New_York"),
      league_url = paste0("https://sleeper.com/leagues/", league_id, sep=""),
      league_id = as.double(league_id)
  ) |>
  left_join(sleeper_current_picks)





write_csv(sleeper_drafts, "draft_picks_sleeper.csv")

write_csv(sleeper_draft_metadata, "draft_metadata_sleeper.csv")

write_csv(sleeper_leagues, "league_ids_sleeper.csv")

# Read in csvs if we're offline
#sleeper_drafts <- read_csv("draft_picks_sleeper.csv") |>
 # mutate(player_id = as.character(player_id))
#sleeper_draft_metadata <- read_csv("draft_metadata_sleeper.csv")
#sleeper_leagues <- read_csv("league_ids_sleeper.csv")


# Need to bring in all player_ids so we can add MFL ids here



pb_upload("draft_metadata_sleeper.csv",
          repo = "mohanpatrick/sfb_13",
          tag = "data_sleeper")
cli::cli_alert_success("Successfully uploaded to Git")

pb_upload("league_ids_sleeper.csv",
          repo = "mohanpatrick/sfb_14",
          tag = "data_sleeper")
cli::cli_alert_success("Successfully uploaded to Git")



pb_upload("draft_picks_sleeper.csv",
          repo = "mohanpatrick/sfb_14",
          tag = "data_sleeper")
cli::cli_alert_success("Successfully uploaded to Git")






