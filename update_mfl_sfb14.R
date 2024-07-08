library(ffscrapr)
library(dplyr)
library(data.table)
library(tidyverse)
library(tidyr)
library(purrr)
library(stringr)
library(piggyback)
library(cli)
library(readr)
library(lubridate)
library(httr2)
library(jsonlite)
library(lubridate)

options(dplyr.summarise.inform = FALSE,
        piggyback.verbose = FALSE)

#### DELETE AFTER TESTING ########
#GITHUB_PAT <- Sys.setenv("GITHUB_PAT")
#Sys.setenv("MFL_CLIENT")

search_year = "2024"
sfb_mfl_string = "\\#SFB14"
mfl_client = "MFLRCLIENT"

mfl_client <- Sys.getenv(c("MFL_CLIENT"))
cli::cli_alert("Client ID: {mfl_client}")

 mfl_leagues <- mfl_getendpoint(mfl_connect(search_year),"leagueSearch", user_agent=mfl_client, SEARCH=sfb_mfl_string) |>
   purrr::pluck("content","leagues","league") |>
   tibble::tibble() |>
   tidyr::unnest_wider(1) |>
   select( league_name = name, league_id = id,league_home = homeURL) |>
   # Going to need some stricter filtering patterns, but for now we take out obvious not real ones
    filter(!grepl("Mock|Template|Sattelite|Mirror", league_name))|>
    filter(league_id != "57652")
# Add filter for 57652 the stray miller liter


#prior_completed_leagues <- read_csv("https://github.com/mohanpatrick/sfb_13/releases/download/data_mfl/completed_leagues.csv", col_names = c("league_name","league_id", "league_home"))|>
#  mutate(league_id = as.character(league_id))|>
#  select(league_id)



#mfl_leagues <- mfl_leagues |>
#anti_join(prior_completed_leagues)


 fwrite(mfl_leagues,"mfl_league_ids.csv",quote = TRUE)


get_mfl_draft <- function(league_id,mfl_client){
  cli::cli_alert("League ID: {league_id}")
  cli::cli_alert("Now we sleep to not piss off MFL")
  Sys.sleep(3)
  conn <- mfl_connect(search_year, league_id, user_agent = mfl_client, rate_limit = TRUE, rate_limit_number = 30, rate_limit_seconds = 60)
  ff_draft(conn)
}

get_mfl_draft_json <- function(league_id) {
  
  cli::cli_alert("Getting JSON draft for League ID: {league_id}")
  Sys.sleep(1)
  api_url <-glue::glue("https://api.myfantasyleague.com/2024/export?TYPE=draftResults&L={league_id}&APIKEY=MFLRCLIENT&JSON=1")
  draft_json <- fromJSON(api_url, flatten =TRUE) |>
    as_tibble() |>
    tidyr::unnest_wider(1, names_sep = "_")|>
    #select(draftResults_draftPick)#|>
    unnest()|>
    mutate(league_id = league_id)|>
    select(league_id, franchise, player,comments,timestamp, round, pick)
  
}


get_mfl_draft_raw <- function(league_id) {
    cli::cli_alert("Getting endpoint draft for league ID: {league_id}")
    cli::cli_alert("Now we sleep to not piss off MFL")
    Sys.sleep(3)
    conn <- mfl_connect(search_year, league_id, user_agent = mfl_client, rate_limit = TRUE, rate_limit_number = 30, rate_limit_seconds = 60)
    mfl_getendpoint(conn, "draftResults")|>
    purrr::pluck("content","draftResults") |>
    tibble::tibble() |>
    tidyr::unnest_wider(1)
    
  }
  
  
  
  


# Seems way faster -- would need cached list of franchises and players though
#json_draft <- get_mfl_draft_json(55580)
#raw_draft <- get_mfl_draft_raw(55580)
#draft <- get_mfl_draft(11815, mfl_client)



# For Testing, subset leagues


#28764 is London live. Only MFL presumably until Monday



mfl_drafts <- mfl_leagues |>
  mutate(drafts = map2(league_id,mfl_client, possibly(get_mfl_draft, otherwise = tibble()))) |>
  unnest(drafts)











# Add interval between picks
mfl_drafts <- mfl_drafts |>
  mutate(
    player_id = as.character(player_id)
  ) |>
  group_by(league_id) |> # Note removed division here
  mutate(
        timestamp = as.POSIXct(timestamp, origin= "1970-01-01"),
        time_to_pick_int = interval(lag(timestamp), timestamp),
        time_to_pick = seconds(time_to_pick_int)
  )

# May have to group first? or Do it separately unless we can group within lag()

# Check for completed

completed <- mfl_drafts |>
  filter(!is.na(timestamp))|>
  group_by(league_name,league_id, league_home) |>
  summarise(last_pick = max(overall, na.rm=TRUE))|>
  filter(last_pick == 264)|>
  select(-last_pick, -league_name)

write_csv(completed, "completed_leagues.csv", append = TRUE)

completed_drafts <- completed |>
  left_join(mfl_drafts)

write_csv(completed_drafts, "completed_mfl_drafts.csv", append = TRUE)

#fwrite(divisions, "divisions_mfl.csv", quote = TRUE)
fwrite(mfl_drafts,"draft_picks_mfl.csv",quote = TRUE)
update_time <- format(Sys.time(), tz = "America/Toronto", usetz = TRUE)
writeLines(update_time, "timestamp.txt")

pb_upload("draft_picks_mfl.csv",
          repo = "mohanpatrick/sfb_14",
          tag = "data_mfl")
cli::cli_alert_success("Successfully uploaded to Git")


pb_upload("mfl_league_ids.csv",
          repo = "mohanpatrick/sfb_14",
          tag = "data_mfl")
cli::cli_alert_success("Successfully uploaded to Git")


pb_upload("completed_leagues.csv",
          repo = "mohanpatrick/sfb_14",
          tag = "data_mfl")
cli::cli_alert_success("Successfully uploaded to Git")

pb_upload("completed_mfl_drafts.csv",
          repo = "mohanpatrick/sfb_14",
          tag = "data_mfl")
cli::cli_alert_success("Successfully uploaded to Git")

cli::cli_alert_success("Successfully got all picks and ADP!")


