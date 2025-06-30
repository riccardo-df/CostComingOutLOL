## @author: riccardo-df
## University of Southern Denmark
## 19\06\2025

## The Cost of Coming Out - Pulling data from Twitch API.

## This script pulls raw data from Twitch API.

# Preliminaries -----------------------------------------------------------
rm(list = ls())
set.seed(1986)
start <- Sys.time()

## Loading packages.
pkgs <- c("dplyr", "twitchr") # CostComingOutLOL
devtools::load_all()
inst <- lapply(pkgs, library, character.only = TRUE)

# Pull videos -------------------------------------------------------------
## Extract players' nicknames.
players <- lol_player_dta %>%
  pull(nick) %>%
  unique()

## Authentication.
twitch_auth()

## Loop over players.
video_dta <- list()
counter_player <- 1

for (this_player in players) {
  ## Print loop info.
  cat("Streamer ", this_player, " (", counter_player, "/", length(players), ") \n", sep = "")

  ## Get the user ID of the streamer.
  user <- suppressWarnings(get_users(login = this_player))

  if (is.null(user)) {
    cat("    Skipping... \n")
    counter_player <- counter_player + 1
    next
  }

  ## Pre-allocate memory.
  this_player_videos <- list()
  cursor <- NULL
  page <- 1

  ## Pull videos for this player.
  repeat {
    ## Print loop info.
    cat("    Fetching page ", page, "...\n", sep = "")

    ## Call API.
    res <- suppressWarnings(get_videos(user_id = user$id, first = 100, after = cursor))
    if (is.null(res)) {
      cat("    Skipping... \n")
      break
    }
    this_player_videos[[page]] <- res$data

    ## Update cursor for next page.
    cursor <- res$pagination
    if (is.null(cursor)) break


    page <- page + 1
  }

  ## Combine all results for this player into a single data frame and save.
  this_player_videos <- bind_rows(this_player_videos)
  video_dta[[counter_player]] <- this_player_videos
  counter_player <- counter_player + 1
}

## Bind results.
video_dta <- bind_rows(video_dta)

# Final operations --------------------------------------------------------
## Export data set.
write.csv(video_dta, "twitch_dta.csv")

## Print computational time.
Sys.time() - start

