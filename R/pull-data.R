#' Pulling LoL Players
#'
#' Pulls names and IDs of all the players currently belonging to a given rank and division.
#'
#' @param server ID of the server of interest.
#' @param queue Queue type.
#' @param rank Rank of interest.
#' @param division Division of interest.
#' @param api_key Your API key. It must follow the format \code{"RGAPI-xxxx"}.
#'
#' @details
#' The following are the only valid values for the inputs:
#' \describe{
#'   \item{\code{server}}{\code{"br1"}, \code{"eun1"}, \code{"euw1"}, \code{"jp1"}, \code{"kr"}, \code{"la1"}, \code{"la2"}, \code{"na1"}, \code{"oc1"}, \code{"ru"}, \code{"tr1"}}
#'   \item{\code{queue}}{\code{"RANKED_SOLO_5x5"}, \code{"RANKED_TFT"}, \code{"RANKED_FLEX_SR"}, \code{"RANKED_FLEX_TT"}}
#'   \item{\code{rank}}{\code{"CHALLENGER"}, \code{"GRANDMASTER"}, \code{"MASTER"}, \code{"DIAMOND"}, \code{"PLATINUM"}, \code{"GOLD"}, \code{"SILVER"}, \code{"BRONZE"}, \code{"IRON"}}
#'   \item{\code{division}}{\code{"I"}, \code{"II"}, \code{"III"}, \code{"IV"}}
#' }
#'
#' To get an API key, you need a Riot account of at least level 5. Then, visit \href{https://developer.riotgames.com/}{https://developer.riotgames.com/} to generate a key.\cr
#'
#' @import dplyr httr jsonlite
#'
#' @author Riccardo Di Francesco
#'
#' @seealso \code{\link{pull_lol_match_ids}}, \code{\link{pull_lol_match_info}}, \code{\link{pull_lol_data}}
#'
#' @noRd
pull_lol_players <- function(server, queue, rank, division, api_key) {
  ## 0.) Checks.
  if (!(server %in% c("br1", "eun1", "euw1", "jp1", "kr", "la1", "la2", "na1", "oc1", "ru", "tr1"))) stop("Invalid 'server'. Check the documentation for valid values.", call. = FALSE)
  if (!(queue %in% c("RANKED_SOLO_5x5", "RANKED_TFT", "RANKED_FLEX_SR", "RANKED_FLEX_TT"))) stop("Invalid 'queue'. Check the documentation for valid values.", call. = FALSE)
  if (!(rank %in% c("CHALLENGER", "GRANDMASTER", "MASTER", "DIAMOND", "PLATINUM", "GOLD", "SILVER", "BRONZE", "IRON"))) stop("Invalid 'rank'. Check the documentation for valid values.", call. = FALSE)
  if (!(division %in% c("I", "II", "III", "IV"))) stop("Invalid 'division'. Check the documentation for valid values.", call. = FALSE)

  ## 1.) Construct url address of interest.
  base_url <- paste("https://", ".api.riotgames.com/lol/league-exp/v4/entries", sep = server)
  url <- paste(base_url, queue, rank, division, sep = "/")

  ## 2.) Get all summoners for given rank and tier.
  ## We do not know how many pages we need to loop over. So, we iterate across several pages and stop if the last page is empty.
  out <- list()
  counter <- 1

  for (i in seq_len(10000000L)) {
    page <- i

    url_page <- paste(url, page, sep = "?page=")
    url_page_key <- paste(url_page, api_key, sep = "&api_key=")

    summoners <- httr::GET(url_page_key)
    if (summoners$status_code == 403) stop("Your API key in invalid.", call. = FALSE)
    temp_dta <- jsonlite::fromJSON(rawToChar(summoners$content))

    if (is.null(dim(temp_dta)[1])) break

    out[[counter]] <- temp_dta[, c(3, 4, 5, 6)]
    counter <- counter + 1
  }

  ## 3.) output.
  out <- dplyr::bind_rows(out)
  colnames(out) <- c("rank", "division", "player_id", "player_name")

  ## Output.
  return(out)
}


#' Pulling LoL Match IDs
#'
#' Pulls the match IDs of a given player.
#'
#' @param server ID of the server of interest.
#' @param player_name The player's nickname.
#' @param api_key Your API key. It must follow the format \code{"RGAPI-xxxx"}.
#'
#' @details
#' The following are the valid values for the inputs:
#' \describe{
#'   \item{\code{server}}{\code{"br1"}, \code{"eun1"}, \code{"euw1"}, \code{"jp1"}, \code{"kr"}, \code{"la1"}, \code{"la2"}, \code{"na1"}, \code{"oc1"}, \code{"ru"}, \code{"tr1"}}
#' }
#'
#' To get an API key, you need a Riot account of at least level 5. Then, visit \href{https://developer.riotgames.com/}{https://developer.riotgames.com/} to generate a key.\cr
#'
#' @return
#' A vector of matches id recently played by \code{summoner_name}.\cr
#'
#' @import stringr anytime
#'
#' @author Riccardo Di Francesco
#'
#' @seealso \code{\link{pull_lol_players}}, \code{\link{pull_lol_match_info}}, \code{\link{pull_lol_data}}
#'
#' @noRd
pull_lol_match_ids <- function(server, player_name, api_key) {
  ## 0.) Handling inputs and checks.
  if (!(server %in% c("br1", "eun1", "euw1", "jp1", "kr", "la1", "la2", "na1", "oc1", "ru", "tr1"))) stop("Invalid 'server'. Check the documentation for valid values.", call. = FALSE)
  if (server == "euw1" | server == "eun1" | server == "ru" | server == "tr1") server_adjusted = "europe"
  if (server == "br1" | server == "la1" | server == "la2" | server == "na1") server_adjusted = "americas"
  if (server == "jp1" | server == "kr") server_adjusted = "asia"
  if (server == "oc1") server_adjusted = "sea"

  player_name_adjusted <- stringr::str_replace_all(player_name, pattern = " ", replacement = "%20")

  ## 1.) Get summoner's puuid.
  base_url_summoner <- paste("https://", ".api.riotgames.com/lol/summoner/v4/summoners/by-name", sep = server)
  url_summoner <- paste(base_url_summoner, player_name_adjusted, sep = "/")
  url_summoner_key <- paste(url_summoner, api_key, sep = "?api_key=")

  summoner <- httr::GET(url_summoner_key)
  summoner_data <- jsonlite::fromJSON(rawToChar(summoner$content))
  summoner_puuid <- summoner_data$puuid

  if (summoner$status_code == 403) stop("Your API key in invalid.", call. = FALSE)

  ## 2.) Get matches IDs using the summoner's puuid.
  ## We do not know how many pages we need to loop over. So, we iterate across several start points and stop if the last page is empty or if last iteration scraped the last ids.
  start <- c(0, seq(101, 10000000, by = 100))
  matches_id <- list()
  counter <- 1

  for (i in start) {
    base_url_matches <- paste("https://", ".api.riotgames.com/lol/match/v5/matches/by-puuid", sep = server_adjusted)
    url_matches <- paste(base_url_matches, summoner_puuid, sep = "/")
    url_matches_start <- paste(url_matches, "/ids?start=", i, "&count=100", sep = "")
    url_matches_key <- paste(url_matches_start, api_key, sep = "&api_key=")

    matches <- httr::GET(url_matches_key)
    new_matches_id <- jsonlite::fromJSON(rawToChar(matches$content))

    if (is.list(new_matches_id)) break

    matches_id[[counter]] <- new_matches_id
    counter <- counter + 1

    if (length(new_matches_id) < 100) break
  }

  ## 3.) Output.
  out <- unlist(matches_id)
  return(out)
}


#' Pulling LoL Match Information
#'
#' Pulls information about the match of interest.
#'
#' @param server ID of the server of interest.
#' @param match_id The match ID of interest.
#' @param api_key Your API key. It must follow the format \code{"RGAPI-xxxx"}.
#' @param min_sec Wait for a random amount of seconds bounded from below by \code{min_sec} before pulling next item.
#' @param max_sec Wait for a random amount of seconds bounded from above by \code{max_sec} before pulling next item.
#'
#' @details
#' The following are the valid values for the inputs:
#' \describe{
#'   \item{\code{server}}{\code{"br1"}, \code{"eun1"}, \code{"euw1"}, \code{"jp1"}, \code{"kr"}, \code{"la1"}, \code{"la2"}, \code{"na1"}, \code{"oc1"}, \code{"ru"}, \code{"tr1"}}
#' }
#'
#' Only classic games are returned.\cr
#'
#' To get an API key, you need a Riot account of at least level 5. Then, visit \href{https://developer.riotgames.com/}{https://developer.riotgames.com/} to generate a key.\cr
#'
#' Imposing a waiting time using \code{min_sec} and \code{max_sec} between each request is useful to avoid hitting the rate limits imposed by Riot. With a personal API key, we can execute 20 requests
#' every 1 second and 100 requests every 2 minutes. To use a deterministic amount of time, set \code{min_sec} and \code{max_sec} to the same number.
#'
#' @return
#' A data frame with the following variables:
#' \describe{
#'   \item{\code{match_id}}{Match ID.}
#'   \item{\code{day}}{Time and date of the beginning of the match.}
#'   \item{\code{duration}}{Duration of the match in minutes.}
#'   \item{\code{queue_id}}{Queue ID. See \href{https://static.developer.riotgames.com/docs/lol/queues.json}{here}. Currently, only 5v5 Ranked Solo games are returned.}
#'   \item{\code{game_mode}}{Game mode. See \href{https://static.developer.riotgames.com/docs/lol/gameModes.json}{here}. Currently, only \code{"CLASSIC"} matches are returned.}
#'   \item{\code{game_type}}{Game type. See \href{https://static.developer.riotgames.com/docs/lol/gameTypes.json}{here}. Currently, only \code{"MATCHED_GAME"} matches are returned.}
#'   \item{\code{player_name}}{Name of each player in the match.}
#'   \item{\code{player_id}}{ID of each player in the match.}
#'   \item{\code{player_puuid}}{PUIID of each player in the match.}
#'   \item{\code{player_level}}{Level of each player in the match.}
#'   \item{\code{champion}}{Champion picked by each player in the match.}
#'   \item{\code{position}}{Position played by each player in the match.}
#'   \item{\code{kills}}{Number of kills of each player in the match.}
#'   \item{\code{assists}}{Number of assists of each player in the match.}
#'   \item{\code{deaths}}{Number of deaths of each player in the match.}
#'   \item{\code{gold}}{Gold earned by each player in the match.}
#'   \item{\code{early_surrender}}{Whether the match ended by early surrender.}
#'   \item{\code{surrender}}{Whether the match ended by surrender.}
#'   \item{\code{win}}{Whether the player belongs to the winning team.}
#'   \item{\code{ban}}{Champions banned for a given game.}
#' }
#'
#' For more details on these variables, please refer to Riot's official documentation:
#' \href{https://developer.riotgames.com/apis#summoner-v4/GET_getBySummonerName}{https://developer.riotgames.com/apis#summoner-v4/GET_getBySummonerName},
#' \href{https://developer.riotgames.com/apis#match-v5/GET_getMatchIdsByPUUID}{https://developer.riotgames.com/apis#match-v5/GET_getMatchIdsByPUUID}, and
#' \href{https://developer.riotgames.com/apis#match-v5/GET_getMatch}{https://developer.riotgames.com/apis#match-v5/GET_getMatch}.
#'
#' @import stringr anytime
#'
#' @author Riccardo Di Francesco
#'
#' @seealso \code{\link{pull_lol_players}}, \code{\link{pull_lol_match_ids}}, \code{\link{pull_lol_data}}
#'
#' @noRd
pull_lol_match_info <- function(server, match_id, api_key,
                                min_sec = 0, max_sec = 0) {
  ## 0.) Handling inputs and checks.
  if (!(server %in% c("br1", "eun1", "euw1", "jp1", "kr", "la1", "la2", "na1", "oc1", "ru", "tr1"))) stop("Invalid 'server'. Check the documentation for valid values.", call. = FALSE)
  if (server == "euw1" | server == "eun1" | server == "ru" | server == "tr1") server_adjusted = "europe"
  if (server == "br1" | server == "la1" | server == "la2" | server == "na1") server_adjusted = "americas"
  if (server == "jp1" | server == "kr") server_adjusted = "asia"
  if (server == "oc1") server_adjusted = "sea"

  sleep(min_sec, max_sec)

  ## 1.) Get list with match data.
  base_url_match <- paste("https://", ".api.riotgames.com/lol/match/v5/matches", sep = server_adjusted)
  url_match <- paste(base_url_match, match_id, sep = "/")
  url_match_key <- paste(url_match, api_key, sep = "?api_key=")

  match <- httr::GET(url_match_key)
  match_data <- jsonlite::fromJSON(rawToChar(match$content))

  if (match$status_code != 200) return(NULL)

  ## 2.) Extract relevant information.
  # 2a.) Generics.
  when <- anytime::anytime(match_data$info$gameCreation / 1000)
  duration <- match_data$info$gameDuration / 60
  queue_id <- match_data$info$queueId
  game_mode <- match_data$info$gameMode
  game_type <- match_data$info$gameType

  if (queue_id != 420 | game_mode != "CLASSIC" | game_type != "MATCHED_GAME") return(NULL)

  # 2b.) Players.
  player_name <- match_data$info$participants$summonerName
  player_id <- match_data$info$participants$summonerId
  player_puiid <- match_data$info$participants$puuid
  player_level <- match_data$info$participants$summonerLevel

  if (length(player_name) != 10 | length(player_id) != 10 | length(player_puiid) != 10 | length(player_level) != 10) return(NULL)

  champion <- match_data$info$participants$championName
  ban <- c(match_data$info$teams$bans[[1]]$championId, match_data$info$teams$bans[[2]]$championId)
  position <- match_data$info$participants$teamPosition

  if (length(champion) != 10 |length(ban) != 10 | length(position) != 10) return(NULL)

  kills <- match_data$info$participants$kills
  assists <- match_data$info$participants$assists
  deaths <- match_data$info$participants$deaths
  gold <- match_data$info$participants$goldEarned

  if (length(kills) != 10 | length(assists) != 10 | length(deaths) != 10 | length(gold) != 10) return(NULL)

  early_surrender <- match_data$info$participants$gameEndedInEarlySurrender
  surrender <- match_data$info$participants$gameEndedInSurrender
  win <- match_data$info$participants$win

  if (length(early_surrender) != 10 | length(surrender) != 10 |length(win) != 10) return(NULL)

  ## Output.
  out <- data.frame("match_id" = match_id, "day" = when, "duration" = duration,
                    "queue_id" = queue_id, "game_mode" = game_mode, "game_type" = game_type,
                    player_name, player_id, player_puiid, player_level,
                    position, champion, ban, kills, assists, deaths, gold,
                    early_surrender, surrender, win)
  return(out)
}


#' Pull LoL Data
#'
#' Pulls raw data from Riot API.
#'
#' @param server ID of the server of interest.
#' @param queue Queue type.
#' @param rank Rank of interest.
#' @param division Division of interest.
#' @param api_key Your API key. It must follow the format \code{"RGAPI-xxxx"}.
#' @param min_sec Wait for a random amount of seconds bounded from below by \code{min_sec} before pulling next item.
#' @param max_sec Wait for a random amount of seconds bounded from above by \code{max_sec} before pulling next item.
#'
#' @details
#' The following are the only valid values for the inputs:
#' \describe{
#'   \item{\code{server}}{\code{"br1"}, \code{"eun1"}, \code{"euw1"}, \code{"jp1"}, \code{"kr"}, \code{"la1"}, \code{"la2"}, \code{"na1"}, \code{"oc1"}, \code{"ru"}, \code{"tr1"}}
#'   \item{\code{queue}}{\code{"RANKED_SOLO_5x5"}, \code{"RANKED_TFT"}, \code{"RANKED_FLEX_SR"}, \code{"RANKED_FLEX_TT"}}
#'   \item{\code{rank}}{\code{"CHALLENGER"}, \code{"GRANDMASTER"}, \code{"MASTER"}, \code{"DIAMOND"}, \code{"PLATINUM"}, \code{"GOLD"}, \code{"SILVER"}, \code{"BRONZE"}, \code{"IRON"}}
#'   \item{\code{division}}{\code{"I"}, \code{"II"}, \code{"III"}, \code{"IV"}}
#' }
#'
#' To get an API key, you need a Riot account of at least level 5. Then, visit \href{https://developer.riotgames.com/}{https://developer.riotgames.com/} to generate a key.\cr
#'
#' For more details on the output, please refer to Riot's official documentation:
#' \href{https://developer.riotgames.com/apis#summoner-v4/GET_getBySummonerName}{https://developer.riotgames.com/apis#summoner-v4/GET_getBySummonerName},
#' \href{https://developer.riotgames.com/apis#match-v5/GET_getMatchIdsByPUUID}{https://developer.riotgames.com/apis#match-v5/GET_getMatchIdsByPUUID}, and
#' \href{https://developer.riotgames.com/apis#match-v5/GET_getMatch}{https://developer.riotgames.com/apis#match-v5/GET_getMatch}.
#'
#' @return
#' Prints a csv file storing a data set with the following columns:
#' \describe{
#'   \item{\code{match_id}}{Match ID.}
#'   \item{\code{day}}{Time and date of the beginning of the match.}
#'   \item{\code{duration}}{Duration of the match in minutes.}
#'   \item{\code{queue_id}}{Queue ID. See \href{https://static.developer.riotgames.com/docs/lol/queues.json}{here}. Currently, only 5v5 Ranked Solo games are returned.}
#'   \item{\code{game_mode}}{Game mode. See \href{https://static.developer.riotgames.com/docs/lol/gameModes.json}{here}. Currently, only \code{"CLASSIC"} matches are returned.}
#'   \item{\code{game_type}}{Game type. See \href{https://static.developer.riotgames.com/docs/lol/gameTypes.json}{here}. Currently, only \code{"MATCHED_GAME"} matches are returned.}
#'   \item{\code{player_name}}{Name of each player in the match.}
#'   \item{\code{player_id}}{ID of each player in the match.}
#'   \item{\code{player_puuid}}{PUIID of each player in the match.}
#'   \item{\code{player_level}}{Level of each player in the match.}
#'   \item{\code{champion}}{Champion picked by each player in the match.}
#'   \item{\code{position}}{Position played by each player in the match.}
#'   \item{\code{kills}}{Number of kills of each player in the match.}
#'   \item{\code{assists}}{Number of assists of each player in the match.}
#'   \item{\code{deaths}}{Number of deaths of each player in the match.}
#'   \item{\code{gold}}{Gold earned by each player in the match.}
#'   \item{\code{early_surrender}}{Whether the match ended by early surrender.}
#'   \item{\code{surrender}}{Whether the match ended by surrender.}
#'   \item{\code{win}}{Whether the player belongs to the winning team.}
#'   \item{\code{ban}}{Champions banned for a given game.}
#' }
#'
#' @import dplyr httr jsonlite utils
#'
#' @author Riccardo Di Francesco
#'
#' @seealso \code{\link{pull_lol_match_ids}}, \code{\link{pull_lol_match_info}}
#'
#' @export
pull_lol_data <- function(server, queue, rank, division, api_key, min_sec, max_sec) {
  ## 0.) Handling inputs and checks.
  ## Not necessary, these are done within the functions called below.

  ## 1.) Pull players' nicknames.
  cat("Pulling players' nicknames. \n")
  players <- pull_lol_players(server, queue, rank, division, api_key)

  ## 2.) Loop over nicknames to get information about players' matches.
  player_names <- players$player_name

  raw_results <- list()
  counter_player <- 1
  counter_storage <- 1

  for (player in player_names) {
    match_ids <- pull_lol_match_ids(server, player, api_key)
    counter_match <- 1

    for (match_id in match_ids) {
      cat(paste0("   Fetching match ", counter_match, "/", length(match_ids), " Player ", player, " ", counter_player, "/", length(player_names), "\n"))

      raw_results[[counter_storage]] <- pull_lol_match_info(server, match_id, api_key, min_sec, max_sec)

      counter_match <- counter_match + 1
      counter_storage <- counter_storage + 1
    }

    counter_player <- counter_player + 1
  }

  results <- dplyr::bind_rows(raw_results)
  results$rank <- rank
  results$division <- division

  ## 3.) Write csv file.
  cat("Writing csv file at ", getwd(), ". \n\n", sep = "")
  file_name <- paste0("lol_", rank, division, "_", server, ".csv")
  utils::write.csv(results, file = file_name, row.names = FALSE)
}
