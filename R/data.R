#' LoL Champion Data Set
#'
#' LoL champion data set, obtained by calling the \code{\link{construct_lol_champion_data}} function on the output of the
#' \code{\link{clean_lol_data}} function.
#'
#' @format
#' A data frame with the following variables:
#' \describe{
#'   \item{\code{region}}{Geographical macro area of observation.}
#'   \item{\code{day}}{Day of observation.}
#'   \item{\code{day_no}}{Numeric version of \code{day}.}
#'   \item{\code{n_matches}}{How many matches where played in that \code{region} and \code{day}.}
#'   \item{\code{champion}}{Champion (unit of observation).}
#'   \item{\code{pick_level}}{How many times \code{champion} was picked in that \code{region} and \code{day}.}
#'   \item{\code{pick_rate}}{Proportion of times \code{champion} was picked in that \code{region} and \code{day} (equals \code{pick_level} divided by \code{n_matches}).}
#'   \item{\code{ban_level}}{How many times \code{champion} was banned in that \code{region} and \code{day}.}
#'   \item{\code{ban_rate}}{Proportion of times \code{champion} was banned in that \code{region} and \code{day} (equals \code{ban_level} divided by \code{n_matches}).}
#'   \item{\code{win_level}}{How many matches \code{champion} has won in that \code{region} and \code{day}.}
#'   \item{\code{win_rate}}{Proportion of matches won by \code{champion} in that \code{region} and \code{day} (equals \code{win_level} divided by \code{pick_level}).}
#'   \item{\code{gold_avg}}{Average gold earned by \code{champion} in that \code{region} and \code{day}.}
#'   \item{\code{kills_avg}}{Average number of kills achieved by \code{champion} in that \code{region} and \code{day}.}
#'   \item{\code{assists_avg}}{Average number of assists achieved by \code{champion} in that \code{region} and \code{day}.}
#'   \item{\code{deaths_avg}}{Average number of deaths achieved by \code{champion} in that \code{region} and \code{day}.}
#'   \item{\code{main_role}}{Main role of \code{champion}.}
#'   \item{\code{aux_role}}{Auxiliary role of \code{champion}.}
#' }
"lol_champ_dta"


#' LoL Champion Data Set (Pooled)
#'
#' LoL champion data set, obtained by calling the \code{\link{construct_lol_champion_pooled_data}} function on the output of the
#' \code{\link{construct_lol_champion_data}} function.
#'
#' @format
#' A data frame with the following variables:
#' \describe{
#'   \item{\code{day}}{Day of observation.}
#'   \item{\code{day_no}}{Numeric version of \code{day}.}
#'   \item{\code{n_matches_sum}}{How many matches where played in that \code{day}.}
#'   \item{\code{champion}}{Champion (unit of observation).}
#'   \item{\code{pick_level_sum}}{How many times \code{champion} was picked in that \code{day}.}
#'   \item{\code{pick_rate_pooled}}{Proportion of times \code{champion} was picked in that \code{day} (equals \code{pick_level_sum} divided by \code{n_matches_sum}).}
#'   \item{\code{ban_level_sum}}{How many times \code{champion} was banned in that \code{day}.}
#'   \item{\code{ban_rate_pooled}}{Proportion of times \code{champion} was banned in that \code{day} (equals \code{ban_level_sum} divided by \code{n_matches_sum}).}
#'   \item{\code{win_level_sum}}{How many matches \code{champion} has won in that \code{day}.}
#'   \item{\code{win_rate_pooled}}{Proportion of matches won by \code{champion} in that \code{day} (equals \code{win_level_sum} divided by \code{pick_level_sum}).}
#'   \item{\code{gold_pooled}}{Average gold earned by \code{champion} in that \code{day} among the matches it participated.}
#'   \item{\code{kills_pooled}}{Average number of kills achieved by \code{champion} in that \code{day} (zeroes, i.e., days where \code{champion} was never picked, are not included in the computation).}
#'   \item{\code{assists_pooled}}{Average number of assists achieved by \code{champion} in that \code{day} (zeroes, i.e., days where \code{champion} was never picked, are not included in the computation).}
#'   \item{\code{deaths_pooled}}{Average number of deaths achieved by \code{champion} in that \code{day} (zeroes, i.e., days where \code{champion} was never picked, are not included in the computation).}
#'   \item{\code{main_role}}{Main role of \code{champion}.}
#'   \item{\code{aux_role}}{Auxiliary role of \code{champion}.}
#' }
"lol_champ_pool_dta"


#' LoL Player Data Set
#'
#' LoL champion data set, obtained by calling the \code{\link{construct_lol_player_data}} function on the output of the
#' \code{\link{clean_lol_data}} function.
#'
#' @format
#' A data frame with the following variables:
#' \describe{
#'   \item{\code{day}}{Day of obervation.}
#'   \item{\code{day_no}}{Numeric version of \code{day}.}
#'   \item{\code{id}}{Player PUIID (unit of observation).}
#'   \item{\code{n_matches}}{How many matches the player played in that \code{day}.}
#'   \item{\code{graves_rate}}{How often the player picked Graves in that \code{day}.}
#'   \item{\code{graves_ban_rate}}{How often the player banned Graves in that \code{day}.}
#'   \item{\code{win_rate}}{How often the player won a match in that \code{day}.}
#'  \item{\code{gold_avg}}{Average gold earned by the player in that \code{day}.}
#'  \item{\code{kills_avg}}{Average number of kills achieved by the player in that \code{day}.}
#'  \item{\code{assists_avg}}{Average number of assists achieved by the player in that \code{day}.}
#'  \item{\code{deaths_avg}}{Average number of deaths achieved by the player in that \code{day}.}
#' }
"lol_player_dta"
