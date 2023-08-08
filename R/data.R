#' LoL Champion Data Set (Pooled)
#'
#' LoL champion data set pooled over regions, obtained by calling the \code{\link{construct_lol_champion_pooled_data}} function on the output of the
#' \code{\link{clean_lol_data}} function.
#'
#' @format
#' A data frame with the following variables:
#' \describe{
#'   \item{\code{region}}{Geographical macro area where server hosting the match is located.}
#'   \item{\code{day}}{Day when the match wad played.}
#'   \item{\code{day_no}}{Numeric version of \code{day}.}
#'   \item{\code{n_matches}}{How many matches where played in that \code{region} and \code{day}.}
#'   \item{\code{champion}}{Champion (unit of observation).}
#'   \item{\code{pick_level}}{How often \code{champion} was picked in that \code{region} and \code{day}.}
#'   \item{\code{pick_rate}}{Equals \code{pick_level} divided by \code{n_matches}.}
#'   \item{\code{ban_level}}{How often \code{champion} was banned in that \code{region} and \code{day}.}
#'   \item{\code{ban_rate}}{Equals \code{ban_level} divided by \code{n_matches}.}
#'   \item{\code{win_level}}{How matches \code{champion} has won in that \code{region} and \code{day}.}
#'   \item{\code{win_rate}}{Equals \code{win_level} divided by \code{pick_level}.}
#'   \item{\code{gold_avg}}{Average gold earned by \code{champion} in that \code{region} and \code{day}.}
#'   \item{\code{kills_avg}}{Average number of kills achieved by \code{champion} in that \code{region} and \code{day}.}
#'   \item{\code{assists_avg}}{Average number of assists achieved by \code{champion} in that \code{region} and \code{day}.}
#'   \item{\code{deaths_avg}}{Average number of deaths achieved by \code{champion} in that \code{region} and \code{day}.}
#'   \item{\code{main_role}}{Main role of \code{champion}.}
#'   \item{\code{aux_role}}{Auxiliary role of \code{champion}.}
#' }
"lol_champ_pool_dta"
