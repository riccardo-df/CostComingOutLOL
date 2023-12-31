% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{lol_player_dta}
\alias{lol_player_dta}
\title{LoL Player Data Set}
\format{
A data frame with the following variables:
\describe{
  \item{\code{day}}{Day of obervation.}
  \item{\code{day_no}}{Numeric version of \code{day}.}
  \item{\code{id}}{Player PUIID (unit of observation).}
  \item{\code{n_matches}}{How many matches the player played in that \code{day}.}
  \item{\code{graves_rate}}{How often the player picked Graves in that \code{day}.}
  \item{\code{graves_ban_rate}}{How often the player banned Graves in that \code{day}.}
  \item{\code{top}}{How many times the player picked a champion whose main role is top lane.}
  \item{\code{jungle}}{How many times the player picked a champion whose main role is jungle.}
  \item{\code{mid}}{How many times the player picked a champion whose main role is middle lane.}
  \item{\code{bottom}}{How many times the player picked a champion whose main role is bottom lane.}
  \item{\code{support}}{How many times the player picked a champion whose main role is support.}
  \item{\code{lgb}}{How many times the player picked a one of Diana, Leona, Nami, or Neeko.}
  \item{\code{win_rate}}{How often the player won a match in that \code{day}.}
 \item{\code{gold_avg}}{Average gold earned by the player in that \code{day}.}
 \item{\code{kills_avg}}{Average number of kills achieved by the player in that \code{day}.}
 \item{\code{assists_avg}}{Average number of assists achieved by the player in that \code{day}.}
 \item{\code{deaths_avg}}{Average number of deaths achieved by the player in that \code{day}.}
}
}
\usage{
lol_player_dta
}
\description{
LoL champion data set, obtained by calling the \code{\link{construct_lol_player_data}} function on the output of the
\code{\link{clean_lol_data}} function.
}
\keyword{datasets}
