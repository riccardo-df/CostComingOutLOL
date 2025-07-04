% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clean-construct-data.R
\name{construct_lol_champion_pooled_data}
\alias{construct_lol_champion_pooled_data}
\title{Construct LoL Champion Pooled Data Set}
\usage{
construct_lol_champion_pooled_data(dta, filename = "lol_champ_pool_dta.csv")
}
\arguments{
\item{dta}{Data set as constructed by the \code{\link{construct_lol_champion_data}} function (you can find this data set already bundled in the package).}

\item{filename}{String to control the name of the output.}
}
\description{
Constructs the LoL champion pooled data set by aggregating observations over regions.
}
\details{
\code{\link{construct_lol_champion_pooled_data}} generates new variables by aggregating the observations (i.e., the champions) over the regions.\cr
\describe{
 \item{\code{pick_level_sum}, \code{ban_level_sum}, \code{win_level_sum}}{Sum the corresponding variables over regions. For instance, \code{pick_level_sum} is constructed by summing \code{pick_level} over Europe, Latin America, North America, and Korea, and tells us how many times \code{champion} has been picked on a given \code{day} overall.}
 \item{\code{n_matches_sum}}{Sum \code{n_matches} over regions. It tells us the total number of matches played in a given \code{day}.}
 \item{\code{pick_rate_pooled}, \code{ban_rate_pooled}, \code{win_rate_pooled}}{Divide the corresponding variables by \code{n_matches_sum}. Exception is \code{win_rate_pooled}, which is divided by \code{pick_level_sum}.}
 \item{\code{gold_pooled}, \code{kills_avg}, \code{assists_avg}, \code{deaths_avg}}{Average the corresponding variables over regions. We consider only days where \code{champion} has a non-zero \code{pick_level}.}
 \item{\code{main_role}}{For each \code{champion}, extract its main position.}
 \item{\code{aux_role}}{For each \code{champion}, extract its auxiliary position.}
}
}
\seealso{
\code{\link{pull_lol_data}}, \code{\link{clean_lol_data}}, \code{\link{construct_lol_champion_data}}, \code{\link{construct_lol_player_data}}
}
\author{
Riccardo Di Francesco
}
