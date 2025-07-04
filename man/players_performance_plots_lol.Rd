% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mechanism.R
\name{players_performance_plots_lol}
\alias{players_performance_plots_lol}
\title{LoL Players' Performance Plots}
\usage{
players_performance_plots_lol(
  n_pre_matches,
  treatment_date = as.POSIXct("2022-06-01", tryFormats = "\%Y-\%m-\%d"),
  save_here = getwd()
)
}
\arguments{
\item{n_pre_matches}{How many matches before \code{treatment_date} players must have played to be kept in the data set.}

\item{treatment_date}{Object of class \code{POSIXct}. When the treatment took place.}

\item{save_here}{String denoting the path where to save the figures.}
}
\value{
Produces nice plots.
}
\description{
Divides players into two groups according to whether they never picked Graves pre-treatment (prior and non-prior users) and produces plots showing the average performance of players
in each group before and after the treatment. It also produces a plot to investigate which positions are the most played in each group before and after the treatment. The same
plot is then repeated only for those prior users that are "treated" as discussed in the documentation of the \code{\link{did_players_performance}}.
}
\details{
\code{treatment_date} must be created by \code{as.POSIXct("YYYY-MM-DD", tryFormats = "\%Y-\%m-\%d")}.\cr

Players that have played less than \code{n_pre_matches} before \code{treatment_date} or that never played after are dropped. The number of players remaining in the data set is printed in the console.
}
\author{
Riccardo Di Francesco
}
