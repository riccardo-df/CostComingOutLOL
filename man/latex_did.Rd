% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mechanism.R
\name{latex_did}
\alias{latex_did}
\title{Diff-in-Diff Table}
\usage{
latex_did(did_results, seed = 1986)
}
\arguments{
\item{did_results}{The results of \code{\link{did_players_performance}}.}

\item{seed}{To make results reproducible.}
}
\value{
Prints LATEX code.
}
\description{
Prints LATEX code for a nice table displaying the aggregated results of \code{\link{did_players_performance}}.
}
\details{
To summarize results, we report an average of the estimated ATT(t) for all t greater than \code{treatment_date} used when calling \code{\link{did_players_performance}}.
}
\author{
Riccardo Di Francesco
}
