% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/main-analysis-helpers.R
\name{produce_plots_pooled}
\alias{produce_plots_pooled}
\title{Cost of Coming Out - Main Analysis}
\usage{
produce_plots_pooled(
  pooled_results,
  ylims = c(0, 100),
  palette = NULL,
  save_here = getwd()
)
}
\arguments{
\item{pooled_results}{Output of \code{\link{run_main_pooled}}}

\item{ylims}{Vector storing lower and upper limit for the y-axis.}

\item{palette}{String vector with hexadecimal codes. It controls the colors for the weight plot. If \code{NULL}, the weight plot is not produced.}

\item{save_here}{String denoting the path where to save the figures.}
}
\value{
Save some nice plots.
}
\description{
Produced plots displaying the results of the main analysis of the Cost of Coming Out paper performed by \code{\link{run_main_pooled}}.
}
\seealso{
\code{\link{produce_plots_regional}} \code{\link{produce_latex_pooled}} \code{\link{produce_latex_regional}}
}
\author{
Riccardo Di Francesco
}
