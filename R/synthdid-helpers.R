#' Regularized Synthetic Control
#'
#' Synthetic control with a ridge penalty, as defined in Algorithm 1 of Arkhangelsky et al. (2021). Taken from
#' \href{https://synth-inference.github.io/synthdid/articles/paper-results.html}{https://synth-inference.github.io/synthdid/articles/paper-results.html}.
#'
#' @param Y Observation matrix.
#' @param N0 Number of control units (control units are in the rows 1-N0 of Y).
#' @param T0 Number of pre-treatment periods (they correspond to columns 1-T0 of Y).
#' @param X Optional 3D array of time-varying covariates. Shape should be N X T X C for C covariates.
#' @param eta.omega Determines the level of ridge regularization. Default corresponds to the level used in the paper. Total regularization equals \code{eta.omega} times the standard deviation of the first differences of \code{Y}.
#'
#' @import dplyr ggplot2
#'
#' @author Riccardo Di Francesco
#'
#' @seealso \code{\link{pull_lol_data}}, \code{\link{construct_lol_champion_data}}, \code{\link{construct_lol_champion_pooled_data}}, \code{\link{\code{\link{construct_lol_player_data}}}}
#'
#' @export
sc_estimate_reg <- function(Y, N0, T0, X = array(dim = c(dim(Y), 0))) {
  sc_estimate(Y, N0, T0, X = X, eta.omega = ((nrow(Y)-N0)*(ncol(Y)-T0))^(1/4))
}


#' Synthetic Outcome
#'
#' Constructs the synthetic outcome using the estimation results stored in an \code{\link[synthdid]{synthdid_estimate}} object.
#'
#' @param object, An \code{\link[synthdid]{synthdid_estimate}} object.
#' @param dta Data set used to estimate \code{object}.
#' @param unit_colname Name of the column of \code{dta} storing the units of observation.
#' @param outcome_colname Name of the column of \code{dta} storing the outcome of interest.
#' @param time_colname Name of the column of \code{dta} storing the time variable.
#'
#' @return
#' A list with the following elements:
#' \describe{
#' \item{weights}{An (nw)x2 tibble (with nw = number of non-zero estimated weights), with one column for the identities and one column for the contributions of units in the donor pool with a non-zero estimated weight.}
#' \item{synth_outcome}{An Tx2 tibble, with one column for the time periods and one column for the synthetic outcome.}
#' }
#'
#' @author Riccardo Di Francesco
#'
#' @seealso \code{\link{actual_controls}}, \code{\link{synth_outcome}}
#'
#' @import dplyr
#'
#' @export
construct_synth_outcome <- function(object, dta, unit_colname, outcome_colname, time_colname) {
  ## 0.) Handling inputs and checks.
  if (!any(c(unit_colname, outcome_colname, time_colname) %in% colnames(dta))) stop("One or more of 'unit_colname', 'outcome_colname', and 'time_colname' are not found in 'dta'.", call. = FALSE)
  if(!inherits(object, "synthdid_estimate")) stop("Invalid 'object'. This must be of class 'synthdid_estimate'.", call. = FALSE)

  ## 1.) Extract non-zero weights and corresponding units.
  weights <- dplyr::as_tibble(data.frame("unit" = rownames(synthdid_controls(object)),
                                         "weight" = synthdid_controls(object)[, 1]))
  colnames(weights)[1] <- unit_colname

  ## 2.) Get outcomes over time for the actual contributors.
  contributors <- weights[, 1] %>%
    pull()

  outcomes_and_weights <- dta %>%
    dplyr::filter(!!as.symbol(unit_colname) %in% contributors) %>%
    dplyr::left_join(weights, by = unit_colname) %>%
    dplyr::select(all_of(c(time_colname, unit_colname, outcome_colname)), "weight")

  ## 3.) Construct synthetic outcome as weighted mean of actual outcomes.
  synth_outcome_results <- outcomes_and_weights %>%
    dplyr::group_by_at(time_colname) %>%
    dplyr::mutate(synth_outcome = stats::weighted.mean(get(outcome_colname), weight)) %>%
    dplyr::select(all_of(time_colname), "synth_outcome") %>%
    dplyr::distinct()

  ## 4.) Output.
  return(list("weights" = weights, "synth_outcome" = synth_outcome_results))
}
