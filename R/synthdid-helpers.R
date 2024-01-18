#' Construct Donor Pool
#'
#' Subsets the provided data to include only the champion under investigation and the desired units in the donor pool.
#'
#' @param dta One of the champion data sets bundled in the package (\code{\link{lol_champ_pool_dta}} or \code{\link{lol_champ_dta}}).
#' @param donors Which units to include in the donor pool. See the details section below.
#' @param my_champion Champion under investigation. We need to include it as well.
#'
#' @return
#' \code{dta} filtered to contain only the champion under investigation and the desired donors.
#'
#' @details
#' \code{donors} must be a character vector with one or more champions contained in \code{dta}. Alternatively, one of the following special strings can be used:
#'
#' \itemize{
#'    \item{"all" }{This includes all champions.}
#'    \item{"non_lgb" }{This includes all champions expect Graves, Twisted Fate, Diana, Leona, Nami, and Neeko.}
#'    \item{"jungle" }{This includes all champions whose main role is Jungle.}
#'    \item{"middle" }{This includes all champions whose main role is Middle.}
#'    \item{"top" }{This includes all champions whose main role is Top.}
#'    \item{"support" }{This includes all champions whose main role is Support.}
#'    \item{"adc" }{This includes all champions whose main role is Adc.}
#'    \item{"main_role"}{This includes all champions whose main role is the same as the champion of interest.}
#'    \item{"aux_role"}{This includes all champions whose auxiliary role is the same as the champion of interest.}
#' }
#'
#' Almost all of these choices (exceptions are \code{"all"} and \code{"non_lgb"}) can be combined with the prefix \code{"non"} to exclude champions from a particular role.
#' For instance, setting \code{donors} to \code{non_jungle} excludes from the donor pool all champions whose main role is jungle.
#'
#' @import dplyr
#'
#' @author Riccardo Di Francesco
#'
#' @keywords internal
construct_donor_pool <- function(dta, donors, my_champion) {
  ## 0.) Handling inputs and checks.
  champion <- NULL
  main_role <- NULL

  if (length(donors) == 1) {
    if (!(donors %in% c("all", "non_lgb", "main_role", "aux_role", "jungle", "middle", "top", "support", "adc", "support_adc", "top_jungle_middle", "non_main_role", "non_aux_role", "non_jungle", "non_middle", "non_top", "non_support", "non_adc"))) stop("Invalid 'donors'. Call 'help(run_main_pooled)' to check valid inputs.", call. = FALSE)
  } else {
    if (sum(!(donors %in% unique(dta$champion))) > 0) stop("Invalid 'donors'. One or more champions are not found in 'dta'.", call. = FALSE)
  }

  ## 2.) Subset dta.
  if (length(donors) == 1) {
    if (donors == "all") {
      my_subset <- dta
    } else if (donors == "non_lgb") {
      lgb_champions <- c("Graves", "TwistedFate", "Nami", "Leona", "Diana", "Neeko")
      exclude_these <- lgb_champions[lgb_champions != my_champion]

      my_subset <- dta %>%
        dplyr::filter(!(champion %in% exclude_these))
    } else if (donors == "main_role") {
      temp_main_role <- dta %>%
        dplyr::filter(champion == my_champion) %>%
        dplyr::pull(main_role) %>%
        unique()

      my_subset <- dta %>%
        dplyr::filter(main_role == temp_main_role | champion == my_champion)
    } else if (donors == "aux_role") {
      temp_aux_role <- dta %>%
        dplyr::filter(champion == my_champion) %>%
        dplyr::pull(aux_role) %>%
        unique()

      my_subset <- dta %>%
        dplyr::filter(main_role == temp_aux_role | champion == my_champion)
    } else if (donors == "jungle") {
      my_subset <- dta %>%
        dplyr::filter(main_role == "JUNGLE" | champion == my_champion)
    } else if (donors == "middle") {
      my_subset <- dta %>%
        dplyr::filter(main_role == "MIDDLE" | champion == my_champion)
    } else if (donors == "top") {
      my_subset <- dta %>%
        dplyr::filter(main_role == "TOP" | champion == my_champion)
    } else if (donors == "support") {
      my_subset <- dta %>%
        dplyr::filter(main_role == "UTILITY" | champion == my_champion)
    } else if (donors == "adc") {
      my_subset <- dta %>%
        dplyr::filter(main_role == "BOTTOM" | champion == my_champion)
    } else if (donors == "support_adc") {
      my_subset <- dta %>%
        dplyr::filter(main_role %in% c("UTILITY", "BOTTOM") | champion == my_champion)
    } else if (donors == "top_jungle_middle") {
      my_subset <- dta %>%
        dplyr::filter(main_role %in% c("TOP", "JUNGLE", "MIDDLE") | champion == my_champion)
    } else if (donors == "non_main_role") {
      temp_main_role <- dta %>%
        dplyr::filter(champion == my_champion) %>%
        dplyr::pull(main_role) %>%
        unique()

      my_subset <- dta %>%
        dplyr::filter(main_role != temp_main_role | champion == my_champion)
    } else if (donors == "non_aux_role") {
      temp_aux_role <- dta %>%
        dplyr::filter(champion == my_champion) %>%
        dplyr::pull(aux_role) %>%
        unique()

      my_subset <- dta %>%
        dplyr::filter(main_role != temp_aux_role | champion == my_champion)
    } else if (donors == "non_jungle") {
      my_subset <- dta %>%
        dplyr::filter(main_role != "JUNGLE" | champion == my_champion)
    } else if (donors == "non_middle") {
      my_subset <- dta %>%
        dplyr::filter(main_role != "MIDDLE" | champion == my_champion)
    } else if (donors == "non_top") {
      my_subset <- dta %>%
        dplyr::filter(main_role != "TOP" | champion == my_champion)
    } else if (donors == "non_support") {
      my_subset <- dta %>%
        dplyr::filter(main_role != "UTILITY" | champion == my_champion)
    } else if (donors == "non_adc") {
      my_subset <- dta %>%
        dplyr::filter(main_role != "BOTTOM" | champion == my_champion)
    }
  } else {
    my_subset <- dta %>%
      dplyr::filter(champion %in% c(donors, my_champion))
  }

  ## 3.) Output.
  return(my_subset)
}


#' Regularized Synthetic Control
#'
#' Synthetic control with a ridge penalty, as defined in Algorithm 1 of Arkhangelsky et al. (2021). Taken from
#' \href{https://synth-inference.github.io/synthdid/articles/paper-results.html}{https://synth-inference.github.io/synthdid/articles/paper-results.html}.
#'
#' @param Y Observation matrix.
#' @param N0 Number of control units (control units are in the rows 1-N0 of Y).
#' @param T0 Number of pre-treatment periods (they correspond to columns 1-T0 of Y).
#' @param X Optional 3D array of time-varying covariates. Shape should be N X T X C for C covariates.
#'
#' @import dplyr ggplot2
#'
#' @author Riccardo Di Francesco
#'
#' @export
sc_estimate_reg <- function(Y, N0, T0, X = array(dim = c(dim(Y), 0))) {
  sc_estimate(Y, N0, T0, X = X, eta.omega = ((nrow(Y) - N0) * (ncol(Y) - T0))^(1/4))
}



#' Call Synthdid Estimation Function
#'
#' Calls one of the \code{synthdid} estimation functions.
#'
#' @param dta One of the champion data sets bundled in the package (\code{\link{lol_champ_pool_dta}} or \code{\link{lol_champ_dta}}).
#' @param outcome_colname Name of the column of \code{dta} storing the outcome of interest.
#' @param estimator Which estimator to use. Must be one of "sc" (standard synthetic control), "sc_reg" (sc + a ridge penalty), "synthdid" (synthetic diff-in-diff).
#' @param covariate_colnames Character vector with the names of the columns storing the time-varying covariates for which we want to adjust for.
#'
#' @details
#' Before calling the estimation function, \code{\link{call_synthdid}} processes \code{dta} to generate the required objects.\cr
#'
#' If \code{covariate_colnames} is non-empty, \code{\link{call_synthdid}} regresses the outcomes on the covariates, on time fixed effects, and unit fixed effects, with the regression estimated without
#' using observations in which the treatment takes place. Then, \code{\link{call_synthdid}} uses the estimated coefficients to compute the residuals of the outcomes for all observations (also those in which
#' the treatment takes place), and uses these residuals rather than the original outcomes for estimation.
#'
#' @import synthdid xsynthdid
#'
#' @author Riccardo Di Francesco
#'
#' @keywords internal
call_synthdid <- function(dta, outcome_colname, estimator, covariate_colnames) {
  ## 0.) Handling inputs and checks.
  if (!(estimator %in% c("sc", "sc_reg", "sdid"))) stop("Invalid 'estimator'. This must be one of 'sc', 'sc_reg', 'sdid'.", call. = FALSE)

  unit <- which(colnames(dta) == "champion")
  time <- which(colnames(dta) == "day_no")
  outcome <- which(colnames(dta) == outcome_colname)
  treatment <- which(colnames(dta) == "treatment")

  ## 1.) If necessary, adjust the outcomes.
  if (length(covariate_colnames) != 0) {
    dta$y_adjusted <- xsynthdid::adjust.outcome.for.x(dta, unit = unit, time = time, outcome = outcome, treatment = treatment, x = covariate_colnames)
    outcome <- which(colnames(dta) == "y_adjusted")
  }

  ## 2.) Process data.
  setup <- synthdid::panel.matrices(as.data.frame(dta), unit = unit, time = time, outcome = outcome, treatment = treatment)

  ## 3.) Run estimation.
  if (estimator == "sc") {
    tau_hat <- synthdid::sc_estimate(setup$Y, setup$N0, setup$T0)
  } else if (estimator == "sc_reg") {
    tau_hat <- sc_estimate_reg(setup$Y, setup$N0, setup$T0)
  } else if (estimator == "sdid") {
    tau_hat <- synthdid::synthdid_estimate(setup$Y, setup$N0, setup$T0)
  }

  return(tau_hat)
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
#' @import dplyr
#' @importFrom stats weighted.mean
#'
#' @export
construct_synth_outcome <- function(object, dta, unit_colname, outcome_colname, time_colname) {
  ## 0.) Handling inputs and checks.
  weight <- NULL

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
