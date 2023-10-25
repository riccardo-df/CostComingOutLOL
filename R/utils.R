#' Waiting for Next Iteration
#'
#' It stops the execution of R for a random number of seconds. Useful to avoid hitting rate limits while web-scraping or pulling from APIs.
#'
#' @param min_sec Minimum amount of seconds to be waited.
#' @param max_sec Maximum amount of seconds to be waited.
#'
#' @author Riccardo Di Francesco
#'
#' @keywords internal
sleep <- function(min_sec = 0, max_sec = 0) {
  ## Handling inputs and checks.
  if (min_sec < 0) stop("Negative time does not make sense.", call. = FALSE)
  if (max_sec < 0) stop("Negative time does not make sense.", call. = FALSE)

  Sys.sleep(stats::runif(1, min = round(min_sec, 0), max = round(max_sec, 0)))
}


#' Renaming Variables for LATEX Usage
#'
#' Renames variables where the character "_" is used, which causes clashes in LATEX. Useful for the \code{phased} print method.
#'
#' @param names string vector.
#'
#' @return
#' The renamed string vector. Strings where "_" is not found are not modified by \code{rename_latex}.
#'
#' @keywords internal
rename_latex <- function(names) {
  ## Locating variables that need renaming.
  idx <- grepl("_", names, fixed = TRUE)

  if (sum(idx) == 0) return(names)

  ## Renaming variables.
  split_names <- stringr::str_split(string = names[idx], pattern = "_", simplify = TRUE)
  attach_names <- paste(split_names[, 1], split_names[, 2], sep = "\\_")

  ## Replacing.
  names[idx] <- attach_names

  ## Output.
  return(names)
}


#' Sample Splitting
#'
#' Spits the sample into K folds of roughly the same size.
#'
#' @param n Sample size.
#' @param K Number of folds.
#'
#' @return
#' A list with indexes for the first and the second fold.
#'
#' @details
#' Currently, only \code{K = 2} is supported.
#'
#' @keywords internal
sample_split <- function(n, K = 2) {
  ## 0.) Handling inputs and checks.
  if (K != 2) stop("Cuurently, only K = 2 is supported.", call. = FALSE)
  observation_idx <- seq_len(n)

  ## 1.) Define sample size and indexes for first fold.
  fold_size <- floor((1 / K) * length(observation_idx))
  first_fold_ind <- sample(seq_len(length(observation_idx)), size = fold_size)

  ## 2.) Output.
  first_fold <- sort(observation_idx[first_fold_ind])
  second_fold <- observation_idx[-first_fold_ind]

  return(list("first_fold" = first_fold, "second_fold" = second_fold))
}


#' Debiased Machine Learning for Difference-in-Differences
#'
#' Fits the DMLDiD estimator.
#'
#' @param Y Outcome variable.
#' @param D Indicator for treated units.
#' @param t Indicator for post-treatment.
#' @param X Matrix of control variables.
#' @param K Controls K in K-fold cross-fitting.
#' @param learner String, controls the machine learner used to estimate the nuisance functions.
#' @param repeat_n Number of replications.
#'
#' @return
#' Point estimate for the ATT and its standard error.
#'
#' @details
#' To obtain a robust result, the estimator is fitted \code{repeat_n} times and the average is returned.\cr
#'
#' Original code: \href{https://github.com/NengChiehChang/Diff-in-Diff/blob/master/RCS_Algorithm.R}{https://github.com/NengChiehChang/Diff-in-Diff/blob/master/RCS_Algorithm.R}.
#'
#' @import glmnet grf
#'
#' @keywords internal
debiased_did <- function(Y, D, t, X,
                         K = 2, learner = "lasso",
                         repeat_n = 100) {
  ## Handling inputs and checks.
  if (!(learner %in% c("lasso", "forest"))) stop("Invalid 'learner'. This must be either 'lasso' or 'forest'.", call. = FALSE)

  n <- length(Y)

  theta_hats <- numeric(repeat_n)
  variances <- numeric(repeat_n)

  ## Obtain several point estimates and aggregate.
  for (l in seq_len(repeat_n)) {
    ## 1.) Split into K folds.
    folds <- sample_split(n, K)

    ## 2.) For the k-th fold, use other folds to estimate nuisances and predict on this fold to construct the k-th intermediate ATT estimator.
    intermediate_estimates <- as.numeric(K)

    for (k in seq_len(K)) {
      ## Define k-th fold.
      this_fold_idx <- folds[[k]]

      Y_in_fold <- Y[this_fold_idx]
      Y_out_fold <- Y[-this_fold_idx]

      D_in_fold <- D[this_fold_idx]
      D_out_fold <- D[-this_fold_idx]

      t_in_fold <- t[this_fold_idx]
      t_out_fold <- t[-this_fold_idx]

      X_in_fold <- X[this_fold_idx, ]
      X_out_fold <- X[-this_fold_idx, ]

      ## Estimate nuisance functions.
      # Propensity score.
      if (learner == "lasso") {
        cross_validate_lasso_g <- glmnet::cv.glmnet(as.matrix(X_out_fold), D_out_fold, family = "binomial", alpha = 1)
        fit_lasso_g <- glmnet::glmnet(as.matrix(X_out_fold), D_out_fold, family = "binomial", alpha = 1, lambda = cross_validate_lasso_g$lambda.1se)

        beta1_hat <- as.numeric(as.character(fit_lasso_g$beta))
        linear_index <- as.matrix(X_in_fold) %*% beta1_hat

        g_hat <- 1 / (1 + exp(- linear_index))
      } else if (learner == "forest") {
        forest_g <- grf::regression_forest(X_out_fold, D_out_fold)
        g_hat <- predict(forest_g, X_in_fold)$predictions
      }

      # Lambda.
      lambda_hat <- mean(t_out_fold)

      # l2 function.
      Y_control_out_fold <- Y_out_fold[D_out_fold == 0]
      t_control_out_fold <- t_out_fold[D_out_fold == 0]
      X_control_out_fold <- X_out_fold[D_out_fold == 0, ]

      if (learner == "lasso") {
        cross_validate_lasso_l2 <- glmnet::cv.glmnet(as.matrix(X_control_out_fold), (t_control_out_fold - lambda_hat) * Y_control_out_fold, family = "gaussian", alpha = 1)
        fit_lasso_l2 <- glmnet::glmnet(as.matrix(X_control_out_fold), (t_control_out_fold - lambda_hat) * Y_control_out_fold, family = "gaussian", alpha = 1, lambda = cross_validate_lasso_l2$lambda.1se)

        beta2_hat <- as.numeric(as.character(fit_lasso_l2$beta))

        l2_hat <- as.matrix(X_in_fold) %*% beta2_hat
      } else if (learner == "forest") {
        forest_l2 <- grf::regression_forest(X_control_out_fold, (t_control_out_fold - lambda_hat) * Y_control_out_fold)
        l2_hat <- predict(forest_l2, X_in_fold)$predictions
      }

      ## Construct intermediate ATT estimator and its variance.
      scores <- (D_in_fold - g_hat) / ( mean(D_in_fold) * lambda_hat * (1 - lambda_hat) * (1 - g_hat)) * ((t_in_fold - lambda_hat) * Y_in_fold - l2_hat)
      intermediate_estimates[k] <- mean(scores)
    }

    final_estimate <- mean(intermediate_estimates)
    theta_hats[l] <- final_estimate
  }

  aggregated_estimate <- mean(theta_hats)

  ## Compute variance of aggregated estimate.
  for (l in seq_len(repeat_n)) {
    ## 1.) Split into K folds.
    folds <- sample_split(n, K)

    ## 2.) For the k-th fold, use other folds to estimate nuisances and predict on this fold to construct the variance of the aggregated ATT estimator.
    intermediate_variances <- as.numeric(K)

    for (k in seq_len(K)) {
      ## Define k-th fold.
      this_fold_idx <- folds[[k]]

      Y_in_fold <- Y[this_fold_idx]
      Y_out_fold <- Y[-this_fold_idx]

      D_in_fold <- D[this_fold_idx]
      D_out_fold <- D[-this_fold_idx]

      t_in_fold <- t[this_fold_idx]
      t_out_fold <- t[-this_fold_idx]

      X_in_fold <- X[this_fold_idx, ]
      X_out_fold <- X[-this_fold_idx, ]

      ## Estimate nuisance functions.
      # Propensity score.
      if (learner == "lasso") {
        cross_validate_lasso_g <- glmnet::cv.glmnet(as.matrix(X_out_fold), D_out_fold, family = "binomial", alpha = 1)
        fit_lasso_g <- glmnet::glmnet(as.matrix(X_out_fold), D_out_fold, family = "binomial", alpha = 1, lambda = cross_validate_lasso_g$lambda.1se)

        beta1_hat <- as.numeric(as.character(fit_lasso_g$beta))
        linear_index <- as.matrix(X_in_fold) %*% beta1_hat

        g_hat <- 1 / (1 + exp(- linear_index))
      } else if (learner == "forest") {
        forest_g <- grf::regression_forest(X_out_fold, D_out_fold)
        g_hat <- predict(forest_g, X_in_fold)$predictions
      }

      # Lambda.
      lambda_hat <- mean(t_out_fold)

      # l2 function.
      Y_control_out_fold <- Y_out_fold[D_out_fold == 0]
      t_control_out_fold <- t_out_fold[D_out_fold == 0]
      X_control_out_fold <- X_out_fold[D_out_fold == 0, ]

      if (learner == "lasso") {
        cross_validate_lasso_l2 <- glmnet::cv.glmnet(as.matrix(X_control_out_fold), (t_control_out_fold - lambda_hat) * Y_control_out_fold, family = "gaussian", alpha = 1)
        fit_lasso_l2 <- glmnet::glmnet(as.matrix(X_control_out_fold), (t_control_out_fold - lambda_hat) * Y_control_out_fold, family = "gaussian", alpha = 1, lambda = cross_validate_lasso_l2$lambda.1se)

        beta2_hat <- as.numeric(as.character(fit_lasso_l2$beta))

        l2_hat <- as.matrix(X_in_fold) %*% beta2_hat
      } else if (learner == "forest") {
        forest_l2 <- grf::regression_forest(X_control_out_fold, (t_control_out_fold - lambda_hat) * Y_control_out_fold)
        l2_hat <- predict(forest_l2, X_in_fold)$predictions
      }

      ## Construct intermediate ATT estimator and its variance.
      G <- -(1 - 2 * lambda_hat) * aggregated_estimate / (lambda_hat * (1 - lambda_hat)) - mean(Y_in_fold * (D_in_fold - g_hat) / (1 - g_hat) / (lambda_hat * (1 - lambda_hat)) / mean(D_in_fold))
      scores_val <- ((t_in_fold - lambda_hat) * Y_in_fold - l2_hat) * (D_in_fold - g_hat) / (1 - g_hat) / mean(D_in_fold) / (lambda_hat * (1 - lambda_hat)) - D_in_fold * aggregated_estimate / mean(D_in_fold) + G * (t_in_fold - lambda_hat)

      intermediate_variances[k] <- mean(scores_val^2)
    }

    final_variance <- mean(intermediate_variances)
    variances[l] <- final_variance
  }

  aggregated_variance <- mean(variances)

  std_error <- sqrt(aggregated_variance) / sqrt(n)
  return(list("point_estimate" = aggregated_estimate, "se" = std_error))
}
