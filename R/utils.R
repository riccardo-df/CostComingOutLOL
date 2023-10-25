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
