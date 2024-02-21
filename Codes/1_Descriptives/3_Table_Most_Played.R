## @author: riccardo-df
## University of Rome Tor Vergata
## 21\02\2023

## The Cost of Coming Out - Most Played Characters.

## This script produces Figure 2.1.

# Preliminaries -----------------------------------------------------------
rm(list = ls())
set.seed(1986)

## Loading packages.
pkgs <- c("CostComingOutLOL")
inst <- lapply(pkgs, library, character.only = TRUE)

## Define time events.
min_date <- as.POSIXct("2022-01-01")
max_date <- as.POSIXct("2022-06-01")

# Most Played Characters --------------------------------------------------
## Subset pre-treatment data.
dta <- lol_champ_pool_dta %>%
  filter(day > min_date & day < max_date)

## Find most played character by main role.
most_played <- dta %>%
  group_by(champion) %>%
  mutate(avg_pick_rate_pre = mean(pick_rate_pooled)) %>%
  ungroup() %>%
  distinct(champion, .keep_all = TRUE) %>%
  group_by(main_role) %>%
  slice_max(avg_pick_rate_pre, n = 5) %>%
  select(main_role, champion, avg_pick_rate_pre)

# LATEX -------------------------------------------------------------------
latex <- function() {
  n_col <- 6

  top_champs <- most_played %>%
    filter(main_role == "TOP") %>%
    pull(champion)

  jungle_champs <- most_played %>%
    filter(main_role == "JUNGLE") %>%
    pull(champion)

  mid_champs <- most_played %>%
    filter(main_role == "MIDDLE") %>%
    pull(champion)

  bottom_champs <- most_played %>%
    filter(main_role == "BOTTOM") %>%
    pull(champion)

  support_champs <- most_played %>%
    filter(main_role == "UTILITY") %>%
    pull(champion)


  cat("\\begingroup
  \\setlength{\\tabcolsep}{8pt}
  \\renewcommand{\\arraystretch}{1.1}
  \\begin{table}[H]
    \\centering
    \\begin{adjustbox}{width = 1\\textwidth}
    \\begin{tabular}{@{\\extracolsep{5pt}}l ", rep("c ", n_col - 1), "}
      \\\\[-1.8ex]\\hline
      \\hline \\\\[-1.8ex]
      & \\textit{Top} & \\textit{Jungle} & \\textit{Mid} & \\textit{Bottom} & \\textit{Support} \\\\
      \\addlinespace[2pt]
      \\hline \\\\[-1.8ex] \n\n", sep = "")

  for (i in seq_len(length(top_champs))) {
    cat("      \\textbf{", i, "} & ", top_champs[i], " & ", jungle_champs[i], " & ", mid_champs[i], " & ", bottom_champs[i], " & ", support_champs[i], "\\\\ \n", sep = "")
  }

  cat("\n      \\addlinespace[3pt]
      \\\\[-1.8ex]\\hline
      \\hline \\\\[-1.8ex]
      \\end{tabular}
      \\end{adjustbox}
      \\caption{Point estimates and $95\\%$ confidence intervals for $\\hat{\\tau}$. Additionally, the number of donors receiving a non-zero weight and the pre-treatment root mean squared error are displayed. The first panel reports the results obtained using all the observed matches. The remaining four panels report the results obtained using only matches from a particular region. Each column corresponds to a different specification, with the specifications differing solely in the employed estimator and donor pool composition.}
      \\label{table_estimation_results}
    \\end{table}
\\endgroup")
}

latex()
