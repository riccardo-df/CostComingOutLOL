## @author: riccardo-df
## University of Rome Tor Vergata
## 30\06\2025

## The Cost of Coming Out - Descriptive Plots.

## This script produces ...

# Preliminaries -----------------------------------------------------------
rm(list = ls())
set.seed(1986)

## Loading packages.
pkgs <- c("CostComingOutLOL")
inst <- lapply(pkgs, library, character.only = TRUE)

## Define time events.
treatment_date <- as.POSIXct("2022-06-01")

# Descriptive table -------------------------------------------------------
## Arrange data pre- and post-treatment. For character panel, select only Graves.
champ_pre <- lol_champ_pool_dta %>%
  filter(day < treatment_date & champion == "Graves")
champ_post <- lol_champ_pool_dta %>%
  filter(day >= treatment_date & champion == "Graves")

player_pre <- lol_player_dta %>%
  filter(day < treatment_date)
player_post <- lol_player_dta %>%
  filter(day >= treatment_date)

## Summary function.
get_mean_sd <- function(var, data_pre, data_post) {
  mean_pre <- mean(data_pre[[var]])
  sd_pre <- sd(data_pre[[var]])
  mean_post <- mean(data_post[[var]])
  sd_post <- sd(data_post[[var]])
  return(data.frame(variable = var, mean_pre, sd_pre, mean_post, sd_post))
}

## Collect statistics.
stats <- bind_rows(get_mean_sd("pick_rate_pooled", champ_pre, champ_post) %>%
                     mutate(variable = "Graves pick rate (\\%)"),
                   get_mean_sd("win_rate_pooled", champ_pre, champ_post) %>%
                     mutate(variable = "Graves win rate (\\%)"),
                   get_mean_sd("win_rate", player_pre, player_post) %>%
                     mutate(variable = "Win rate (\\%)"),
                   get_mean_sd("n_matches", player_pre, player_post) %>%
                     mutate(variable = "N. daily matches"),
                   get_mean_sd("n_hours", player_pre, player_post) %>%
                     mutate(variable = "N. daily hours"),
                   get_mean_sd("top_rate", player_pre, player_post) %>%
                     mutate(variable = "Top (\\%)"),
                   get_mean_sd("jungle_rate", player_pre, player_post) %>%
                     mutate(variable = "Jungle (\\%)"),
                   get_mean_sd("mid_rate", player_pre, player_post) %>%
                     mutate(variable = "Mid (\\%)"),
                   get_mean_sd("bottom_rate", player_pre, player_post) %>%
                     mutate(variable = "Bottom (\\%)"),
                   get_mean_sd("support_rate", player_pre, player_post) %>%
                     mutate(variable = "Support (\\%)"))

## Function to format as LaTeX.
latex_code <- function(stats, label = "table_summary_stats", caption = "Pre- and post-treatment summary statistics.") {
  categories <- list(
    "Graves metrics" = c("Graves pick rate (\\%)", "Graves win rate (\\%)"),
    "Player activity" = c("Win rate (\\%)", "N. daily matches", "N. daily hours"),
    "Role selection patterns" = c("Top (\\%)", "Jungle (\\%)", "Mid (\\%)", "Bottom (\\%)", "Support (\\%)")
  )

  latex <- c(
    "\\begin{table}[t!]",
    "    \\centering",
    paste0("    \\caption{\\label{", label, "} ", caption, "}"),
    "    \\vspace{-0.25cm}",
    "    \\footnotesize",
    "    \\begin{tabular}{lccc}",
    "        \\toprule",
    "        & \\textbf{Pre-treatment} & \\textbf{Post-treatment} & \\textbf{Mean difference} \\\\",
    "        \\midrule"
  )

  for (group in names(categories)) {
    latex <- c(latex, paste0("        \\multicolumn{4}{l}{\\textbf{", group, "}} \\\\"))
    for (var in categories[[group]]) {
      row <- stats[stats$variable == var, ]
      if (nrow(row) == 0) next
      m1 <- row$mean_pre
      sd1 <- row$sd_pre
      m2 <- row$mean_post
      sd2 <- row$sd_post
      diff <- m2 - m1
      latex <- c(
        latex,
        sprintf("        %s & %.2f & %.2f & %+0.2f \\\\", var, m1, m2, diff),
        sprintf("         & (%.2f) & (%.2f) & \\\\", sd1, sd2)
      )
    }
    latex <- c(latex, "", "        \\addlinespace")
  }

  latex <- c(
    latex,
    "        \\bottomrule",
    "    \\end{tabular}",
    "",
    "    \\footnotesize",
    "    \\renewcommand{\\baselineskip}{11pt}",
    "    \\textit{Notes.} Means are reported in the first row for each variable, followed by standard deviations in parentheses. Pick and win rates are computed from the character-level data; remaining statistics are computed from the player-level data. Time spent in-game only captures time within matches, excluding champion selection and matchmaking time. The final column reports differences in means between the pre- and post-treatment periods.",
    "\\end{table}"
  )

  cat(paste(latex, collapse = "\n"))
}


## LATEX.
latex_code(stats)
