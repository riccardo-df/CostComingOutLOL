## @author: riccardo-df
## University of Rome Tor Vergata
## 21\02\2023

## The Cost of Coming Out - Most Played Characters.

## This script produces ...

# Preliminaries -----------------------------------------------------------
rm(list = ls())
set.seed(1986)

## Loading packages.
pkgs <- c("CostComingOutLOL")
inst <- lapply(pkgs, library, character.only = TRUE)

## Define time events.
treatment_date <- as.POSIXct("2022-06-01")

## Path to save plots.
save_here <- "C:/Users/riccardo-df/Dropbox/University/Research/Projects/Cost_Coming_Out/2_Data_Collection/CostComingOutLOL/Figures/1_Descriptives/Pick_Rates"

# Most Played Characters --------------------------------------------------
## Find most played character by main role.
most_played_pre <- lol_champ_pool_dta %>%
  filter(day < treatment_date) %>%
  group_by(champion) %>%
  mutate(avg_pick_rate = mean(pick_rate_pooled)) %>%
  ungroup() %>%
  distinct(champion, .keep_all = TRUE) %>%
  group_by(main_role) %>%
  slice_max(avg_pick_rate, n = 5) %>%
  select(main_role, champion, avg_pick_rate)

most_played_post <- lol_champ_pool_dta %>%
  filter(day >= treatment_date) %>%
  group_by(champion) %>%
  mutate(avg_pick_rate = mean(pick_rate_pooled)) %>%
  ungroup() %>%
  distinct(champion, .keep_all = TRUE) %>%
  group_by(main_role) %>%
  slice_max(avg_pick_rate, n = 5) %>%
  select(main_role, champion, avg_pick_rate)

## LATEX.
latex <- function() {
  ## Define table inputs.
  n_col <- 11

  ## Extract most played characters by role pre-treatment.
  top_champs_pre <- most_played_pre %>%
    filter(main_role == "TOP") %>%
    pull(champion)

  jungle_champs_pre <- most_played_pre %>%
    filter(main_role == "JUNGLE") %>%
    pull(champion)

  mid_champs_pre <- most_played_pre %>%
    filter(main_role == "MIDDLE") %>%
    pull(champion)

  bottom_champs_pre <- most_played_pre %>%
    filter(main_role == "BOTTOM") %>%
    pull(champion)

  support_champs_pre <- most_played_pre %>%
    filter(main_role == "UTILITY") %>%
    pull(champion)

  ## Extract most played characters by role post-treatment.
  top_champs_post <- most_played_post %>%
    filter(main_role == "TOP") %>%
    pull(champion)

  jungle_champs_post <- most_played_post %>%
    filter(main_role == "JUNGLE") %>%
    pull(champion)

  mid_champs_post <- most_played_post %>%
    filter(main_role == "MIDDLE") %>%
    pull(champion)

  bottom_champs_post <- most_played_post %>%
    filter(main_role == "BOTTOM") %>%
    pull(champion)

  support_champs_post <- most_played_post %>%
    filter(main_role == "UTILITY") %>%
    pull(champion)

  ## Extract average pre-treatment pick rates.
  top_pick_rates_pre <- most_played_pre %>%
    filter(main_role == "TOP") %>%
    pull(avg_pick_rate) %>%
    round(3)

  jungle_pick_rates_pre <- most_played_pre %>%
    filter(main_role == "JUNGLE") %>%
    pull(avg_pick_rate) %>%
    round(3)

  mid_pick_rates_pre <- most_played_pre %>%
    filter(main_role == "MIDDLE") %>%
    pull(avg_pick_rate) %>%
    round(3)

  bottom_pick_rates_pre <- most_played_pre %>%
    filter(main_role == "BOTTOM") %>%
    pull(avg_pick_rate) %>%
    round(3)

  support_pick_rates_pre <- most_played_pre %>%
    filter(main_role == "UTILITY") %>%
    pull(avg_pick_rate) %>%
    round(3)

  ## Extract average prost-treatment pick rates.
  top_pick_rates_post <- most_played_post %>%
    filter(main_role == "TOP") %>%
    pull(avg_pick_rate) %>%
    round(3)

  jungle_pick_rates_post <- most_played_post %>%
    filter(main_role == "JUNGLE") %>%
    pull(avg_pick_rate) %>%
    round(3)

  mid_pick_rates_post <- most_played_post %>%
    filter(main_role == "MIDDLE") %>%
    pull(avg_pick_rate) %>%
    round(3)

  bottom_pick_rates_post <- most_played_post %>%
    filter(main_role == "BOTTOM") %>%
    pull(avg_pick_rate) %>%
    round(3)

  support_pick_rates_post <- most_played_post %>%
    filter(main_role == "UTILITY") %>%
    pull(avg_pick_rate) %>%
    round(3)

  ## Print LATEX code.
  cat("\\begingroup
  \\setlength{\\tabcolsep}{8pt}
  \\renewcommand{\\arraystretch}{1.1}
  \\begin{table}[H]
    \\centering
    \\begin{adjustbox}{width = 1\\textwidth}
    \\begin{tabular}{@{\\extracolsep{5pt}}l ", rep("c ", n_col - 1), "}
      \\\\[-1.8ex]\\hline
      \\hline \\\\[-1.8ex]
      & \\multicolumn{5}{c}{\\textit{Pre-treatment}} & \\multicolumn{5}{c}{\\textit{Post-treatment}} \\\\ \\cmidrule{2-6} \\cmidrule{7-11}
      & \\textit{Top} & \\textit{Jungle} & \\textit{Mid} & \\textit{Bottom} & \\textit{Support} & \\textit{Top} & \\textit{Jungle} & \\textit{Mid} & \\textit{Bottom} & \\textit{Support} \\\\
      \\addlinespace[2pt]
      \\hline \\\\[-1.8ex] \n\n", sep = "")

  for (i in seq_len(length(top_champs_pre))) {
    cat("      \\textbf{", i, "} & ", top_champs_pre[i], " & ", jungle_champs_pre[i], " & ", mid_champs_pre[i], " & ", bottom_champs_pre[i], " & ", support_champs_pre[i], " & ", top_champs_post[i], " & ", jungle_champs_post[i], " & ", mid_champs_post[i], " & ", bottom_champs_post[i], " & ", support_champs_post[i], " \\\\ \n", sep = "")
    cat("                 & (", top_pick_rates_pre[i], ") & (", jungle_pick_rates_pre[i], ") & (", mid_pick_rates_pre[i], ") & (", bottom_pick_rates_pre[i], ") & (", support_pick_rates_pre[i], ") & (", top_pick_rates_post[i], ") & (", jungle_pick_rates_post[i], ") & (", mid_pick_rates_post[i], ") & (", bottom_pick_rates_post[i], ") & (", support_pick_rates_post[i], ") \\\\ \n", sep = "")
  }

  cat("\n      \\addlinespace[3pt]
      \\\\[-1.8ex]\\hline
      \\hline \\\\[-1.8ex]
      \\end{tabular}
      \\end{adjustbox}
      \\caption{Most popular characters by role based on average pick rates before the coming-out event. The pre-treatment average pick rates are displayed in parenthesis.}
      \\label{table_most_played_champions}
    \\end{table}
\\endgroup")
}

latex()

# Pick rate distribution across characters --------------------------------
## Compute average pre-treatment pick rate for all characters.
averages_pick <- lol_champ_pool_dta %>%
  filter(day <= max_date) %>%
  mutate(treatment = factor(day < treatment_date, levels = c(TRUE, FALSE), labels = c("Pre-treatment", "Post-treatment"))) %>%
  group_by(champion, treatment) %>%
  mutate(avg_pick_rate = mean(pick_rate_pooled)) %>%
  ungroup() %>%
  distinct(champion, treatment, .keep_all = TRUE) %>%
  select(champion, avg_pick_rate, treatment)

## Plot.
annotations <- data.frame(treatment = c("Pre-treatment", "Post-treatment"),
                          avg_pick_rate = averages_pick$avg_pick_rate[which(averages_pick$champion == "Graves")],
                          label = "Graves")

plot_density <- averages_pick %>%
  ggplot(aes(x = avg_pick_rate)) +
  geom_density(fill = "dodgerblue", color = "black", alpha = 0.8) +
  facet_grid(rows = vars(factor(treatment, levels = c("Pre-treatment", "Post-treatment")))) +
  geom_vline(data = filter(averages_pick, treatment == "Pre-treatment" & champion == "Graves"), aes(xintercept = avg_pick_rate), color = "red") +
  geom_vline(data = filter(averages_pick, treatment == "Post-treatment" & champion == "Graves"), aes(xintercept = avg_pick_rate), color = "red") +
  geom_text(data = annotations, aes(x = avg_pick_rate + 2, y = 0.07, label = label)) +
  xlab("Average pick rate") + ylab("") +
  theme_bw() +
  theme()
ggsave(paste0(save_here, "/", "plot_pick_rate_density.pdf"), plot_density, width = 7, height = 7)

