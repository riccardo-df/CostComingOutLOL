#' Cleaning LoL Data
#'
#' Cleans the raw LoL data as obtained from calling the \code{\link{pull_lol_data}} function.
#'
#' @param dta Data set as constructed by the \code{\link{pull_lol_data}} function (you can rowbind more data sets and pass in the final one).
#'
#' @details
#' \code{\link{clean_lol_data}} performs the following operations on the raw data.\cr
#'
#' First, it aggregates the regions of interest according to the geographical macro area where servers hosting the match are located (Latin America, North America, Europe, and Korea)
#' and drops regions with too many missing values (Oceania, Russia, Japan, and Turkey).\cr
#'
#' Second, it drops duplicated matches and matches with corrupted data (i.e., matches with less than ten players or which lasted less than five minutes or more than two hours or
#' where the position of some player is not assigned).\cr
#'
#' Third, it hard-codes the 'ban' variable (it maps it from integer numbers to champions' names).\cr
#'
#' Fourth, it identifies the main and the auxiliary positions of each champion based on where they are played the most and drops matches where champions are
#' played in a different position.\cr
#'
#' @import dplyr
#'
#' @importFrom data.table fwrite
#'
#' @author Riccardo Di Francesco
#'
#' @seealso \code{\link{pull_lol_data}}, \code{\link{construct_lol_champion_data}}, \code{\link{construct_lol_champion_pooled_data}}, \code{\link{construct_lol_player_data}}
#'
#' @export
clean_lol_data <- function(dta) {
  ## Merge regions according to macro area and drop regions with too many missings.
  cat("Merging regions of interest and dropping the others. \n")
  dta$region[dta$region == "Latin_America_South" | dta$region == "Latin_America_North" | dta$region == "Brasil"] <- "Latin_America"
  dta$region[dta$region == "Europe_West" | dta$region == "Europe_Nord_East"] <- "Europe"

  dta_drop <- dta %>%
    dplyr::filter(!(region %in% c("Oceania", "Russia", "Japan", "Turkey")))
  unique(dta_drop$region)

  ## Drop duplicate rows (match id/participant name) and corrupted matches.
  cat("Dropping duplicated and corrupted matches. \n")
  dta_noduplicate <- dta_drop %>%
    dplyr::distinct(match_id, player_puiid, .keep_all = TRUE) %>%
    dplyr::group_by(match_id) %>%
    dplyr::filter(n() == 10) %>%
    dplyr::filter(5 <= duration & duration <= 120) %>%
    filter(position != "") %>%
    dplyr::ungroup()

  ## Drop again matches with less than 10 players (double-check).
  dta_noduplicate <- dta_noduplicate %>%
    group_by(match_id) %>%
    filter(n() == 10) %>%
    ungroup()

  ## Hard-code ban variable.
  cat("Hard-coding ban variable. \n")
  dta_noduplicate$ban[dta_noduplicate$ban == 516] = "Ornn"
  dta_noduplicate$ban[dta_noduplicate$ban == 112] = "Viktor"
  dta_noduplicate$ban[dta_noduplicate$ban == 91] = "Talon"
  dta_noduplicate$ban[dta_noduplicate$ban == 21] = "MissFortune"
  dta_noduplicate$ban[dta_noduplicate$ban == 43] = "Karma"
  dta_noduplicate$ban[dta_noduplicate$ban == 777] = "Yone"
  dta_noduplicate$ban[dta_noduplicate$ban == 64] = "LeeSin"
  dta_noduplicate$ban[dta_noduplicate$ban == 134] = "Syndra"
  dta_noduplicate$ban[dta_noduplicate$ban == 22] = "Ashe"
  dta_noduplicate$ban[dta_noduplicate$ban == 26] = "Zilean"
  dta_noduplicate$ban[dta_noduplicate$ban == 875] = "Sett"
  dta_noduplicate$ban[dta_noduplicate$ban == 59] = "JarvanIV"
  dta_noduplicate$ban[dta_noduplicate$ban == 34] = "Anivia"
  dta_noduplicate$ban[dta_noduplicate$ban == 202] = "Jhin"
  dta_noduplicate$ban[dta_noduplicate$ban == 555] = "Pyke"
  dta_noduplicate$ban[dta_noduplicate$ban == 266] = "Aatrox"
  dta_noduplicate$ban[dta_noduplicate$ban == 56] = "Nocturne"
  dta_noduplicate$ban[dta_noduplicate$ban == 51] = "Caitlyn"
  dta_noduplicate$ban[dta_noduplicate$ban == 99] = "Lux"
  dta_noduplicate$ban[dta_noduplicate$ban == 1] = "Annie"
  dta_noduplicate$ban[dta_noduplicate$ban == 245] = "Ekko"
  dta_noduplicate$ban[dta_noduplicate$ban == 157] = "Yasuo"
  dta_noduplicate$ban[dta_noduplicate$ban == 111] = "Nautilus"
  dta_noduplicate$ban[dta_noduplicate$ban == 40] = "Janna"
  dta_noduplicate$ban[dta_noduplicate$ban == 80] = "Pantheon"
  dta_noduplicate$ban[dta_noduplicate$ban == 50] = "Swain"
  dta_noduplicate$ban[dta_noduplicate$ban == 498] = "Xayah"
  dta_noduplicate$ban[dta_noduplicate$ban == 497] = "Rakan"
  dta_noduplicate$ban[dta_noduplicate$ban == 114] = "Fiora"
  dta_noduplicate$ban[dta_noduplicate$ban == 711] = "Vex"
  dta_noduplicate$ban[dta_noduplicate$ban == 119] = "Draven"
  dta_noduplicate$ban[dta_noduplicate$ban == 57] = "Maokai"
  dta_noduplicate$ban[dta_noduplicate$ban == 74] = "Heimerdinger"
  dta_noduplicate$ban[dta_noduplicate$ban == 200] = "Belveth"
  dta_noduplicate$ban[dta_noduplicate$ban == 517] = "Sylas"
  dta_noduplicate$ban[dta_noduplicate$ban == 235] = "Senna"
  dta_noduplicate$ban[dta_noduplicate$ban == 164] = "Camille"
  dta_noduplicate$ban[dta_noduplicate$ban == 142] = "Zoe"
  dta_noduplicate$ban[dta_noduplicate$ban == 526] = "Rell"
  dta_noduplicate$ban[dta_noduplicate$ban == 82] = "Mordekaiser"
  dta_noduplicate$ban[dta_noduplicate$ban == 412] = "Thresh"
  dta_noduplicate$ban[dta_noduplicate$ban == 69] = "Cassiopeia"
  dta_noduplicate$ban[dta_noduplicate$ban == 8] = "Vladimir"
  dta_noduplicate$ban[dta_noduplicate$ban == 523] = "Aphelios"
  dta_noduplicate$ban[dta_noduplicate$ban == 147] = "Seraphine"
  dta_noduplicate$ban[dta_noduplicate$ban == 39] = "Irelia"
  dta_noduplicate$ban[dta_noduplicate$ban == 30] = "Karthus"
  dta_noduplicate$ban[dta_noduplicate$ban == 246] = "Qiyana"
  dta_noduplicate$ban[dta_noduplicate$ban == 141] = "Kayn"
  dta_noduplicate$ban[dta_noduplicate$ban == 18] = "Tristana"
  dta_noduplicate$ban[dta_noduplicate$ban == 888] = "Renata"
  dta_noduplicate$ban[dta_noduplicate$ban == 122] = "Darius"
  dta_noduplicate$ban[dta_noduplicate$ban == 429] = "Kalista"
  dta_noduplicate$ban[dta_noduplicate$ban == 234] = "Viego"
  dta_noduplicate$ban[dta_noduplicate$ban == 29] = "Twitch"
  dta_noduplicate$ban[dta_noduplicate$ban == 117] = "Lulu"
  dta_noduplicate$ban[dta_noduplicate$ban == 110] = "Varus"
  dta_noduplicate$ban[dta_noduplicate$ban == 201] = "Braum"
  dta_noduplicate$ban[dta_noduplicate$ban == 145] = "Kaisa"
  dta_noduplicate$ban[dta_noduplicate$ban == 121] = "Khazix"
  dta_noduplicate$ban[dta_noduplicate$ban == 14] = "Sion"
  dta_noduplicate$ban[dta_noduplicate$ban == 161] = "Velkoz"
  dta_noduplicate$ban[dta_noduplicate$ban == 85] = "Kennen"
  dta_noduplicate$ban[dta_noduplicate$ban == 421] = "RekSai"
  dta_noduplicate$ban[dta_noduplicate$ban == 37] = "Sona"
  dta_noduplicate$ban[dta_noduplicate$ban == 58] = "Renekton"
  dta_noduplicate$ban[dta_noduplicate$ban == 77] = "Udyr"
  dta_noduplicate$ban[dta_noduplicate$ban == 76] = "Nidalee"
  dta_noduplicate$ban[dta_noduplicate$ban == 127] = "Lissandra"
  dta_noduplicate$ban[dta_noduplicate$ban == 27] = "Singed"
  dta_noduplicate$ban[dta_noduplicate$ban == 101] = "Xerath"
  dta_noduplicate$ban[dta_noduplicate$ban == 150] = "Gnar"
  dta_noduplicate$ban[dta_noduplicate$ban == 131] = "Diana"
  dta_noduplicate$ban[dta_noduplicate$ban == 2] = "Olaf"
  dta_noduplicate$ban[dta_noduplicate$ban == 17] = "Teemo"
  dta_noduplicate$ban[dta_noduplicate$ban == 107] = "Rengar"
  dta_noduplicate$ban[dta_noduplicate$ban == 81] = "Ezreal"
  dta_noduplicate$ban[dta_noduplicate$ban == 350] = "Yuumi"
  dta_noduplicate$ban[dta_noduplicate$ban == 45] = "Veigar"
  dta_noduplicate$ban[dta_noduplicate$ban == 15] = "Sivir"
  dta_noduplicate$ban[dta_noduplicate$ban == 33] = "Rammus"
  dta_noduplicate$ban[dta_noduplicate$ban == 154] = "Zac"
  dta_noduplicate$ban[dta_noduplicate$ban == 86] = "Garen"
  dta_noduplicate$ban[dta_noduplicate$ban == 113] = "Sejuani"
  dta_noduplicate$ban[dta_noduplicate$ban == 63] = "Brand"
  dta_noduplicate$ban[dta_noduplicate$ban == 11] = "MasterYi"
  dta_noduplicate$ban[dta_noduplicate$ban == 68] = "Rumble"
  dta_noduplicate$ban[dta_noduplicate$ban == 120] = "Hecarim"
  dta_noduplicate$ban[dta_noduplicate$ban == 84] = "Akali"
  dta_noduplicate$ban[dta_noduplicate$ban == 104] = "Graves"
  dta_noduplicate$ban[dta_noduplicate$ban == 223] = "TahmKench"
  dta_noduplicate$ban[dta_noduplicate$ban == 268] = "Azir"
  dta_noduplicate$ban[dta_noduplicate$ban == 10] = "Kayle"
  dta_noduplicate$ban[dta_noduplicate$ban == 41] = "Gangplank"
  dta_noduplicate$ban[dta_noduplicate$ban == 887] = "Gwen"
  dta_noduplicate$ban[dta_noduplicate$ban == 96] = "KogMaw"
  dta_noduplicate$ban[dta_noduplicate$ban == 98] = "Shen"
  dta_noduplicate$ban[dta_noduplicate$ban == 4] = "TwistedFate"
  dta_noduplicate$ban[dta_noduplicate$ban == 222] = "Jinx"
  dta_noduplicate$ban[dta_noduplicate$ban == 163] = "Taliyah"
  dta_noduplicate$ban[dta_noduplicate$ban == 360] = "Samira"
  dta_noduplicate$ban[dta_noduplicate$ban == 9] = "FiddleSticks"
  dta_noduplicate$ban[dta_noduplicate$ban == 103] = "Ahri"
  dta_noduplicate$ban[dta_noduplicate$ban == 23] = "Tryndamere"
  dta_noduplicate$ban[dta_noduplicate$ban == 16] = "Soraka"
  dta_noduplicate$ban[dta_noduplicate$ban == 7] = "Leblanc"
  dta_noduplicate$ban[dta_noduplicate$ban == 54] = "Malphite"
  dta_noduplicate$ban[dta_noduplicate$ban == 92] = "Riven"
  dta_noduplicate$ban[dta_noduplicate$ban == 32] = "Amumu"
  dta_noduplicate$ban[dta_noduplicate$ban == 89] = "Leona"
  dta_noduplicate$ban[dta_noduplicate$ban == 106] = "Volibear"
  dta_noduplicate$ban[dta_noduplicate$ban == 20] = "Nunu"
  dta_noduplicate$ban[dta_noduplicate$ban == 24] = "Jax"
  dta_noduplicate$ban[dta_noduplicate$ban == 55] = "Katarina"
  dta_noduplicate$ban[dta_noduplicate$ban == 19] = "Warwick"
  dta_noduplicate$ban[dta_noduplicate$ban == 48] = "Trundle"
  dta_noduplicate$ban[dta_noduplicate$ban == 238] = "Zed"
  dta_noduplicate$ban[dta_noduplicate$ban == 254] = "Vi"
  dta_noduplicate$ban[dta_noduplicate$ban == 78] = "Poppy"
  dta_noduplicate$ban[dta_noduplicate$ban == 38] = "Kassadin"
  dta_noduplicate$ban[dta_noduplicate$ban == 36] = "DrMundo"
  dta_noduplicate$ban[dta_noduplicate$ban == 79] = "Gragas"
  dta_noduplicate$ban[dta_noduplicate$ban == 67] = "Vayne"
  dta_noduplicate$ban[dta_noduplicate$ban == 267] = "Nami"
  dta_noduplicate$ban[dta_noduplicate$ban == 3] = "Galio"
  dta_noduplicate$ban[dta_noduplicate$ban == 53] = "Blitzcrank"
  dta_noduplicate$ban[dta_noduplicate$ban == 432] = "Bard"
  dta_noduplicate$ban[dta_noduplicate$ban == 115] = "Ziggs"
  dta_noduplicate$ban[dta_noduplicate$ban == 203] = "Kindred"
  dta_noduplicate$ban[dta_noduplicate$ban == 6] = "Urgot"
  dta_noduplicate$ban[dta_noduplicate$ban == 12] = "Alistar"
  dta_noduplicate$ban[dta_noduplicate$ban == 518] = "Neeko"
  dta_noduplicate$ban[dta_noduplicate$ban == 420] = "Illaoi"
  dta_noduplicate$ban[dta_noduplicate$ban == 61] = "Orianna"
  dta_noduplicate$ban[dta_noduplicate$ban == 895] = "Nilah"
  dta_noduplicate$ban[dta_noduplicate$ban == 221] = "Zeri"
  dta_noduplicate$ban[dta_noduplicate$ban == 62] = "MonkeyKing"
  dta_noduplicate$ban[dta_noduplicate$ban == 166] = "Akshan"
  dta_noduplicate$ban[dta_noduplicate$ban == 236] = "Lucian"
  dta_noduplicate$ban[dta_noduplicate$ban == 876] = "Lillia"
  dta_noduplicate$ban[dta_noduplicate$ban == 75] = "Nasus"
  dta_noduplicate$ban[dta_noduplicate$ban == 427] = "Ivern"
  dta_noduplicate$ban[dta_noduplicate$ban == 83] = "Yorick"
  dta_noduplicate$ban[dta_noduplicate$ban == 133] = "Quinn"
  dta_noduplicate$ban[dta_noduplicate$ban == 31] = "Chogath"
  dta_noduplicate$ban[dta_noduplicate$ban == 240] = "Kled"
  dta_noduplicate$ban[dta_noduplicate$ban == 28] = "Evelynn"
  dta_noduplicate$ban[dta_noduplicate$ban == 126] = "Jayce"
  dta_noduplicate$ban[dta_noduplicate$ban == 5] = "XinZhao"
  dta_noduplicate$ban[dta_noduplicate$ban == 90] = "Malzahar"
  dta_noduplicate$ban[dta_noduplicate$ban == 105] = "Fizz"
  dta_noduplicate$ban[dta_noduplicate$ban == 60] = "Elise"
  dta_noduplicate$ban[dta_noduplicate$ban == 13] = "Ryze"
  dta_noduplicate$ban[dta_noduplicate$ban == 143] = "Zyra"
  dta_noduplicate$ban[dta_noduplicate$ban == 44] = "Taric"
  dta_noduplicate$ban[dta_noduplicate$ban == 25] = "Morgana"
  dta_noduplicate$ban[dta_noduplicate$ban == 72] = "Skarner"
  dta_noduplicate$ban[dta_noduplicate$ban == 35] = "Shaco"
  dta_noduplicate$ban[dta_noduplicate$ban == 102] = "Shyvana"
  dta_noduplicate$ban[dta_noduplicate$ban == 42] = "Corki"
  dta_noduplicate$ban[dta_noduplicate$ban == 136] = "AurelionSol"
  dta_noduplicate$ban[dta_noduplicate$ban == 897] = "KSante"
  dta_noduplicate$ban[dta_noduplicate$ban == -1] = "None"

  ## Format dates by dropping hours, minutes and seconds.
  cat("Formatting time variable. \n")
  dta_noduplicate$day <- as.POSIXct(strftime(dta_noduplicate$day, format = "%Y-%m-%d"))

  ## To each champion, assign most-played positions.Then, drop observations playing "out-of-role."
  cat("Identifying main and auxiliary positions and dropping 'out-of-role' matches. \n")
  champs_positions1 <- apply(table(dta_noduplicate$champion, dta_noduplicate$position), MARGIN = 1, function(x) {
    if (which.max(x) == 1) {"BOTTOM"}
    else if (which.max(x) == 2) {"JUNGLE"}
    else if (which.max(x) == 3) {"MIDDLE"}
    else if (which.max(x) == 4) {"TOP"}
    else if (which.max(x) == 5) {"UTILITY"}
  })

  champs_positions2 <- apply(table(dta_noduplicate$champion, dta_noduplicate$position), MARGIN = 1, function(x) {
    if (order(x, decreasing = TRUE)[2] == 1) {"BOTTOM"}
    else if (order(x, decreasing = TRUE)[2] == 2) {"JUNGLE"}
    else if (order(x, decreasing = TRUE)[2] == 3) {"MIDDLE"}
    else if (order(x, decreasing = TRUE)[2] == 4) {"TOP"}
    else if (order(x, decreasing = TRUE)[2] == 5) {"UTILITY"}
  })

  dta_noduplicate$main_role <- sapply(dta_noduplicate$champion, function(x) { champs_positions1[x] })
  dta_noduplicate$aux_role <- sapply(dta_noduplicate$champion, function(x) { champs_positions2[x] })

  dta_noduplicate <- dta_noduplicate %>%
    dplyr::group_by(match_id, champion) %>%
    dplyr::mutate(in_position = position %in% c(main_role, aux_role)) %>%
    dplyr::filter(in_position) %>%
    dplyr::ungroup()

  # Drop again matches with less than ten players (double-check).
  dta_noduplicate <- dta_noduplicate %>%
    dplyr::group_by(match_id) %>%
    dplyr::filter(n() == 10) %>%
    dplyr::ungroup()

  ## Drop useless covariates and order columns.
  dta_final <- dta_noduplicate %>%
    select(c("match_id", "player_name", "player_puiid", "player_level", "day",
             "champion", "position", "kills", "assists", "deaths", "gold", "duration", "early_surrender",
             "surrender", "win", "ban", "region", "main_role", "aux_role", "rank", "division"))

  ## Final checks.
  if (length(unique(table(dta_final$position))) != 1) warning("The number of played positions is not equal! You may want to double-check.")
  if (sum(dta_final$win) != sum(1 - dta_final$win)) warning("The shares of won and lost matches are unequal! You may want to double-check.")

  ## Write csv.
  cat("Writing csv file at ", getwd(), ". \n\n", sep = "")
  data.table::fwrite(dta_final, file = "lol_cleaned_dta.csv", row.names = FALSE)
}


#' Construct LoL Champion Data Set
#'
#' Constructs the LoL champion data set by pooling observations over regions.
#'
#' @param dta Data set as constructed by the \code{\link{clean_lol_data}} function.
#'
#' @details
#' \code{\link{construct_lol_champion_pooled_data}} performs the following operations on \code{dta}.\cr
#'
#' First, it keeps only data for the year 2022 up to July 15th and drops champions that have been released after June 1st, 2022 (KSante, Nilah, and Belveth).\cr
#'
#' Second, it generates the variables of interest. Notice that NAs may be produced here because of champions never picked or banned in a particular
#' \code{day} and \code{region}. These are replaced with zeroes.
#' \describe{
#'  \item{\code{pick_level}}{For each pair \code{region} and \code{day}, count how many times a particular \code{champion} is picked by players.}
#'  \item{\code{ban_level}}{For each pair \code{region} and \code{day}, count how many times a particular \code{champion} is banned by players.}
#'  \item{\code{win_level}}{For each triple \code{region}, \code{day}, and \code{champion}, count how many times a match is won.}
#'  \item{\code{gold_avg}}{For each triple \code{region}, \code{day}, and \code{champion}, compute the average gold earned in matches.}
#'  \item{\code{kills_avg}}{For each triple \code{region}, \code{day}, and \code{champion}, compute the average number of kills achieved in matches.}
#'  \item{\code{assists_avg}}{For each triple \code{region}, \code{day}, and \code{champion}, compute the average number of assists achieved in matches.}
#'  \item{\code{deaths_avg}}{For each triple \code{region}, \code{day}, and \code{champion}, compute the average number of deaths achieved in matches.}
#'  \item{\code{main_role}}{For each \code{champion}, extract its main position.}
#'  \item{\code{aux_role}}{For each \code{champion}, extract its auxiliary position.}
#' }
#'
#' Third, it balances the panel by imputing the zeroes for champions that have not been picked in a particular pair \code{region} and \code{day}. This part
#' of the function works in parallel using all but one processors of your computer.\cr
#'
#' Fourth, it generates additional variables. As before, NAs may be produced and are replaced with zeroes.\cr
#' \describe{
#'  \item{\code{n_matches}}{For each pair \code{region} and \code{day}, count how many matches have been played.}
#'  \item{\code{pick_rate}}{Divide \code{pick_level} by \code{n_matches}.}
#'  \item{\code{ban_rate}}{Divide \code{ban_level} by \code{n_matches}.}
#'  \item{\code{win_rate}}{Divide \code{win_level} by \code{pick_level}.}
#'  \item{\code{day_no}}{Numeric version of the \code{day} variable, useful when the data set is loaded in other sessions to get the time right.}
#' }
#'
#' @import dplyr lubridate parallel doParallel foreach plm
#'
#' @importFrom data.table fwrite
#'
#' @author Riccardo Di Francesco
#'
#' @seealso \code{\link{pull_lol_data}}, \code{\link{clean_lol_data}}, \code{\link{construct_lol_champion_pooled_data}}, \code{\link{construct_lol_player_data}}
#'
#' @export
construct_lol_champion_data <- function(dta) {
  ## Keep only 2022 data and drop champions released after treatment.
  cat("Keeping only 2022 data up to July 15th and dropping champions released after treatment. \n")
  new_champions <- c("KSante", "Nilah", "Belveth")

  dta <- dta %>%
    dplyr::filter(lubridate::year(day) == 2022) %>%
    dplyr::filter(day < as.POSIXct("2022-07-15")) %>%
    dplyr::filter(!(champion %in% new_champions))

  ## Generate variables and merging.
  cat("Generating outcomes and covariates. \n")

  cat("    Pick level. \n")
  pick_level <- dta %>%
    dplyr::group_by(region, day) %>%
    dplyr::count(champion) %>%
    ungroup()
  colnames(pick_level)[4] <- "pick_level"

  cat("    Ban level. \n")
  ban_level <- dta %>%
    dplyr::group_by(region, day) %>%
    dplyr::count(ban) %>%
    ungroup()
  colnames(ban_level) <- c("region", "day", "champion", "ban_level")

  cat("    Win level. \n")
  win_level <- dta %>%
    dplyr::group_by(region, day, champion) %>%
    dplyr::summarise(win_level = sum(win)) %>%
    dplyr::distinct(.keep_all = TRUE) %>%
    ungroup()

  cat("    Average gold, kills, assists, and deaths. \n")
  averages <- dta %>%
    dplyr::group_by(region, day, champion) %>%
    dplyr::summarise_at(c("gold", "kills", "assists", "deaths"), mean)
  colnames(averages) <- c("region", "day", "champion", "gold_avg", "kills_avg", "assists_avg", "deaths_avg")

  cat("    Main and auxiliary roles. \n")
  main_roles <- dta %>%
    dplyr::select("champion", "main_role", "aux_role") %>%
    dplyr::distinct(.keep_all = TRUE)

  daily_panel <- left_join(pick_level, ban_level, by = c("region", "day", "champion")) %>%
    dplyr::left_join(win_level, by = c("region", "day", "champion")) %>%
    dplyr::left_join(averages, by = c("region", "day", "champion")) %>%
    replace(is.na(.), 0)

  ## Imputing zeroes.
  cat("Balancing the panel by imputing zeroes. \n")

  days <- unique(daily_panel$day)
  champions_pool <- unique(dta$champion)

  # Setting parallel backend.
  cores <- parallel::detectCores()
  cl <- parallel::makeCluster(cores - 1)
  doParallel::registerDoParallel(cl)

  # For each day, impute zeroes to missing champions.
  imputation <- foreach(i = seq_len(length(days)), .combine = rbind) %dopar% {
    library(dplyr)

    # Select champions not picked in each region and impute values.
    temp_day <- days[i]
    tibble_not_picked <- tibble()

    for (r in seq_len(length(unique(daily_panel$region)))) {
      temp_region <- as.character(unique(daily_panel$region)[r])

      not_picked <- champions_pool[!(champions_pool %in% (daily_panel %>%
                                                            dplyr::filter(region == temp_region & day == temp_day) %>%
                                                            dplyr::pull(champion)))]
      if (length(not_picked) == 0) next

      # Create tibble with zeroes.
      tibble_not_picked <- rbind(tibble_not_picked,
                                 dplyr::tibble("region" = temp_region,
                                        "day" = temp_day,
                                        "champion" = not_picked,
                                        "pick_level" = 0,
                                        "win_level" = 0,
                                        "ban_level" = 0,
                                        "gold_avg" = 0,
                                        "kills_avg" = 0,
                                        "assists_avg" = 0,
                                        "deaths_avg" = 0))
    }

    tibble_not_picked
  }

  parallel::stopCluster(cl)

  # Rbind.
  new_daily_panel <- dplyr::bind_rows(daily_panel, imputation)

  ## Generate additional variables.
  cat("Generating additional covariates. \n")

  cat("    Number of matches. \n")
  n_matches <- dta %>%
    dplyr::group_by(region, day) %>%
    dplyr::summarise(n_matches = n_distinct(match_id)) %>%
    ungroup()

  cat("    Compute variables in rates. \n")
  final_daily_panel <- left_join(new_daily_panel, n_matches, by = c("region", "day")) %>%
    dplyr::mutate(pick_rate = pick_level / n_matches,
           ban_rate = ban_level / n_matches,
           win_rate = win_level / pick_level) %>%
    dplyr::left_join(main_roles, by = c("champion")) %>%
    replace(is.na(.), 0)

  ## Final operations.
  panel <- final_daily_panel
  panel$day_no <- as.numeric(panel$day)
  panel <- panel %>%
    dplyr::select(region, day, day_no, n_matches, champion, pick_level, pick_rate, ban_level, ban_rate, win_level, win_rate, gold_avg, kills_avg, assists_avg, deaths_avg, main_role, aux_role)

  ## Final checks.
  if (!unique(panel %>%
              dplyr::group_by(region, day) %>%
              dplyr::summarise(n_distinct(champion)) %>%
              dplyr::pull(`n_distinct(champion)`)) == length(champions_pool)) warning("Something is wrong. You may want to double-check.")

  if (!panel %>%
      dplyr::filter(region == "Europe") %>%
      plm::is.pbalanced(index = c("champion", "day"))) warning("The panel is not balanced. You may want to double-check.")

  if (!panel %>%
      dplyr::filter(region == "Korea") %>%
      plm::is.pbalanced(index = c("champion", "day"))) warning("The panel is not balanced. You may want to double-check.")

  if (!panel %>%
      dplyr::filter(region == "North_America") %>%
      plm::is.pbalanced(index = c("champion", "day"))) warning("The panel is not balanced. You may want to double-check.")

  if (!panel %>%
      dplyr::filter(region == "Latin_America") %>%
      plm::is.pbalanced(index = c("champion", "day"))) warning("The panel is not balanced. You may want to double-check.")

  ## Write csv.
  cat("Writing csv file at ", getwd(), ". \n\n", sep = "")
  data.table::fwrite(panel, file = "lol_champ_dta.csv", row.names = FALSE)
}


#' Construct LoL Champion Data Set (Pooled)
#'
#' Constructs the LoL champion data set by aggregating observations over regions.
#'
#' @param dta Data set as constructed by the \code{\link{construct_lol_champion_data}} function (you can find this data set already bundled in the package).
#'
#' @details
#' \code{\link{construct_lol_champion_pooled_data}} generates new variables by aggregating the observations (i.e., the champions) over the regions.\cr
#' \describe{
#'  \item{\code{pick_level_sum}, \code{ban_level_sum}, \code{win_level_sum}}{Sum the corresponding variables over regions. For instance, \code{pick_level_sum} is constructed by summing \code{pick_level} over Europe, Latin America, North America, and Korea, and tells us how many times \code{champion} has been picked on a given \code{day} overall.}
#'  \item{\code{n_matches_sum}}{Sum \code{n_matches} over regions. It tells us the total number of matches played in a given \code{day}.}
#'  \item{\code{pick_rate_pooled}, \code{ban_rate_pooled}, \code{win_rate_pooled}}{Divide the corresponding variables by \code{n_matches_sum}. Exception is \code{win_rate_pooled}, which is divided by \code{pick_level_sum}.}
#'  \item{\code{gold_pooled}, \code{kills_avg}, \code{assists_avg}, \code{deaths_avg}}{Average the corresponding variables over regions. We consider only days where \code{champion} has a non-zero \code{pick_level}.}
#'  \item{\code{main_role}}{For each \code{champion}, extract its main position.}
#'  \item{\code{aux_role}}{For each \code{champion}, extract its auxiliary position.}
#' }
#'
#' @import dplyr plm
#'
#' @importFrom data.table fwrite
#'
#' @author Riccardo Di Francesco
#'
#' @seealso \code{\link{pull_lol_data}}, \code{\link{clean_lol_data}}, \code{\link{construct_lol_champion_data}}, \code{\link{construct_lol_player_data}}
#'
#' @export
construct_lol_champion_pooled_data <- function(dta) {
  ## Generate pooled variables.
  cat("Generating pooled variables. \n")

  pooled <- dta %>%
    dplyr::group_by(day, champion) %>%
    dplyr::mutate(pick_level_sum = sum(pick_level),
           ban_level_sum = sum(ban_level),
           win_level_sum = sum(win_level),
           n_matches_sum = sum(n_matches),
           pick_rate_pooled = pick_level_sum / n_matches_sum,
           ban_rate_pooled = ban_level_sum / n_matches_sum,
           win_rate_pooled = win_level_sum / pick_level_sum,
           gold_pooled = mean(gold_avg[pick_level != 0]),
           kills_pooled = mean(kills_avg[pick_level != 0]),
           assists_pooled = mean(assists_avg[pick_level != 0]),
           deaths_pooled = mean(deaths_avg[pick_level != 0])) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(day, champion, .keep_all = TRUE) %>%
    dplyr::select(day, day_no, n_matches_sum, champion, pick_level_sum, pick_rate_pooled, ban_level_sum, ban_rate_pooled, win_level_sum, win_rate_pooled, gold_pooled, kills_pooled, assists_pooled, deaths_pooled)

    main_roles <- dta %>%
      dplyr::select("champion", "main_role", "aux_role") %>%
      dplyr::distinct(.keep_all = TRUE)

    ## Final panel and checks.
    panel <- dplyr::left_join(pooled, main_roles, by = "champion") %>%
      replace(is.na(.), 0)

    if (!plm::is.pbalanced(panel, index = c("champion", "day"))) warning("The panel is not balanced. You may want to double-check.")

  ## Write csv.
  cat("Writing csv file at ", getwd(), ". \n\n", sep = "")
  data.table::fwrite(panel, file = "lol_champ_pool_dta.csv", row.names = FALSE)
}


#' Construct LoL Player Data Set
#'
#' Constructs the LoL player data set.
#'
#' @param dta Data set as constructed by the \code{\link{clean_lol_data}} function.
#'
#' @details
#' \code{\link{construct_lol_player_data}} performs the following operations on \code{dta}.\cr
#'
#' First, it keeps only data for the year 2022 up to July 15th.\cr
#'
#' Second, it generates the variables of interest.
#' \describe{
#'  \item{\code{Graves}}{How often the player picked Graves in that \code{day}.}
#'  \item{\code{Graves_ban}}{How often the player banned Graves in that \code{day}.}
#'  \item{\code{win_sum}}{How many matches the player won in that \code{day}.}
#'  \item{\code{n_matches}}{How many matches the player has played in that \code{day}.}
#'  \item{\code{Graves_rate}}{Equals \code{Graves} divided by \code{n_matches}.}
#'  \item{\code{Graves_ban_rate}}{Equals \code{Graves_ban} divided by \code{n_matches}.}
#'  \item{\code{win_rate}}{Equals \code{win_sum} divided by \code{n_matches}.}
#'  \item{\code{gold_avg}}{Average gold earned by the player in that \code{day}.}
#'  \item{\code{kills_avg}}{Average number of kills achieved by the player in that \code{day}.}
#'  \item{\code{assists_avg}}{Average number of assists achieved by the player in that \code{day}.}
#'  \item{\code{deaths_avg}}{Average number of deaths achieved by the player in that \code{day}.}
#'  \item{\code{day_no}}{Numeric version of the \code{day} variable, useful when the data set is loaded in other sessions to get the time right.}
#' }
#'
#' @import dplyr plm lubridate
#'
#' @importFrom data.table fwrite
#'
#' @author Riccardo Di Francesco
#'
#' @seealso \code{\link{pull_lol_data}}, \code{\link{clean_lol_data}}, \code{\link{construct_lol_champion_data}} \code{\link{construct_lol_champion_pooled_data}}
#'
#' @export
construct_lol_player_data <- function(dta) {
  ## Keep only 2022 data.
  cat("Keeping only 2022 data up to July 15th. \n")
    dta <- dta %>%
      dplyr::filter(lubridate::year(day) == 2022) %>%
      dplyr::filter(day < as.POSIXct("2022-07-15"))

  ## Generate variables.
  cat("Generating variables. \n")
  cat("    Interest for Graves (picks and bans). \n")
  picks_bans <- dta %>%
    group_by(player_puiid, day) %>%
    mutate(Graves = sum(champion == 'Graves'),
           Graves_ban = sum(ban == 'Graves')) %>%
    select(player_puiid, day, Graves, Graves_ban) %>%
    distinct(player_puiid, day, .keep_all = TRUE) %>%
    ungroup()

  cat("    Number of matches. \n")
  n_matches <- dta %>%
    group_by(player_puiid, day) %>%
    summarise(n_matches = n_distinct(match_id)) %>%
    ungroup()

  cat("    Total gold, kills, assists, and deaths. \n")
  numeric_covariates <- dta %>%
    group_by(player_puiid, day) %>%
    mutate(gold_sum = sum(gold),
           kills_sum = sum(kills),
           assists_sum = sum(assists),
           deaths_sum = sum(deaths),
           win_sum = sum(win)) %>%
    select(player_puiid, day, gold_sum, kills_sum, assists_sum, deaths_sum, win_sum) %>%
    distinct(player_puiid, day, .keep_all = TRUE) %>%
    ungroup()

  daily_panel <- picks_bans %>%
    left_join(n_matches, by = c("player_puiid", "day")) %>%
    left_join(numeric_covariates, by = c("player_puiid", "day")) %>%
    select(player_puiid, day, Graves, Graves_ban, n_matches, win_sum, gold_sum, kills_sum, assists_sum, deaths_sum)

  cat("    Variables in rates. \n")
  extended_daily_panel <- daily_panel %>%
    mutate(Graves_rate = Graves / n_matches,
           Graves_ban_rate = Graves_ban / n_matches,
           win_rate = win_sum / n_matches,
           gold_avg = gold_sum / n_matches,
           kills_avg = kills_sum / n_matches,
           assists_avg = assists_sum / n_matches,
           deaths_avg = deaths_sum / n_matches) %>%
    select(day, player_puiid, n_matches, Graves_rate, Graves_ban_rate, win_rate, gold_avg, kills_avg, assists_avg, deaths_avg)

  ## Final operations.
  panel <- extended_daily_panel
  panel$day_no <- as.numeric(panel$day)

  panel <- panel %>%
    select(day, day_no, player_puiid, n_matches, Graves_rate, Graves_ban_rate, win_rate, gold_avg, kills_avg, assists_avg, deaths_avg)
  colnames(panel)[3] <- c("id")

  ## Write csv.
  cat("Writing csv file at ", getwd(), ". \n\n", sep = "")
  data.table::fwrite(panel, file = "lol_player_dta.csv", row.names = FALSE)
}
