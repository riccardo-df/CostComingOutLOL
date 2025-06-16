#' Cleaning LoL Data
#'
#' Cleans the raw LoL data as obtained from calling the \code{\link{pull_lol_data}} function.
#'
#' @param dta Data set as constructed by the \code{\link{pull_lol_data}} function (you can rowbind more data sets and pass in the final one).
#'
#' @details
#' \code{\link{clean_lol_data}} performs the following operations on the raw data.\cr
#'
#' First, it keeps data for the period January-July 2022.
#'
#' Second, it aggregates the regions of interest according to the geographical macro area where servers hosting the match are located (Latin America, North America, Europe, and Korea)
#' and drops regions with too many missing values (Oceania, Russia, Japan, and Turkey).\cr
#'
#' Third, it drops duplicated matches and matches with corrupted data (i.e., matches with less than ten players or which lasted more than two hours or
#' where the position of some player is not assigned).\cr
#'
#' Fourth, it hard-codes the 'ban' variable (it maps it from integer numbers to champions' names).\cr
#'
#' Fifth, it identifies and assign the main and the auxiliary positions of each champion based on where they are played the most. It then drops matches where champions were played in weird roles.\cr
#'
#' The raw data are available from the authors on request.
#'
#' @import dplyr lubridate
#'
#' @importFrom data.table fwrite
#'
#' @author Riccardo Di Francesco
#'
#' @seealso \code{\link{pull_lol_data}}, \code{\link{construct_lol_champion_data}}, \code{\link{construct_lol_champion_pooled_data}}, \code{\link{construct_lol_player_data}}
#'
#' @export
clean_lol_data <- function(dta) {
  ## Handle warnings.
  region <- NULL
  match_id <- NULL
  player_puiid <- NULL
  position <- NULL
  champion <- NULL
  main_role <- NULL
  aux_role <- NULL
  in_position <- NULL

  ## Keep only January-July 2022.
  dta <- dta %>%
    dplyr::filter(lubridate::year(day) %in% c(2022)) %>%
    dplyr::filter(day < as.POSIXct("2022-07-15", tryFormats = "%Y-%m-%d"))

  ## Merge regions according to macro area and drop regions with too many missings.
  cat("Merging regions of interest and dropping the others. \n")
  dta$region[dta$region == "Latin_America_South" | dta$region == "Latin_America_North" | dta$region == "Brasil"] <- "Latin_America"
  dta$region[dta$region == "Europe_West" | dta$region == "Europe_Nord_East"] <- "Europe"

  dta_drop <- dta %>%
    dplyr::filter(!(region %in% c("Oceania", "Russia", "Japan", "Turkey")))

  ## Drop duplicate rows (match id/participant name) and corrupted matches.
  cat("Dropping duplicated and corrupted matches. \n")
  dta_noduplicate <- dta_drop %>%
    dplyr::distinct(match_id, player_puiid, .keep_all = TRUE) %>%
    dplyr::group_by(match_id) %>%
    dplyr::filter(n() == 10) %>%
    dplyr::filter(duration <= 120) %>%
    dplyr::filter(position != "") %>%
    dplyr::ungroup()

  ## Drop again matches with less than 10 players (double-check).
  dta_noduplicate <- dta_noduplicate %>%
    dplyr::group_by(match_id) %>%
    dplyr::filter(n() == 10) %>%
    dplyr::ungroup()

  ## Hard-code ban variable. Check https://darkintaqt.com/blog/champ-ids.
  cat("Hard-coding ban variable. \n")
  dta_noduplicate$ban[dta_noduplicate$ban == 266] = "Aatrox"
  dta_noduplicate$ban[dta_noduplicate$ban == 103] = "Ahri"
  dta_noduplicate$ban[dta_noduplicate$ban == 84] = "Akali"
  dta_noduplicate$ban[dta_noduplicate$ban == 166] = "Akshan"
  dta_noduplicate$ban[dta_noduplicate$ban == 12] = "Alistar"
  dta_noduplicate$ban[dta_noduplicate$ban == 32] = "Amumu"
  dta_noduplicate$ban[dta_noduplicate$ban == 34] = "Anivia"
  dta_noduplicate$ban[dta_noduplicate$ban == 1] = "Annie"
  dta_noduplicate$ban[dta_noduplicate$ban == 523] = "Aphelios"
  dta_noduplicate$ban[dta_noduplicate$ban == 22] = "Ashe"
  dta_noduplicate$ban[dta_noduplicate$ban == 136] = "AurelionSol"
  dta_noduplicate$ban[dta_noduplicate$ban == 268] = "Azir"
  dta_noduplicate$ban[dta_noduplicate$ban == 432] = "Bard"
  dta_noduplicate$ban[dta_noduplicate$ban == 200] = "Belveth"
  dta_noduplicate$ban[dta_noduplicate$ban == 53] = "Blitzcrank"
  dta_noduplicate$ban[dta_noduplicate$ban == 63] = "Brand"
  dta_noduplicate$ban[dta_noduplicate$ban == 201] = "Braum"
  dta_noduplicate$ban[dta_noduplicate$ban == 51] = "Caitlyn"
  dta_noduplicate$ban[dta_noduplicate$ban == 164] = "Camille"
  dta_noduplicate$ban[dta_noduplicate$ban == 69] = "Cassiopeia"
  dta_noduplicate$ban[dta_noduplicate$ban == 31] = "Chogath"
  dta_noduplicate$ban[dta_noduplicate$ban == 42] = "Corki"
  dta_noduplicate$ban[dta_noduplicate$ban == 122] = "Darius"
  dta_noduplicate$ban[dta_noduplicate$ban == 131] = "Diana"
  dta_noduplicate$ban[dta_noduplicate$ban == 119] = "Draven"
  dta_noduplicate$ban[dta_noduplicate$ban == 36] = "DrMundo"
  dta_noduplicate$ban[dta_noduplicate$ban == 245] = "Ekko"
  dta_noduplicate$ban[dta_noduplicate$ban == 60] = "Elise"
  dta_noduplicate$ban[dta_noduplicate$ban == 28] = "Evelynn"
  dta_noduplicate$ban[dta_noduplicate$ban == 81] = "Ezreal"
  dta_noduplicate$ban[dta_noduplicate$ban == 9] = "FiddleSticks"
  dta_noduplicate$ban[dta_noduplicate$ban == 114] = "Fiora"
  dta_noduplicate$ban[dta_noduplicate$ban == 105] = "Fizz"
  dta_noduplicate$ban[dta_noduplicate$ban == 3] = "Galio"
  dta_noduplicate$ban[dta_noduplicate$ban == 41] = "Gangplank"
  dta_noduplicate$ban[dta_noduplicate$ban == 86] = "Garen"
  dta_noduplicate$ban[dta_noduplicate$ban == 150] = "Gnar"
  dta_noduplicate$ban[dta_noduplicate$ban == 79] = "Gragas"
  dta_noduplicate$ban[dta_noduplicate$ban == 104] = "Graves"
  dta_noduplicate$ban[dta_noduplicate$ban == 887] = "Gwen"
  dta_noduplicate$ban[dta_noduplicate$ban == 120] = "Hecarim"
  dta_noduplicate$ban[dta_noduplicate$ban == 74] = "Heimerdinger"
  dta_noduplicate$ban[dta_noduplicate$ban == 420] = "Illaoi"
  dta_noduplicate$ban[dta_noduplicate$ban == 39] = "Irelia"
  dta_noduplicate$ban[dta_noduplicate$ban == 427] = "Ivern"
  dta_noduplicate$ban[dta_noduplicate$ban == 40] = "Janna"
  dta_noduplicate$ban[dta_noduplicate$ban == 59] = "JarvanIV"
  dta_noduplicate$ban[dta_noduplicate$ban == 24] = "Jax"
  dta_noduplicate$ban[dta_noduplicate$ban == 126] = "Jayce"
  dta_noduplicate$ban[dta_noduplicate$ban == 202] = "Jhin"
  dta_noduplicate$ban[dta_noduplicate$ban == 222] = "Jinx"
  dta_noduplicate$ban[dta_noduplicate$ban == 145] = "Kaisa"
  dta_noduplicate$ban[dta_noduplicate$ban == 429] = "Kalista"
  dta_noduplicate$ban[dta_noduplicate$ban == 43] = "Karma"
  dta_noduplicate$ban[dta_noduplicate$ban == 30] = "Karthus"
  dta_noduplicate$ban[dta_noduplicate$ban == 38] = "Kassadin"
  dta_noduplicate$ban[dta_noduplicate$ban == 55] = "Katarina"
  dta_noduplicate$ban[dta_noduplicate$ban == 10] = "Kayle"
  dta_noduplicate$ban[dta_noduplicate$ban == 141] = "Kayn"
  dta_noduplicate$ban[dta_noduplicate$ban == 85] = "Kennen"
  dta_noduplicate$ban[dta_noduplicate$ban == 121] = "Khazix"
  dta_noduplicate$ban[dta_noduplicate$ban == 203] = "Kindred"
  dta_noduplicate$ban[dta_noduplicate$ban == 240] = "Kled"
  dta_noduplicate$ban[dta_noduplicate$ban == 96] = "KogMaw"
  dta_noduplicate$ban[dta_noduplicate$ban == 897] = "KSante"
  dta_noduplicate$ban[dta_noduplicate$ban == 7] = "Leblanc"
  dta_noduplicate$ban[dta_noduplicate$ban == 64] = "LeeSin"
  dta_noduplicate$ban[dta_noduplicate$ban == 89] = "Leona"
  dta_noduplicate$ban[dta_noduplicate$ban == 876] = "Lillia"
  dta_noduplicate$ban[dta_noduplicate$ban == 127] = "Lissandra"
  dta_noduplicate$ban[dta_noduplicate$ban == 236] = "Lucian"
  dta_noduplicate$ban[dta_noduplicate$ban == 117] = "Lulu"
  dta_noduplicate$ban[dta_noduplicate$ban == 99] = "Lux"
  dta_noduplicate$ban[dta_noduplicate$ban == 54] = "Malphite"
  dta_noduplicate$ban[dta_noduplicate$ban == 90] = "Malzahar"
  dta_noduplicate$ban[dta_noduplicate$ban == 57] = "Maokai"
  dta_noduplicate$ban[dta_noduplicate$ban == 11] = "MasterYi"
  dta_noduplicate$ban[dta_noduplicate$ban == 902] = "Milio"
  dta_noduplicate$ban[dta_noduplicate$ban == 21] = "MissFortune"
  dta_noduplicate$ban[dta_noduplicate$ban == 62] = "MonkeyKing"
  dta_noduplicate$ban[dta_noduplicate$ban == 82] = "Mordekaiser"
  dta_noduplicate$ban[dta_noduplicate$ban == 25] = "Morgana"
  dta_noduplicate$ban[dta_noduplicate$ban == 950] = "Naafiri"
  dta_noduplicate$ban[dta_noduplicate$ban == 267] = "Nami"
  dta_noduplicate$ban[dta_noduplicate$ban == 75] = "Nasus"
  dta_noduplicate$ban[dta_noduplicate$ban == 111] = "Nautilus"
  dta_noduplicate$ban[dta_noduplicate$ban == 518] = "Neeko"
  dta_noduplicate$ban[dta_noduplicate$ban == 76] = "Nidalee"
  dta_noduplicate$ban[dta_noduplicate$ban == 895] = "Nilah"
  dta_noduplicate$ban[dta_noduplicate$ban == 56] = "Nocturne"
  dta_noduplicate$ban[dta_noduplicate$ban == 20] = "Nunu"
  dta_noduplicate$ban[dta_noduplicate$ban == 2] = "Olaf"
  dta_noduplicate$ban[dta_noduplicate$ban == 61] = "Orianna"
  dta_noduplicate$ban[dta_noduplicate$ban == 516] = "Ornn"
  dta_noduplicate$ban[dta_noduplicate$ban == 80] = "Pantheon"
  dta_noduplicate$ban[dta_noduplicate$ban == 78] = "Poppy"
  dta_noduplicate$ban[dta_noduplicate$ban == 555] = "Pyke"
  dta_noduplicate$ban[dta_noduplicate$ban == 246] = "Qiyana"
  dta_noduplicate$ban[dta_noduplicate$ban == 133] = "Quinn"
  dta_noduplicate$ban[dta_noduplicate$ban == 497] = "Rakan"
  dta_noduplicate$ban[dta_noduplicate$ban == 33] = "Rammus"
  dta_noduplicate$ban[dta_noduplicate$ban == 421] = "RekSai"
  dta_noduplicate$ban[dta_noduplicate$ban == 526] = "Rell"
  dta_noduplicate$ban[dta_noduplicate$ban == 888] = "Renata"
  dta_noduplicate$ban[dta_noduplicate$ban == 58] = "Renekton"
  dta_noduplicate$ban[dta_noduplicate$ban == 107] = "Rengar"
  dta_noduplicate$ban[dta_noduplicate$ban == 92] = "Riven"
  dta_noduplicate$ban[dta_noduplicate$ban == 68] = "Rumble"
  dta_noduplicate$ban[dta_noduplicate$ban == 13] = "Ryze"
  dta_noduplicate$ban[dta_noduplicate$ban == 360] = "Samira"
  dta_noduplicate$ban[dta_noduplicate$ban == 113] = "Sejuani"
  dta_noduplicate$ban[dta_noduplicate$ban == 235] = "Senna"
  dta_noduplicate$ban[dta_noduplicate$ban == 147] = "Seraphine"
  dta_noduplicate$ban[dta_noduplicate$ban == 875] = "Sett"
  dta_noduplicate$ban[dta_noduplicate$ban == 35] = "Shaco"
  dta_noduplicate$ban[dta_noduplicate$ban == 98] = "Shen"
  dta_noduplicate$ban[dta_noduplicate$ban == 102] = "Shyvana"
  dta_noduplicate$ban[dta_noduplicate$ban == 27] = "Singed"
  dta_noduplicate$ban[dta_noduplicate$ban == 14] = "Sion"
  dta_noduplicate$ban[dta_noduplicate$ban == 15] = "Sivir"
  dta_noduplicate$ban[dta_noduplicate$ban == 72] = "Skarner"
  dta_noduplicate$ban[dta_noduplicate$ban == 37] = "Sona"
  dta_noduplicate$ban[dta_noduplicate$ban == 16] = "Soraka"
  dta_noduplicate$ban[dta_noduplicate$ban == 50] = "Swain"
  dta_noduplicate$ban[dta_noduplicate$ban == 517] = "Sylas"
  dta_noduplicate$ban[dta_noduplicate$ban == 134] = "Syndra"
  dta_noduplicate$ban[dta_noduplicate$ban == 223] = "TahmKench"
  dta_noduplicate$ban[dta_noduplicate$ban == 163] = "Taliyah"
  dta_noduplicate$ban[dta_noduplicate$ban == 91] = "Talon"
  dta_noduplicate$ban[dta_noduplicate$ban == 44] = "Taric"
  dta_noduplicate$ban[dta_noduplicate$ban == 17] = "Teemo"
  dta_noduplicate$ban[dta_noduplicate$ban == 412] = "Thresh"
  dta_noduplicate$ban[dta_noduplicate$ban == 18] = "Tristana"
  dta_noduplicate$ban[dta_noduplicate$ban == 48] = "Trundle"
  dta_noduplicate$ban[dta_noduplicate$ban == 23] = "Tryndamere"
  dta_noduplicate$ban[dta_noduplicate$ban == 4] = "TwistedFate"
  dta_noduplicate$ban[dta_noduplicate$ban == 29] = "Twitch"
  dta_noduplicate$ban[dta_noduplicate$ban == 77] = "Udyr"
  dta_noduplicate$ban[dta_noduplicate$ban == 6] = "Urgot"
  dta_noduplicate$ban[dta_noduplicate$ban == 110] = "Varus"
  dta_noduplicate$ban[dta_noduplicate$ban == 67] = "Vayne"
  dta_noduplicate$ban[dta_noduplicate$ban == 45] = "Veigar"
  dta_noduplicate$ban[dta_noduplicate$ban == 161] = "Velkoz"
  dta_noduplicate$ban[dta_noduplicate$ban == 711] = "Vex"
  dta_noduplicate$ban[dta_noduplicate$ban == 254] = "Vi"
  dta_noduplicate$ban[dta_noduplicate$ban == 234] = "Viego"
  dta_noduplicate$ban[dta_noduplicate$ban == 112] = "Viktor"
  dta_noduplicate$ban[dta_noduplicate$ban == 8] = "Vladimir"
  dta_noduplicate$ban[dta_noduplicate$ban == 106] = "Volibear"
  dta_noduplicate$ban[dta_noduplicate$ban == 19] = "Warwick"
  dta_noduplicate$ban[dta_noduplicate$ban == 498] = "Xayah"
  dta_noduplicate$ban[dta_noduplicate$ban == 101] = "Xerath"
  dta_noduplicate$ban[dta_noduplicate$ban == 5] = "XinZhao"
  dta_noduplicate$ban[dta_noduplicate$ban == 157] = "Yasuo"
  dta_noduplicate$ban[dta_noduplicate$ban == 777] = "Yone"
  dta_noduplicate$ban[dta_noduplicate$ban == 83] = "Yorick"
  dta_noduplicate$ban[dta_noduplicate$ban == 350] = "Yuumi"
  dta_noduplicate$ban[dta_noduplicate$ban == 154] = "Zac"
  dta_noduplicate$ban[dta_noduplicate$ban == 238] = "Zed"
  dta_noduplicate$ban[dta_noduplicate$ban == 221] = "Zeri"
  dta_noduplicate$ban[dta_noduplicate$ban == 115] = "Ziggs"
  dta_noduplicate$ban[dta_noduplicate$ban == 26] = "Zilean"
  dta_noduplicate$ban[dta_noduplicate$ban == 142] = "Zoe"
  dta_noduplicate$ban[dta_noduplicate$ban == 143] = "Zyra"
  dta_noduplicate$ban[dta_noduplicate$ban == -1] = "None"

  ## Format dates by dropping hours, minutes and seconds.
  cat("Formatting time variable. \n")
  dta_noduplicate$day <- as.POSIXct(strftime(dta_noduplicate$day, format = "%Y-%m-%d"))

  ## To each champion, assign most-played positions.
  cat("Identifying main and auxiliary positions. \n")
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

  ## Drop again matches with less than ten players (double-check).
  dta_noduplicate <- dta_noduplicate %>%
    dplyr::group_by(match_id) %>%
    dplyr::filter(n() == 10) %>%
    dplyr::ungroup()

  ## Drop useless covariates and order columns.
  dta_final <- dta_noduplicate %>%
    select(c("match_id", "player_name", "player_puiid", "player_level", "day",
             "champion", "position", "kills", "assists", "deaths", "gold", "duration", "early_surrender",
             "surrender", "win", "ban", "region", "main_role", "aux_role"))

  ## Write csv.
  cat("Writing csv file at ", getwd(), ". \n\n", sep = "")
  data.table::fwrite(dta_final, file = "lol_cleaned.csv", row.names = FALSE)
}


#' Construct LoL Champion Regional Data Set
#'
#' Constructs the LoL champion regional data set.
#'
#' @param dta Data set as constructed by the \code{\link{clean_lol_data}} function.
#'
#' @details
#' \code{\link{construct_lol_champion_pooled_data}} performs the following operations on \code{dta}.\cr
#'
#' First, it generates the variables of interest. Notice that NAs may be produced here because of champions never picked or banned in a particular
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
#' Second, it balances the panel by imputing the zeroes for champions that have not been picked in a particular pair \code{region} and \code{day}. This part
#' of the function works in parallel using all but one processors of your computer.\cr
#'
#' Third, it generates additional variables. As before, NAs may be produced and are replaced with zeroes.\cr
#' \describe{
#'  \item{\code{n_matches}}{For each pair \code{region} and \code{day}, count how many matches have been played.}
#'  \item{\code{pick_rate}}{Divide \code{pick_level} by \code{n_matches}.}
#'  \item{\code{ban_rate}}{Divide \code{ban_level} by \code{n_matches}.}
#'  \item{\code{win_rate}}{Divide \code{win_level} by \code{pick_level}.}
#'  \item{\code{day_no}}{Numeric version of the \code{day} variable, useful when the data set is loaded in other sessions to get the time right.}
#' }
#'
#' @import dplyr lubridate parallel doParallel foreach
#'
#' @importFrom data.table fwrite
#' @importFrom plm is.pbalanced
#'
#' @author Riccardo Di Francesco
#'
#' @seealso \code{\link{pull_lol_data}}, \code{\link{clean_lol_data}}, \code{\link{construct_lol_champion_pooled_data}}, \code{\link{construct_lol_player_data}}
#'
#' @export
construct_lol_champion_data <- function(dta) {
  ## Handle warnings.
  region <- NULL
  champion <- NULL
  ban <- NULL
  win <- NULL
  . <- NULL
  match_id <- NULL
  day_no <- NULL
  pick_rate <- NULL
  ban_rate <- NULL
  win_rate <- NULL
  gold_avg <- NULL
  kills_avg <- NULL
  assists_avg <- NULL
  deaths_avg <- NULL
  main_role <- NULL
  aux_role <- NULL
  kills_avg <- NULL

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
    dplyr::mutate(pick_rate = pick_level / n_matches * 100,
           ban_rate = ban_level / n_matches * 100,
           win_rate = win_level / pick_level * 100) %>%
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


#' Construct LoL Champion Pooled Data Set
#'
#' Constructs the LoL champion pooled data set by aggregating observations over regions.
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
#' @import dplyr
#'
#' @importFrom data.table fwrite
#' @importFrom plm is.pbalanced
#'
#' @author Riccardo Di Francesco
#'
#' @seealso \code{\link{pull_lol_data}}, \code{\link{clean_lol_data}}, \code{\link{construct_lol_champion_data}}, \code{\link{construct_lol_player_data}}
#'
#' @export
construct_lol_champion_pooled_data <- function(dta) {
  ## Handle warnings.
  champion <- NULL
  pick_level <- NULL
  ban_level <- NULL
  win_level <- NULL
  n_matches <- NULL
  pick_level_sum <- NULL
  n_matches_sum <- NULL
  ban_level_sum <- NULL
  win_level_sum <- NULL
  gold_avg <- NULL
  kills_avg <- NULL
  assists_avg <- NULL
  deaths_avg <- NULL
  day_no <- NULL
  pick_rate_pooled <- NULL
  ban_rate_pooled <- NULL
  win_rate_pooled <- NULL
  gold_pooled <- NULL
  kills_pooled <- NULL
  assists_pooled <- NULL
  deaths_pooled <- NULL
  . <- NULL

  ## Generate pooled variables.
  cat("Generating pooled variables. \n")

  pooled <- dta %>%
    dplyr::group_by(day, champion) %>%
    dplyr::mutate(pick_level_sum = sum(pick_level),
           ban_level_sum = sum(ban_level),
           win_level_sum = sum(win_level),
           n_matches_sum = sum(n_matches),
           pick_rate_pooled = pick_level_sum / n_matches_sum * 100,
           ban_rate_pooled = ban_level_sum / n_matches_sum * 100,
           win_rate_pooled = win_level_sum / pick_level_sum * 100,
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
#' First, it keeps only players that played at least 50 matches in our data. This is because, due to our data construction, some players
#' might appear only once or twice, and thus information on their behavior is "weak."
#'
#' Second, it generates the variables of interest.
#' \describe{
#'  \item{\code{n_matches}}{How many matches the player has played in that \code{day}.}
#'  \item{\code{n_hours}}{How many hours the player has played in that \code{day}.}
#'  \item{\code{graves_rate}}{Daily percentage of matches where the player picked \code{Graves} out of their \code{n_matches}.}
#'  \item{\code{graves_ban_rate}}{Daily percentage of matches where the player banned \code{Graves} out of their \code{n_matches}.}
#'  \item{\code{belveth_rate}}{Daily percentage of matches where the player picked \code{Belveth} out of their \code{n_matches}.}
#'  \item{\code{belveth_ban_rate}}{Daily percentage of matches where the player banned \code{Belveth} out of their \code{n_matches}.}
#'  \item{\code{top_rate}}{Daily percentage of matches where the player picked a top champion out of their \code{n_matches}.}
#'  \item{\code{jungle_rate}}{Daily percentage of matches where the player picked a jungle champion out of their \code{n_matches}.}
#'  \item{\code{mid_rate}}{Daily percentage of matches where the player picked a mid champion out of their \code{n_matches}.}
#'  \item{\code{bottom_rate}}{Daily percentage of matches where the player picked a bottom champion out of their \code{n_matches}.}
#'  \item{\code{support_rate}}{Daily percentage of matches where the player picked a support champion out of their \code{n_matches}.}
#'  \item{\code{lgb_rate}}{Daily percentage of matches where the player picked an LGB champion ("Diana", "Leona", "Nami", "Neeko") out of their \code{n_matches}.}
#'  \item{\code{win_rate}}{Daily percentage of matches won by the player out of their \code{n_matches}.}
#'  \item{\code{gold_avg}}{Average gold earned by the player in that \code{day}.}
#'  \item{\code{kills_avg}}{Average number of kills achieved by the player in that \code{day}.}
#'  \item{\code{assists_avg}}{Average number of assists achieved by the player in that \code{day}.}
#'  \item{\code{deaths_avg}}{Average number of deaths achieved by the player in that \code{day}.}
#'  \item{\code{day_no}}{Numeric version of the \code{day} variable, useful when the data set is loaded in other sessions to get the time right.}
#' }
#'
#' @import dplyr lubridate
#'
#' @importFrom data.table fwrite
#' @importFrom plm is.pbalanced
#'
#' @author Riccardo Di Francesco
#'
#' @seealso \code{\link{pull_lol_data}}, \code{\link{clean_lol_data}}, \code{\link{construct_lol_champion_data}} \code{\link{construct_lol_champion_pooled_data}}
#'
#' @export
construct_lol_player_data <- function(dta) {
  ## Handle warnings.
  player_puiid <- NULL
  champion <- NULL
  ban <- NULL
  graves <- NULL
  graves_ban <- NULL
  match_id <- NULL
  gold <- NULL
  kills <- NULL
  assists <- NULL
  deaths <- NULL
  win <- NULL
  gold_sum <- NULL
  kills_sum <- NULL
  assists_sum <- NULL
  deaths_sum <- NULL
  win_sum <- NULL
  graves_rate <- NULL
  graves_ban_rate <- NULL
  win_rate <- NULL
  gold_avg <- NULL
  kills_avg <- NULL
  assists_avg <- NULL
  deaths_avg <- NULL
  day_no <- NULL

  ## Keep players with at least 50 matches.
  cat("Keep players with at least 50 matches. \n")
  dta <- dta %>%
    dplyr::group_by(player_puiid) %>%
    dplyr::summarise(n_matches = n_distinct(match_id),
                     .groups = "drop") %>%
    dplyr::filter(n_matches >= 50) %>%
    dplyr::select(-n_matches)

  ## Generate variables.
  cat("Generating variables. \n")
  cat("    Preferences for champions (picks and bans) and roles. \n")
  picks_bans <- dta %>%
    dplyr::group_by(player_puiid, day) %>%
    dplyr::mutate(graves = sum(champion == 'Graves'),
                  graves_ban = sum(ban == 'Graves'),
                  belveth = sum(champion == 'Belveth'),
                  belveth_ban = sum(ban == 'Belveth'),
                  top = sum(main_role == "TOP"),
                  jungle = sum(main_role == "JUNGLE"),
                  mid = sum(main_role == "MIDDLE"),
                  bottom = sum(main_role == "BOTTOM"),
                  support = sum(main_role == "UTILITY"),
                  lgb = sum(champion %in% c("Diana", "Leona", "Nami", "Neeko"))) %>% # https://gaymingmag.com/2023/05/every-lgbtq-character-in-league-of-legends/
    dplyr::select(player_puiid, day, graves, graves_ban, belveth, belveth_ban, top, jungle, mid, bottom, support, lgb) %>%
    dplyr::distinct(player_puiid, day, .keep_all = TRUE) %>%
    dplyr::ungroup()

  cat("    Number of matches and hours played. \n")
  n_matches <- dta %>%
    dplyr::group_by(player_puiid, day) %>%
    dplyr::summarise(n_matches = n_distinct(match_id),
                     n_hours = sum(duration) / 60,
                     .groups = "drop")

  cat("    Total gold, kills, assists, and deaths. \n")
  numeric_covariates <- dta %>%
    dplyr::group_by(player_puiid, day) %>%
    dplyr::mutate(gold_sum = sum(gold),
                  kills_sum = sum(kills),
                  assists_sum = sum(assists),
                  deaths_sum = sum(deaths),
                  win_sum = sum(win)) %>%
    dplyr::select(player_puiid, day, gold_sum, kills_sum, assists_sum, deaths_sum, win_sum) %>%
    dplyr::distinct(player_puiid, day, .keep_all = TRUE) %>%
    dplyr::ungroup()

  daily_panel <- picks_bans %>%
    dplyr::left_join(n_matches, by = c("player_puiid", "day")) %>%
    dplyr::left_join(numeric_covariates, by = c("player_puiid", "day")) %>%
    dplyr::select(player_puiid, day, graves, graves_ban, belveth, belveth_ban, top, jungle, mid, bottom, support, lgb, n_matches, n_hours, win_sum, gold_sum, kills_sum, assists_sum, deaths_sum)

  cat("    Variables in rates. \n")
  extended_daily_panel <- daily_panel %>%
    dplyr::mutate(graves_rate = graves / n_matches * 100,
                  graves_ban_rate = graves_ban / n_matches * 100,
                  belveth_rate = belveth / n_matches * 100,
                  belveth_ban_rate = belveth_ban / n_matches * 100,
                  top_rate = top / n_matches * 100,
                  jungle_rate = jungle / n_matches * 100,
                  mid_rate = mid / n_matches * 100,
                  bottom_rate = bottom / n_matches * 100,
                  support_rate = support / n_matches * 100,
                  lgb_rate = lgb / n_matches * 100,
                  win_rate = win_sum / n_matches * 100,
                  gold_avg = gold_sum / n_matches,
                  kills_avg = kills_sum / n_matches,
                  assists_avg = assists_sum / n_matches,
                  deaths_avg = deaths_sum / n_matches) %>%
    dplyr::select(day, player_puiid, n_matches, n_hours,
                  graves_rate, graves_ban_rate, belveth_rate, belveth_ban_rate,
                  win_rate, gold_avg, kills_avg, assists_avg, deaths_avg,
                  top_rate, jungle_rate, mid_rate, bottom_rate, support_rate_rate, lgb_rate)

  ## Final operations.
  panel <- extended_daily_panel
  panel$day_no <- as.numeric(panel$day)

  panel <- panel %>%
    dplyr::select(day, day_no, player_puiid, n_matches, n_hours,
                  graves_rate, graves_ban_rate, belveth_rate, belveth_ban_rate,
                  top_rate, jungle_rate, mid_rate, bottom_rate, support_rate, lgb_rate,
                  win_rate, gold_avg, kills_avg, assists_avg, deaths_avg)
  colnames(panel)[3] <- c("id")

  ## Write csv.
  cat("Writing csv file at ", getwd(), ". \n\n", sep = "")
  data.table::fwrite(panel, file = "lol_player_dta.csv", row.names = FALSE)
}


#' Construct LoL Match Data Set
#'
#' Constructs the LoL macth data set.
#'
#' @param dta Data set as constructed by the \code{\link{clean_lol_data}} function.
#'
#' @details
#' \code{\link{construct_lol_match_data}} performs the following operations on \code{dta}.\cr
#'
#' First, it codes team identifiers (1 or 2) and generates the variables of interest.
#' \describe{
#'  \item{\code{victory}}{Whether \code{team} won \code{match_id} (team-level).}
#'  \item{\code{surrender}}{Whether \code{match_id} ended by a surrender (match-level).}
#'  \item{\code{early_surrender}}{Whether \code{match_id} ended by an early surrender (match-level).}
#'  \item{\code{duration}}{Duration of \code{match_id} in minutes (match-level).}
#'  \item{\code{Graves}}{Whether someone in \code{team} has picked \code{Graves} (team-level).}
#'  \item{\code{day_no}}{Numeric version of the \code{day} variable, useful when the data set is loaded in other sessions to get the time right.}
#' }
#'
#' @import dplyr lubridate
#'
#' @importFrom data.table fwrite
#' @importFrom plm is.pbalanced
#'
#' @author Riccardo Di Francesco
#'
#' @seealso \code{\link{pull_lol_data}}, \code{\link{clean_lol_data}}, \code{\link{construct_lol_champion_data}} \code{\link{construct_lol_champion_pooled_data}}
#'
#' @export
construct_lol_match_data <- function(dta) {
  ## Handle warnings.
  player_puiid <- NULL
  champion <- NULL
  victory <- NULL
  surrender <- NULL
  early_surrender <- NULL
  duration <- NULL
  Graves <- NULL
  match_id <- NULL
  day_no <- NULL

  ## Generate variables.
  cat("Generating variables. \n")
  is_graves_picked <- dta %>%
    dplyr::rename(team = win) %>%
    dplyr::group_by(match_id, team) %>% # Grouping by "win" after grouping by "match_id" is equivalent to group by team.
    dplyr::summarise(day = first(day),
                     victory = first(team),
                     surrender = first(surrender),
                     early_surrender = first(early_surrender),
                     duration = first(duration),
                     Graves = any(champion == "Graves"),
                     .groups = "drop")

  ## Final operations.
  panel <- is_graves_picked
  panel$day_no <- as.numeric(panel$day)

  ## Write csv.
  cat("Writing csv file at ", getwd(), ". \n\n", sep = "")
  data.table::fwrite(panel, file = "lol_match_dta.csv", row.names = FALSE)
}
