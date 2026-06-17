# =============================================================================
# R/data.R — dataset documentation
# =============================================================================

#' Kobe Bryant Game Log
#'
#' A game-by-game log of Kobe Bryant's first 15 NBA regular seasons
#' (1996–2011), totalling 1,156 games. Each row is one game.
#'
#' @format A data frame with 1,156 rows and 38 columns:
#' \describe{
#'   \item{Name}{Character. Player code: \code{"KB"}.}
#'   \item{Last_Name}{Character. \code{"Bryant"}.}
#'   \item{Season}{Character. Season label (\code{"season_1"} through
#'     \code{"season_15"}).}
#'   \item{Game_Location}{Character. \code{"Home"} or \code{"Away"}.}
#'   \item{Game_Outcome}{Character. \code{"W"} (win) or \code{"L"} (loss).}
#'   \item{Point_Margin}{Numeric. Point difference (positive = win,
#'     negative = loss).}
#'   \item{Rk}{Numeric. Rank / game number in sequence.}
#'   \item{G}{Numeric. Games played in season up to this point.}
#'   \item{Date}{Date. Game date.}
#'   \item{Age}{Character. Player age in \code{"years-days"} format.}
#'   \item{Age_Years}{Numeric. Years component of age.}
#'   \item{Age_Days}{Numeric. Days component of age.}
#'   \item{Tm}{Character. Team abbreviation (e.g. \code{"LAL"}).}
#'   \item{Opp}{Character. Opponent abbreviation.}
#'   \item{GS}{Numeric. Games started (1 = started, 0 = bench).}
#'   \item{MP}{Numeric. Minutes played.}
#'   \item{FG}{Numeric. Field goals made.}
#'   \item{FGA}{Numeric. Field goal attempts.}
#'   \item{FG_Percent}{Numeric. Field goal percentage (0-1).}
#'   \item{3P}{Numeric. Three-pointers made.}
#'   \item{3PA}{Numeric. Three-pointers attempted.}
#'   \item{3P_Percent}{Numeric. Three-point percentage (0-1).}
#'   \item{FT}{Numeric. Free throws made.}
#'   \item{FTA}{Numeric. Free throw attempts.}
#'   \item{FT_Percent}{Numeric. Free throw percentage (0-1).}
#'   \item{ORB}{Numeric. Offensive rebounds.}
#'   \item{DRB}{Numeric. Defensive rebounds.}
#'   \item{TRB}{Numeric. Total rebounds.}
#'   \item{AST}{Numeric. Assists.}
#'   \item{STL}{Numeric. Steals.}
#'   \item{BLK}{Numeric. Blocks.}
#'   \item{TOV}{Numeric. Turnovers.}
#'   \item{PF}{Numeric. Personal fouls.}
#'   \item{PTS}{Numeric. Points scored. Ranges from 0 to 81.}
#'   \item{GmSc}{Numeric. Game Score (Hollinger productivity metric).}
#'   \item{number_game}{Numeric. Game number within the season.}
#'   \item{DD}{Numeric. Double-double indicator (1 = yes, 0 = no).}
#'   \item{TD}{Numeric. Triple-double indicator (1 = yes, 0 = no).}
#' }
#'
#' @source Basketball-Reference.com game logs, compiled for pedagogical use.
#'
#' @examples
#' data(kb_df)
#' head(kb_df[, c("Season", "Date", "PTS", "AST", "TRB")])
"kb_df"


#' LeBron James Game Log
#'
#' A game-by-game log of LeBron James's NBA regular-season games, totalling
#' 1,214 games. Each row is one game. Column definitions are identical to
#' \code{\link{kb_df}}. \code{Name} is \code{"LJ"} and \code{Last_Name} is
#' \code{"James"}.
#'
#' @format A data frame with 1,214 rows and 38 columns. See
#'   \code{\link{kb_df}} for the full column descriptions.
#'
#' @source Basketball-Reference.com game logs, compiled for pedagogical use.
#'
#' @examples
#' data(lb_df)
#' head(lb_df[, c("Season", "Date", "PTS", "AST", "TRB")])
"lb_df"


#' Michael Jordan Game Log
#'
#' A game-by-game log of Michael Jordan's NBA regular-season games, totalling
#' 1,030 games. Each row is one game. Column definitions are identical to
#' \code{\link{kb_df}}. \code{Name} is \code{"MJ"} and \code{Last_Name} is
#' \code{"Jordan"}.
#'
#' @format A data frame with 1,030 rows and 38 columns. See
#'   \code{\link{kb_df}} for the full column descriptions.
#'
#' @source Basketball-Reference.com game logs, compiled for pedagogical use.
#'
#' @examples
#' data(mj_df)
#' head(mj_df[, c("Season", "Date", "PTS", "AST", "TRB")])
"mj_df"


#' NBA Variable Names and Descriptions
#'
#' A lookup table describing each column in the player game-log data frames
#' (\code{\link{kb_df}}, \code{\link{lb_df}}, \code{\link{mj_df}}).
#'
#' @format A data frame with 38 rows and 2 columns:
#' \describe{
#'   \item{Variable}{Character. Column name as it appears in the
#'     game-log data frames.}
#'   \item{Description}{Character. Plain-English description of the variable.}
#' }
#'
#' @examples
#' data(variable_names_df)
#' print(variable_names_df)
"variable_names_df"


#' NBA Team Divisions and Conferences
#'
#' A lookup table mapping every NBA team abbreviation to its full name,
#' division, and conference. Useful for joining against the \code{Tm} and
#' \code{Opp} columns in the player game-log data frames.
#'
#' @format A data frame with 39 rows and 4 columns:
#' \describe{
#'   \item{Abbreviation}{Character. Three-letter team abbreviation
#'     (e.g. \code{"LAL"}).}
#'   \item{Full Team Name}{Character. Full franchise name.}
#'   \item{Division}{Character. Division name (e.g.
#'     \code{"Pacific Division"}).}
#'   \item{Conference}{Character. \code{"Eastern Conference"} or
#'     \code{"Western Conference"}.}
#' }
#'
#' @examples
#' data(divisions_df)
#' head(divisions_df)
"divisions_df"
