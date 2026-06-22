#
#
#
#
#
#
#
#
#
#
#
#
#
knitr::opts_chunk$set(
  echo      = TRUE,
  message   = FALSE,
  warning   = FALSE,
  comment   = "#>"
)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
library(animatedplyr)
library(dplyr)
#
#
#
#
#
data(kb_df)   # explicit load (optional but good practice in scripts)
dim(kb_df)
#
#
#
#
#
dplyr::glimpse(kb_df)
#
#
#
#
#
head(kb_df[, c("Season", "Date", "Game_Location",
                    "Game_Outcome", "PTS", "AST", "TRB", "GmSc")], 8)
#
#
#
#
#
summary(kb_df[, c("PTS", "AST", "TRB", "MP", "GmSc")])
#
#
#
#
#
kb_df[which.max(kb_df$PTS),
          c("Date", "Season", "Opp", "Game_Location", "PTS", "FGA", "GmSc")]
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
animate_select(kb_df, Name, Season, TRB, seed = 1)
#
#
#
#
#
#
#
animate_select(kb_df,
               Date, Season, Game_Location, Game_Outcome, Point_Margin,
               seed = 7)
#
#
#
#
#
animate_select(kb_df, PTS, FG, FGA, FG_Percent, FT, FTA, FT_Percent,
               seed = 42)
#
#
#
#
#
#
#
with_animation(kb_df,
               select(Name, Season, PTS, AST, TRB, BLK, STL),
               seed = 10)
#
#
#
#
#
#
#
cfg <- animate_config(duration = 1800, font_size = 15)

animate_select(kb_df, Season, Tm, Opp, PTS, AST, TRB,
               config = cfg, seed = 5)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
animate_filter(kb_df, Game_Location == "Home", seed = 1)
#
#
#
#
#
#
#
animate_filter(nba_goats,
               Game_Location == "Away" & Game_Outcome == "W",
               seed = 3)
#
#
#
#
#
animate_filter(kb_df, PTS >= 40, seed = 42)
#
#
#
#
#
sum(kb_df$PTS >= 40, na.rm = TRUE)
#
#
#
#
#
animate_filter(kb_df, PTS == 81, seed = 1)
#
#
#
#
#
animate_filter(kb_df, DD == 1, seed = 2)
#
#
#
#
#
#
#
animate_filter(kb_df, GmSc >= 30, seed = 6)
#
#
#
#
#
with_animation(kb_df,
               filter(PTS > 30 & Game_Outcome == "W"),
               seed = 9)
#
#
#
#
#
animate_filter(kb_df, Season == "season_8", seed = 5)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
animate_mutate(kb_df, pts_per_attempt = PTS / FGA, seed = 1)
#
#
#
#
#
animate_mutate(kb_df, high_multi = PTS * 30, seed = 4)
#
#
#
#
#
#
#
animate_mutate(kb_df, is_home = Game_Location == "Home", seed = 7)
#
#
#
#
#
#
#
avg_pts <- mean(kb_df$PTS, na.rm = TRUE)
animate_mutate(kb_df, pts_above_avg = PTS - avg_pts, seed = 3)
#
#
#
#
#
animate_mutate(kb_df, scoring_playmaking = PTS + (AST * 2), seed = 2)
#
#
#
#
#
animate_mutate(kb_df,
               result_label = ifelse(Game_Outcome == "W", "Win", "Loss"),
               seed = 5)
#
#
#
#
#
with_animation(kb_df,
               mutate(blk_stl_sum = BLK + STL),
               seed = 8)
#
#
#
#
#
#
#
# Step 1 — narrow the view
animate_select(kb_df, Season, PTS, AST, TRB, FGA, Game_Outcome, seed = 1)
#
#
#
# Step 2 — keep only wins
animate_filter(kb_df, Game_Outcome == "W", seed = 1)
#
#
#
# Step 3 — add an efficiency column
animate_mutate(kb_df, pts_per_fg_attempt = PTS / FGA, seed = 1)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
