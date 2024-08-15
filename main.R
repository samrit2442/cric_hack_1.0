# Collecting all club competition data ----
library(pacman)
pacman::p_load(tidyverse, plyr, reader, scales)

unzip("./club_cricket/apl_male_csv2.zip", exdir = paste0("./APL"))
unzip("./club_cricket/bbl_male_csv2.zip", exdir = paste0("./BBL"))
unzip("./club_cricket/bpl_male_csv2.zip", exdir = paste0("./BPL"))
unzip("./club_cricket/cpl_male_csv2.zip", exdir = paste0("./CPL"))
unzip("./club_cricket/ctc_male_csv2.zip", exdir = paste0("./CTC"))
unzip("./club_cricket/ipl_male_csv2.zip", exdir = paste0("./IPL"))
unzip("./club_cricket/lpl_male_csv2.zip", exdir = paste0("./LPL"))
unzip("./club_cricket/mlc_male_csv2.zip", exdir = paste0("./MLC"))
unzip("./club_cricket/ntb_male_csv2.zip", exdir = paste0("./NTB"))
unzip("./club_cricket/psl_male_csv2.zip", exdir = paste0("./PSL"))
unzip("./club_cricket/sat_male_csv2.zip", exdir = paste0("./SAT"))

apl <- read.csv("./APL/all_matches.csv")
bbl <- read.csv("./BBL/all_matches.csv")
bpl <- read.csv("./BPL/all_matches.csv")
cpl <- read.csv("./CPL/all_matches.csv")
ctc <- read.csv("./CTC/all_matches.csv")
ipl <- read.csv("./IPL/all_matches.csv")
lpl <- read.csv("./LPL/all_matches.csv")
mlc <- read.csv("./MLC/all_matches.csv")
ntb <- read.csv("./NTB/all_matches.csv")
psl <- read.csv("./PSL/all_matches.csv")
sat <- read.csv("./SAT/all_matches.csv")

# Creating Summary stats of top 15 batters and bowlers of each league ----------

all_wkt <- ipl$wicket_type |> unique()
bowl_wkt <- c("0", "caught", "bowled", "lbw", "stumped", "caught and bowled")

data_clean <- function(dataset) {
  ## Filling the blank entries as NA
  dataset[dataset == ""] <- NA
  
  ## Filling the NAs with 0
  dataset[is.na(dataset)] <- "0"
  
  df <- dataset |>
    tibble::as_tibble() |> 
    dplyr::filter(innings == 1 | innings == 2) |> 
    dplyr::mutate(over = ceiling(ball),
                  isOut = if_else(wicket_type != 0, 1, 0),
                  team_runs = runs_off_bat + extras,
                  year = year(as.Date(start_date))) |> 
    dplyr::select(match_id, year, striker, batting_team, bowling_team, bowler,
                  runs_off_bat, over, ball, extras, wides, noballs, byes, 
                  legbyes, penalty, team_runs, isOut,
                  wicket_type, player_dismissed)
  return(df)
}

master_data <- rbind(apl, bbl, bpl, cpl, ctc, ipl, mlc, ntb, psl, sat)

m_data <- data_clean(master_data)

m_data$batting_team |> unique() |> sort()

# saveRDS(m_data, "m_data.rds")

################################################################################

# m_data <- readRDS("m_data.rds")
  
batter_stat <- function(cleaned_data) {
  df <- cleaned_data |> 
    as_tibble() |> 
    dplyr::group_by(striker, year, batting_team) |> 
    dplyr::summarise(ing_plyd = n_distinct(match_id),
                     runs = sum(runs_off_bat),
                     balls = length(runs_off_bat),
                     runs_per_ing = runs/ing_plyd,
                     strike_rate = runs/balls*100) |> 
    dplyr::ungroup() |> 
    dplyr::filter(ing_plyd >= 8)
  return(df)
}

bowler_stat <- function(cleaned_data) {
  df1 <- cleaned_data |> 
    as_tibble() |> 
    dplyr::filter(noballs == 0 & wides == 0) |> 
    dplyr::group_by(bowler, year, bowling_team) |> 
    dplyr::summarise(balls = length(runs_off_bat))
  
  df2 <- cleaned_data |> 
    dplyr::filter(wicket_type %in% bowl_wkt) |> 
    dplyr::filter(byes == 0 & legbyes == 0 & penalty == 0) |> 
    dplyr::group_by(bowler, year, bowling_team) |> 
    dplyr::summarise(ing_plyd = n_distinct(match_id),
                     runs_conc = sum(team_runs),
                     wkts_per_ing = sum(isOut)/ing_plyd) |> 
    dplyr::inner_join(df1) |> 
    dplyr::mutate(econ = round(runs_conc/balls*6, 2)) |> 
    dplyr::ungroup() |> 
    dplyr::filter(ing_plyd >= 8)
}

m_bat <- batter_stat(m_data)
m_bowl <- bowler_stat(m_data)

# Batter's Table --------------------------------------------------

m_bat_ranked <- m_bat |> 
  dplyr::group_by(year, batting_team) |> 
  dplyr::mutate(R = rank(-runs_per_ing),
                S = rank(-strike_rate),
                `T` = (R + S)/2,
                comb_rank = floor(rank(`T`)),
                norm_score = 1 - rescale(comb_rank),
                N = length(R)) |> 
  dplyr::ungroup() |> 
  dplyr::group_by(striker, year) |> 
  dplyr::mutate(No_teams = length(batting_team)) |> 
  dplyr::ungroup() |> 
  dplyr::filter(No_teams > 1)

m_bat_ranked2 <- m_bat_ranked |> 
  dplyr::arrange(desc(norm_score), desc(N)) |> 
  dplyr::group_by(striker, year) |> 
  dplyr::slice(1)

# Bowler's Table --------------------------------------------------

m_bowl_ranked <- m_bowl |> 
  dplyr::group_by(year, bowling_team) |> 
  dplyr::mutate(R = rank(-wkts_per_ing),
                S = rank(econ),
                `T` = (R + S)/2,
                comb_rank = floor(rank(`T`)),
                norm_score = 1 - rescale(comb_rank),
                N = length(R)) |> 
  dplyr::ungroup() |> 
  dplyr::group_by(bowler, year) |> 
  dplyr::mutate(No_teams = length(bowling_team)) |> 
  dplyr::ungroup() |> 
  dplyr::filter(No_teams > 1)

m_bowl_ranked2 <- m_bowl_ranked |> 
  dplyr::arrange(desc(norm_score), desc(N)) |> 
  dplyr::group_by(bowler, year) |> 
  dplyr::slice(1)






