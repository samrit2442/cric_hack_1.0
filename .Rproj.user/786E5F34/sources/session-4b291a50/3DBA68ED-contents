# Collecting all club competition data ----
library(pacman)
pacman::p_load(tidyverse, plyr, reader)

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

common_plyr <- inner_join(distinct(apl, striker),
                          distinct(bbl, striker)) |> 
  inner_join(distinct(bpl, striker)) |> 
  inner_join(distinct(cpl, striker)) |> 
  inner_join(distinct(ctc, striker)) |> 
  inner_join(distinct(ipl, striker)) |> 
  inner_join(distinct(lpl, striker)) |> 
  inner_join(distinct(mlc, striker)) |> 
  inner_join(distinct(ntb, striker)) |> 
  inner_join(distinct(psl, striker)) |> 
  inner_join(distinct(sat, striker))

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
    dplyr::select(-season, -other_wicket_type, -other_player_dismissed) |> 
    dplyr::filter(innings == 1 | innings == 2) |> 
    dplyr::mutate(over = ceiling(ball),
                  over_type = case_when(
                    over >= 1 & over <= 6 ~ "Powerplay",
                    over >= 7 & over <= 16 ~ "Middle Over",
                    over >= 17 ~ "Death Over",
                    .default = "over")) |> 
    dplyr::mutate(isDot = if_else(runs_off_bat == 0, 1, 0),
                  isFour = if_else(runs_off_bat == 4, 1, 0),
                  isSix = if_else(runs_off_bat == 6, 1, 0),
                  isOut = if_else(wicket_type != 0, 1, 0),
                  team_runs = runs_off_bat + extras,
                  year = year(as.Date(start_date)))
  return(df)
}

apl2 <- data_clean(apl)
bbl2 <- data_clean(bbl)
ipl2 <- data_clean(ipl)

batter_stat <- function(cleaned_data) {
  df <- cleaned_data |> 
    as_tibble() |> 
    dplyr::group_by(striker, year, batting_team) |> 
    dplyr::summarise(ing_plyd = n_distinct(match_id),
                     runs = sum(runs_off_bat),
                     balls = length(runs_off_bat),
                     runs_per_ing = round(runs/ing_plyd, 2),
                     strike_rate = round(runs/balls*100, 2))
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
                     wkts = sum(isOut)) |> 
    dplyr::inner_join(df1) |> 
    dplyr::mutate(eco = round(runs_conc/balls*6, 2))
}


ipl_bat <- batter_stat(ipl2)
ipl_bowl <- bowler_stat(ipl2)




