library(tidyverse)
library(rvest)
library(ncaahoopR)

# get the data
url <- "https://kenpom.com/"
rankings <- url %>%
  html() %>%
  html_nodes(xpath='//*[@id="ratings-table"]') %>%
  html_table()

# data clean up 
rankings <- rankings[[1]]
rankings
rankings <- rankings[-c(43, 42, 84, 85, 126, 127, 168, 169, 210, 211, 252, 253, 294, 295, 336, 337), ]
# View(rankings)
colNames <- colnames(rankings)
colnames(rankings) <- paste(colNames, rankings[1, ], sep ='')
colnames(rankings)[7] = paste(colnames(rankings)[7], 'RANK', sep = ' ')
colnames(rankings)[9] = paste(colnames(rankings)[9], 'RANK', sep = ' ')
colnames(rankings)[11] = paste(colnames(rankings)[11], 'RANK', sep = ' ')
colnames(rankings)[13] = paste(colnames(rankings)[13], 'RANK', sep = ' ')
colnames(rankings)[15] = paste(colnames(rankings)[15], 'RANK', sep = ' ')
colnames(rankings)[17] = paste(colnames(rankings)[17], 'RANK', sep = ' ')
colnames(rankings)[19] = paste(colnames(rankings)[19], 'RANK', sep = ' ')
colnames(rankings)[21] = paste(colnames(rankings)[21], 'RANK', sep = ' ')
rankings <- rankings[-1, ]
rankings$AdjEM <- gsub(pattern = '+', replacement = '', x = rankings$AdjEM, fixed = TRUE)
rankings$AdjEM <- as.numeric(rankings$AdjEM)
rankings$AdjT <- as.numeric(rankings$AdjT)

# filter to big ten
big_ten_rankings <- rankings %>% filter(Conf == 'B10')

# find the standard deviation of big ten games this season
big_team_teams <- big_ten_rankings %>% pull(Team)
master_schedule <- NULL
team_schedule <- NULL
# this takes a while (10-15 mins), but needed
for (i in 1:length(big_team_teams)) {
  team_schedule <- get_team_schedule(season = "2019-20", team.name = big_team_teams[i])
  team_schedule <- team_schedule[complete.cases(team_schedule$Game_ID), ]
  team_big_ten_schedule <- team_schedule[which(team_schedule$Home %in% big_team_teams & 
                                                 team_schedule$Away %in% big_team_teams), ]
  team_big_ten_schedule[, ncol(team_big_ten_schedule) + 1] <- big_team_teams[i]
  master_schedule <- rbind(master_schedule, team_big_ten_schedule)
  team_schedule <- NULL
}
# View(master_schedule)
master_schedule <- master_schedule %>% distinct(Game_ID, .keep_all = TRUE)
master_schedule$Home_Score <- as.numeric(master_schedule$Home_Score)
master_schedule$Away_Score <- as.numeric(master_schedule$Away_Score)
master_schedule <- master_schedule %>% mutate(home_point_diff = Home_Score - Away_Score)
# mean(master_schedule$home_point_diff)
stdev_b10 <- sd(master_schedule$home_point_diff)

# some manipulation
master_schedule <- master_schedule %>% mutate(home_win = ifelse(Home_Score > Away_Score, TRUE, FALSE))

# function for point diff
point_differential <- function(AdjEM_A, AdjEM_B, AdjT_A, AdjT_B) {
  return ((AdjEM_A - AdjEM_B)*(AdjT_A + AdjT_B)/200)
}

# win prob function
win_prob_team_a <- function(spread, standdev = stdev_b10) {
  win_prob <- (1 - pnorm(0.5, mean = spread, sd = standdev)) + # regulation
    (pnorm(0.5, mean = spread, sd = standdev) - pnorm(-0.5, mean = spread, sd = standdev)) # OT
  return(win_prob)
}

# combine functions
point_diff_and_win_prob <- function(AdjEM_A, AdjEM_B, AdjT_A, AdjT_B) { 
  spread_a <- point_differential(AdjEM_A, AdjEM_B, AdjT_A, AdjT_B)
  win_prob <- win_prob_team_a(spread_a)
  return(c(spread_a, win_prob, 1 - win_prob))
}

# see how our model does in the past

df <- NULL
master_predict <- data.frame('Game_ID' = character(0), 'Home_Point_Spread' = numeric(0), 'Home_Win_Prob' = numeric(0))
for (j in 1:nrow(master_schedule)) {
  home_point_spread <- 
    point_diff_and_win_prob(big_ten_rankings %>% filter(Team == master_schedule$Home[j]) %>% pull(AdjEM), 
                            big_ten_rankings %>% filter(Team == master_schedule$Away[j]) %>% pull(AdjEM), 
                            big_ten_rankings %>% filter(Team == master_schedule$Home[j]) %>% pull(AdjT), 
                            big_ten_rankings %>% filter(Team == master_schedule$Away[j]) %>% pull(AdjT))[1]
  home_win_prob <- 
    point_diff_and_win_prob(big_ten_rankings %>% filter(Team == master_schedule$Home[j]) %>% pull(AdjEM), 
                            big_ten_rankings %>% filter(Team == master_schedule$Away[j]) %>% pull(AdjEM), 
                            big_ten_rankings %>% filter(Team == master_schedule$Home[j]) %>% pull(AdjT), 
                            big_ten_rankings %>% filter(Team == master_schedule$Away[j]) %>% pull(AdjT))[2]
  game_id <- master_schedule$Game_ID[j]
  df <- data.frame('Game_ID' = game_id, 'Home_Point_Spread' = home_point_spread, 'Home_Win_Prob' = home_win_prob)
  master_predict <- rbind(master_predict, df)
  df <- NULL
}
master_predict$Game_ID <- as.factor(master_predict$Game_ID)
master_schedule_and_predict <- master_schedule %>% left_join(x = master_predict, by = 'Game_ID')
# View(master_schedule_and_predict)

# naive checking
master_schedule_and_predict <- master_schedule_and_predict %>% 
  mutate(predict_home_win_naive = ifelse(Home_Win_Prob > 0.5, TRUE, FALSE))
master_schedule_and_predict <-master_schedule_and_predict %>% 
  mutate(correct_winner_predicted = ifelse(predict_home_win_naive == home_win, TRUE, FALSE))
sum(master_schedule_and_predict$correct_winner_predicted)/nrow(master_schedule_and_predict)

# simulate games 1000 times and record results
master_sim <- data.frame('Game_ID' = character(0), 'Sim_Home_Wins' = integer(0))
df <- NULL
for (k in 1:nrow(master_schedule_and_predict)) {
  home_team <- master_schedule_and_predict$Home[k]
  away_team <- master_schedule_and_predict$Away[k]
  home_win_prob <- master_schedule_and_predict$Home_Win_Prob[k]
  game_id <- master_schedule_and_predict$Game_ID[k]
  set.seed(25)
  total_home_team_wins <- 0
  for (l in 1:1000) {
    num <- runif(1, min = 0, max = 1)
    if (num <= home_win_prob) {
      total_home_team_wins <- total_home_team_wins + 1
    }
  }
  df <- data.frame('Game_ID' = game_id, 'Sim_Home_Wins' = total_home_team_wins)
  master_sim <- rbind(master_sim, df)
}
master_schedule_predict_sims <- master_schedule_and_predict %>% left_join(x = master_sim, by = 'Game_ID')
master_schedule_predict_sims <- master_schedule_predict_sims %>% 
  mutate(sim_predicted_home_to_win = ifelse(Sim_Home_Wins > 500, TRUE, FALSE))
master_schedule_predict_sims <- master_schedule_predict_sims %>% 
  mutate(sims_predict_correct_winner = ifelse(sim_predicted_home_to_win == home_win, TRUE, FALSE))
sum(master_schedule_predict_sims$sims_predict_correct_winner)/nrow(master_schedule_predict_sims)
# master_schedule_predict_sims %>% dplyr::select(home_win, predict_home_win_naive, sim_predicted_home_to_win, 
#                                                correct_winner_predicted, sims_predict_correct_winner)
# model predicts about 64% of winners corrrectly
# start the analysis/sims
# BRACKET ONE
# first round
michigan_rutgers_game <- point_diff_and_win_prob(big_ten_rankings %>% filter(Team == 'Michigan') %>% pull(AdjEM), 
                                              big_ten_rankings %>% filter(Team == 'Rutgers') %>% pull(AdjEM), 
                                              big_ten_rankings %>% filter(Team == 'Michigan') %>% pull(AdjT), 
                                              big_ten_rankings %>% filter(Team == 'Rutgers') %>% pull(AdjT))
set.seed(25)
total_mich_rut <- 0
for (i in 1:1000) {
  num <- runif(1, min = 0, max = 1)
  if (num <= michigan_rutgers_game[3]) {
    total_mich_rut <- total_mich_rut + 1
  }
}
1000 - total_mich_rut # mich wins 563

minnesota_iowa_game <- point_diff_and_win_prob(big_ten_rankings %>% filter(Team == 'Minnesota') %>% pull(AdjEM), 
                                                 big_ten_rankings %>% filter(Team == 'Iowa') %>% pull(AdjEM), 
                                                 big_ten_rankings %>% filter(Team == 'Minnesota') %>% pull(AdjT), 
                                                 big_ten_rankings %>% filter(Team == 'Iowa') %>% pull(AdjT))
set.seed(25)
total_minn_iowa_wins <- 0
for (i in 1:1000) {
  num <- runif(1, min = 0, max = 1)
  if (num <= minnesota_iowa_game[3]) {
    total_minn_iowa_wins <- total_minn_iowa_wins + 1
  }
}
1000 - total_minn_iowa_wins # (minn wins 515)

osu_purdue_game <- point_diff_and_win_prob(big_ten_rankings %>% filter(Team == 'Ohio St.') %>% pull(AdjEM), 
                                               big_ten_rankings %>% filter(Team == 'Purdue') %>% pull(AdjEM), 
                                               big_ten_rankings %>% filter(Team == 'Ohio St.') %>% pull(AdjT), 
                                               big_ten_rankings %>% filter(Team == 'Purdue') %>% pull(AdjT))
set.seed(25)
total_pur_wins_v_osu <- 0
for (i in 1:1000) {
  num <- runif(1, min = 0, max = 1)
  if (num <= osu_purdue_game[3]) {
    total_pur_wins_v_osu <- (total_pur_wins_v_osu + 1)
  }
}
1000 - total_pur_wins_v_osu # osu wins 592

psu_iu_game <- point_diff_and_win_prob(big_ten_rankings %>% filter(Team == 'Penn St.') %>% pull(AdjEM), 
                                           big_ten_rankings %>% filter(Team == 'Indiana') %>% pull(AdjEM), 
                                           big_ten_rankings %>% filter(Team == 'Penn St.') %>% pull(AdjT), 
                                           big_ten_rankings %>% filter(Team == 'Indiana') %>% pull(AdjT))
set.seed(25)
total_iu_wins_over_psu <- 0
for (i in 1:1000) {
  num <- runif(1, min = 0, max = 1)
  if (num <= psu_iu_game[3]) {
    total_iu_wins_over_psu <- (total_iu_wins_over_psu + 1)
  }
}
1000 - total_iu_wins_over_psu # psu wins 561

# second round
michigan_wisc_game <- point_diff_and_win_prob(big_ten_rankings %>% filter(Team == 'Michigan') %>% pull(AdjEM), 
                                                 big_ten_rankings %>% filter(Team == 'Wisconsin') %>% pull(AdjEM), 
                                                 big_ten_rankings %>% filter(Team == 'Michigan') %>% pull(AdjT), 
                                                 big_ten_rankings %>% filter(Team == 'Wisconsin') %>% pull(AdjT))
set.seed(25)
total_wisc_wins_mich <- 0
for (i in 1:1000) {
  num <- runif(1, min = 0, max = 1)
  if (num <= michigan_wisc_game[3]) {
    total_wisc_wins_mich <- (total_wisc_wins_mich + 1)
  }
}
1000 - total_wisc_wins_mich # mich wins 556

# minnesota wins 
minn_ill_game <- point_diff_and_win_prob(big_ten_rankings %>% filter(Team == 'Minnesota') %>% pull(AdjEM), 
                                         big_ten_rankings %>% filter(Team == 'Illinois') %>% pull(AdjEM), 
                                         big_ten_rankings %>% filter(Team == 'Minnesota') %>% pull(AdjT), 
                                         big_ten_rankings %>% filter(Team == 'Illinois') %>% pull(AdjT))
set.seed(25)
total_ill_wins_min <- 0
for (i in 1:1000) {
  num <- runif(1, min = 0, max = 1)
  if (num <= minn_ill_game[3]) {
    total_ill_wins_min <- (total_ill_wins_min + 1)
  }
}
1000 - total_ill_wins_min # MINN wins 547

osu_msu_game <- point_diff_and_win_prob(big_ten_rankings %>% filter(Team == 'Ohio St.') %>% pull(AdjEM), 
                                           big_ten_rankings %>% filter(Team == 'Michigan St.') %>% pull(AdjEM), 
                                           big_ten_rankings %>% filter(Team == 'Ohio St.') %>% pull(AdjT), 
                                           big_ten_rankings %>% filter(Team == 'Michigan St.') %>% pull(AdjT))
set.seed(25)
total_msu_wins_osu <- 0
for (i in 1:1000) {
  num <- runif(1, min = 0, max = 1)
  if (num <= osu_msu_game[3]) {
    total_msu_wins_osu <- (total_msu_wins_osu + 1)
  }
}
1000 - total_msu_wins_osu # osu wins 496

psu_mary_game <- point_diff_and_win_prob(big_ten_rankings %>% filter(Team == 'Penn St.') %>% pull(AdjEM), 
                                       big_ten_rankings %>% filter(Team == 'Maryland') %>% pull(AdjEM), 
                                       big_ten_rankings %>% filter(Team == 'Penn St.') %>% pull(AdjT), 
                                       big_ten_rankings %>% filter(Team == 'Maryland') %>% pull(AdjT))
set.seed(25)
total_mary_wins_psu <- 0
for (i in 1:1000) {
  num <- runif(1, min = 0, max = 1)
  if (num <= psu_mary_game[3]) {
    total_mary_wins_psu <- (total_mary_wins_psu + 1)
  }
}
1000 - total_mary_wins_psu # penn state wins 461, mary wins 539

# third round, mich minn and msu mary
minn_mich_game <- point_diff_and_win_prob(big_ten_rankings %>% filter(Team == 'Minnesota') %>% pull(AdjEM), 
                                         big_ten_rankings %>% filter(Team == 'Michigan') %>% pull(AdjEM), 
                                         big_ten_rankings %>% filter(Team == 'Minnesota') %>% pull(AdjT), 
                                         big_ten_rankings %>% filter(Team == 'Michigan') %>% pull(AdjT))
set.seed(25)
total_mich_wins_minn <- 0
for (i in 1:1000) {
  num <- runif(1, min = 0, max = 1)
  if (num <= minn_mich_game[3]) {
    total_mich_wins_minn <- (total_mich_wins_minn + 1)
  }
}
1000 - total_mich_wins_minn # MINN wins 485, mich wins 515

msu_mary_game <- point_diff_and_win_prob(big_ten_rankings %>% filter(Team == 'Michigan St.') %>% pull(AdjEM), 
                                         big_ten_rankings %>% filter(Team == 'Maryland') %>% pull(AdjEM), 
                                         big_ten_rankings %>% filter(Team == 'Michigan St.') %>% pull(AdjT), 
                                         big_ten_rankings %>% filter(Team == 'Maryland') %>% pull(AdjT))
set.seed(25)
total_mary_wins_msu <- 0
for (i in 1:1000) {
  num <- runif(1, min = 0, max = 1)
  if (num <= msu_mary_game[3]) {
    total_mary_wins_msu <- (total_mary_wins_msu + 1)
  }
}
1000 - total_mary_wins_msu # msu wins 573

# finals 
msu_mich_game <- point_diff_and_win_prob(big_ten_rankings %>% filter(Team == 'Michigan St.') %>% pull(AdjEM), 
                                         big_ten_rankings %>% filter(Team == 'Michigan') %>% pull(AdjEM), 
                                         big_ten_rankings %>% filter(Team == 'Michigan St.') %>% pull(AdjT), 
                                         big_ten_rankings %>% filter(Team == 'Michigan') %>% pull(AdjT))
set.seed(25)
total_mich_wins_msu <- 0
for (i in 1:1000) {
  num <- runif(1, min = 0, max = 1)
  if (num <= msu_mich_game[3]) {
    total_mich_wins_msu <- (total_mich_wins_msu + 1)
  }
}
1000 - total_mich_wins_msu # msu wins 600

### Now, let's imagine ohio state actually *beats* msu in second round.
# third round would be mich minn (same) and osu maryland
osu_mary_game <- point_diff_and_win_prob(big_ten_rankings %>% filter(Team == 'Ohio St.') %>% pull(AdjEM), 
                                         big_ten_rankings %>% filter(Team == 'Maryland') %>% pull(AdjEM), 
                                         big_ten_rankings %>% filter(Team == 'Ohio St.') %>% pull(AdjT), 
                                         big_ten_rankings %>% filter(Team == 'Maryland') %>% pull(AdjT))
set.seed(25)
total_mary_wins_osu <- 0
for (i in 1:1000) {
  num <- runif(1, min = 0, max = 1)
  if (num <= osu_mary_game[3]) {
    total_mary_wins_osu <- (total_mary_wins_osu + 1)
  }
}
1000 - total_mary_wins_osu # osu wins 555

# final round would be ohio state michigan
osumich_game <- point_diff_and_win_prob(big_ten_rankings %>% filter(Team == 'Ohio St.') %>% pull(AdjEM), 
                                         big_ten_rankings %>% filter(Team == 'Michigan') %>% pull(AdjEM), 
                                         big_ten_rankings %>% filter(Team == 'Ohio St.') %>% pull(AdjT), 
                                         big_ten_rankings %>% filter(Team == 'Michigan') %>% pull(AdjT))
set.seed(25)
total_mich_wins_osu <- 0
for (i in 1:1000) {
  num <- runif(1, min = 0, max = 1)
  if (num <= osumich_game[3]) {
    total_mich_wins_osu <- (total_mich_wins_osu + 1)
  }
}
1000 - total_mich_wins_osu # osu wins 560, wins Big Ten tourney with that

# would osu state be favored against every  opponent in the big ten? 
# set it up
big_ten_rankings_no_osu <- big_ten_rankings %>% filter(Team != 'Ohio St.')

# ready
master_osu_big_ten_spreads <- data.frame('Team_A' = character(0), 'Team_B' = character(0), 'Team_A_Spread' = numeric(0), 
                                         'Team_A_Win_Prob' = numeric(0), 'Team_B_Win_Prob' = numeric(0))
df <- NULL
for (m in 1:nrow(big_ten_rankings_no_osu)) {
  game_info <- point_diff_and_win_prob(big_ten_rankings %>% filter(Team == 'Ohio St.') %>% pull(AdjEM), 
                          big_ten_rankings %>% filter(Team == big_ten_rankings_no_osu$Team[m]) %>% pull(AdjEM), 
                          big_ten_rankings %>% filter(Team == 'Ohio St.') %>% pull(AdjT), 
                          big_ten_rankings %>% filter(Team == big_ten_rankings_no_osu$Team[m]) %>% pull(AdjT))
  df <- data.frame('Team_A' = 'Ohio St.', 'Team_B' = big_ten_rankings_no_osu$Team[m], 'Team_A_Spread' = game_info[1], 
             'Team_A_Win_Prob' = game_info[2], 'Team_B_Win_Prob' = game_info[3])
  master_osu_big_ten_spreads <- rbind(master_osu_big_ten_spreads, df)
}
# View(master_osu_big_ten_spreads)
osu_color <- ncaa_colors$primary_color[which(ncaa_colors$espn_name == "Ohio State")]
ggplot(master_osu_big_ten_spreads, aes(x = Team_B, y = Team_A_Spread)) + geom_bar(stat = 'identity', fill = osu_color) + 
  geom_hline(yintercept = 0) + theme_minimal() +xlab('Big Ten Teams') + ylab('Ohio State Spread') + 
  ggtitle('Ohio State Would Be Favored Against Nearly Every Team in the Conference') + 
  theme(text=element_text(size=18))


