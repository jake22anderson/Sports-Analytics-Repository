library(na.tools)
library(nflscrapR)
library(tidyverse)
library(jsonlite)
library(rvest)
library(selectr)
library(jsonlite)
library(stringr)

### AIR YARDS DATA PREP
df_air <- fromJSON('http://airyards.com/2019/weeks')
ess_data <- df_air %>% group_by(full_name) %>% dplyr::select(full_name, position, team, player_id)
ess_data <- unique(ess_data)
df_air_season <- df_air %>% 
  group_by(full_name) %>% 
  summarize(sum(tar), sum(td), sum(rush_td), sum(rec), sum(rec_yards), sum(rush_yards), sum(yac), sum(air_yards), 
            sum(tm_att), sum(team_air), aypt = sum(air_yards)/sum(tar), racr = sum(rec_yards)/sum(air_yards), 
            target_share = sum(tar)/sum(tm_att))

df_air_season <- left_join(df_air_season, ess_data, by = "full_name")
vec <- which(df_air_season$`sum(tar)` == 0)
# df_air_season_tar_pos is a dataset that contains all players with one or more target on the season
df_air_season_tar_pos <- df_air_season[-vec, ]
df_air_season_tar_pos_wr_only <- df_air_season_tar_pos %>% filter(position == "WR")

# notes to remember: yac_epa penalizes receivers for interceptions
### NFLSCRAPR DATA PREP
season_19_data <- scrape_season_play_by_play(season = 2019, "reg")
season_19_data <- season_19_data %>% mutate(success = ifelse(epa>0, 1, 0))
season_19_data[which(season_19_data$posteam == "LA"), which(colnames(season_19_data) == "posteam")] <- "LAR"
season_19_data[which(season_19_data$defteam == "LA"), which(colnames(season_19_data) == "defteam")] <- "LAR"
season_19_data[which(season_19_data$posteam == "LA"), which(colnames(season_19_data) == "posteam")] <- "LAR"
season_19_data[which(season_19_data$defteam == "LA"), which(colnames(season_19_data) == "defteam")] <- "LAR"
season_19_data[which(season_19_data$side_of_field == "LA"), which(colnames(season_19_data) == "side_of_field")] <- "LAR"
# WRs
rec_epa <- season_19_data %>% 
  group_by(receiver_player_name, receiver_player_id) %>% 
  summarize(mean_total_epa = mean(epa), sum_total_epa = sum(epa), mean_air_epa = mean(air_epa), sum_air_epa = sum(air_epa), 
            mean_yac_epa = mean(yac_epa), sum_yac_epa = sum(yac_epa), mean_comp_air_epa = mean(comp_air_epa), 
            sum_comp_air_epa = sum(comp_air_epa), mean_comp_yac_epa = mean(comp_yac_epa), 
            sum_comp_yac_epa = sum(comp_yac_epa), success_rate = mean(success), num_successes = sum(success), n = n())
colnames(rec_epa)[2] = "player_id"
# QBs
qb_summary <- season_19_data %>% 
  group_by(passer_player_name, posteam, passer_player_id) %>%
  summarize(mean_total_epa = mean(epa, na.rm = TRUE), sum_total_epa = sum(epa, na.rm = TRUE), 
            mean_air_epa = mean(air_epa, na.rm = TRUE), sum_air_epa = sum(air_epa, na.rm = TRUE), 
            mean_yac_epa = mean(yac_epa, na.rm = TRUE), sum_yac_epa = sum(yac_epa, na.rm = TRUE),
            mean_comp_air_epa = mean(comp_air_epa, na.rm = TRUE), sum_comp_air_epa = sum(comp_air_epa, na.rm = TRUE), 
            mean_comp_yac_epa = mean(comp_yac_epa, na.rm = TRUE), sum_comp_yac_epa = sum(comp_yac_epa, na.rm = TRUE), 
            success_rate = mean(success, na.rm = TRUE), num_successes = sum(success, na.rm = TRUE), n = n(), 
            qb_dropbacks = sum(qb_dropback, na.rm = TRUE), qb_scrambles = sum(qb_scramble, na.rm = TRUE), 
            n_short_passes = sum(which(pass_length == "short"), na.rm = TRUE), 
            n_deep_passes = sum(which(pass_length == "deep"), na.rm = TRUE), 
            n_right_passes = sum(which(pass_length == "right"), na.rm = TRUE), 
            n_left_passes = sum(which(pass_length == "left"), na.rm = TRUE),
            n_middle_passes = sum(which(pass_length == "middle"), na.rm = TRUE), 
            air_yards = sum(air_yards, na.rm = TRUE), yac = sum(yards_after_catch, na.rm = TRUE))
qb_summary <- qb_summary %>% filter(!is.na(passer_player_name), n > 10)

### WEB SCRAPE ESPN COLLEGE DATA PREP
webpage <- read_html("http://www.espn.com/nfl/college/_/letter/")
alphabet = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "r", "s", "t", "u", 
             "v", "w", "y")
n <- 1
full_dataset <- NULL
formatted_table <- NULL
for (n in 1:length(alphabet)) {
  webpage <- read_html(paste("http://www.espn.com/nfl/college/_/letter/", alphabet[n], sep = ""))
  formatted_table <- webpage %>%
    html_nodes(xpath='//*[@id="my-players-table"]/div[1]/div/table') %>%
    html_table()
  formatted_table <- formatted_table[[1]]
  full_dataset <- rbind(full_dataset, formatted_table)
  formatted_table <- NULL
}
testing_set <- full_dataset
testing_set[,1] <- gsub(pattern = " Jr.", replacement = "", x = testing_set[,1], ignore.case = TRUE)
testing_set[,1] <- gsub(pattern = " II", replacement = "", x = testing_set[,1], ignore.case = TRUE)
testing_set[,1] <- gsub(pattern = " III", replacement = "", x = testing_set[,1], ignore.case = TRUE)
testing_set[,1] <- gsub(pattern = " IV", replacement = "", x = testing_set[,1], ignore.case = TRUE)
testing_set[,1] <- gsub(pattern = "DJ", replacement = "D.J.", x = testing_set[,1], ignore.case = TRUE)
testing_set[,1] <- gsub(pattern = "DK", replacement = "D.K.", x = testing_set[,1], ignore.case = TRUE)
testing_set[,1] <- gsub(pattern = "JK", replacement = "J.K.", x = testing_set[,1], ignore.case = TRUE)
i <- 1
num_rows <- (nrow(testing_set))
while(i < num_rows) {
  if (testing_set[i,1] == testing_set[i,2]) {
    # gets first player of selected school
    first_row_player_of_school <- i + 1
    # gets the school to be transposed 
    selected_school <- testing_set[i,1]
    # finds all instances of player in first column
    player_row_nums <- which(testing_set[,1] == "PLAYER") 
    # identifies which row to delete
    player_row_num_to_delete <- player_row_nums[1]
    # identifies the next player header to determine when to stop college
    next_player_row_num <- player_row_nums[2]
    if (is.na(next_player_row_num)) {
      next_player_row_num <- nrow(testing_set) + 2
    }
    # deletes player header row
    testing_set <- testing_set[-player_row_num_to_delete, ]
    # finds the rorw of final player row of the current school
    final_row_of_current_school <- next_player_row_num-3
    testing_set[first_row_player_of_school:final_row_of_current_school, 4] <- selected_school
    testing_set <- testing_set[-i, ]
  }
  num_rows <- (nrow(testing_set))
  i <- i + 1
}
colnames(testing_set)[1] = "full_name"
colnames(testing_set)[2] = "full_team_name"
colnames(testing_set)[3] = "full_position_name"
colnames(testing_set)[4] = "College"
# View(testing_air)

### Combining AirYards, College Data, and scrapR
df_air_wr_season_with_college <- left_join(df_air_season_tar_pos_wr_only, testing_set, by = "full_name")
df_air_wr_season_with_college[which(df_air_wr_season_with_college$full_name == "John Ross"), 
                              ncol(df_air_wr_season_with_college)] = "Washington"
df_air_wr_season_with_college[which(df_air_wr_season_with_college$full_name == "Will Fuller"), 
                              ncol(df_air_wr_season_with_college)] = "Notre Dame"
df_air_wr_season_with_college[which(df_air_wr_season_with_college$full_name == "Scott Miller"), 
                              ncol(df_air_wr_season_with_college)] = "Bowling Green"
df_air_wr_season_with_college[which(df_air_wr_season_with_college$full_name == "Ced Wilson"), 
                              ncol(df_air_wr_season_with_college)] = "Boise State"

df_air_wr_season_with_college <- left_join(df_air_wr_season_with_college, rec_epa, by = "player_id")
df_air_wr_season_with_college <- df_air_wr_season_with_college[-which(df_air_wr_season_with_college$full_name == "Trevor Davis")[1], ]
df_air_wr_season_with_college <- df_air_wr_season_with_college[-which(df_air_wr_season_with_college$full_name == "Richie James")[1], ]

df_air_wr_season_with_college$College <- as.factor(df_air_wr_season_with_college$College)
df_air_wr_season_with_college <- df_air_wr_season_with_college %>% mutate(osu = ifelse(College == "Ohio State", 1, 0))
df_air_wr_season_with_college$osu <- as.factor(df_air_wr_season_with_college$osu)
sums_by_school <- df_air_wr_season_with_college %>% 
  group_by(College) %>% 
  summarize(sum_rec_yards = sum(`sum(rec_yards)`, na.rm = TRUE), sum_rec_td = sum(`sum(td)`, na.rm = TRUE), 
            sum_rec = sum(`sum(rec)`, na.rm = TRUE), sum_tar = sum(`sum(tar)`, na.rm = TRUE), 
            sum_yac = sum(`sum(yac)`, na.rm = TRUE), 
            total_catch_rate = sum(`sum(rec)`, na.rm = TRUE)/sum(`sum(tar)`, na.rm = TRUE), 
            total_target_share = sum(`sum(tar)`, na.rm = TRUE)/sum(`sum(tm_att)`, na.rm = TRUE), 
            total_sum_epa = sum(sum_total_epa, na.rm = TRUE), mean_rec_yards = mean(`sum(rec_yards)`, na.rm = TRUE), 
            mean_tds = mean(`sum(td)`, na.rm = TRUE), mean_rec = mean(`sum(rec)`, na.rm = TRUE), 
            mean_tar = mean(`sum(tar)`, na.rm = TRUE), yac_per_player = mean(`sum(yac)`, na.rm = TRUE), 
            mean_total_sum_epa = mean(sum_total_epa, na.rm = TRUE), total_sum_air_epa = sum(sum_air_epa, na.rm = TRUE), 
            mean_total_air_epa = mean(sum_air_epa, na.rm = TRUE), total_sum_yac_epa = sum(sum_yac_epa, na.rm = TRUE), 
            mean_total_yac_epa = mean(sum_yac_epa, na.rm = TRUE), 
            total_sum_air_comp_epa = sum(sum_comp_air_epa, na.rm = TRUE), 
            total_mean_air_comp_epa = mean(sum_comp_air_epa, na.rm = TRUE), 
            total_sum_yac_comp_epa = sum(sum_comp_yac_epa, na.rm = TRUE), 
            total_mean_yac_comp_epa = mean(sum_comp_yac_epa, na.rm = TRUE), 
            num_successes = sum(num_successes, na.rm = TRUE), 
            success_rate = sum(num_successes, na.rm = TRUE)/sum(n, na.rm = TRUE), 
            mean_epa_per_catch = sum(sum_total_epa, na.rm = TRUE)/sum(`sum(rec)`, na.rm = TRUE), 
            mean_epa_per_target = sum(sum_total_epa, na.rm = TRUE)/sum(`sum(tar)`, na.rm = TRUE), 
            racr = mean(sum(`sum(rec_yards)`, na.rm = TRUE)/sum(`sum(air_yards)`, na.rm = TRUE)), 
            adot = sum(`sum(air_yards)`, na.rm = TRUE)/sum(`sum(tar)`, na.rm = TRUE), n())
stats_by_school_three_plus_rec <- sums_by_school %>% filter(`n()` > 2)
stats_schools_more_than_55_tar <- sums_by_school %>% filter(sum_tar >= ceiling(median(sums_by_school$sum_tar)))

### ESPN QBR WEBSCRAPE 
x_path <- '//*[@id="my-players-table"]/div[1]/div[2]/table'
URL_espn <- "http://www.espn.com/nfl/qbr"
webpage <- read_html(URL_espn)
qbr_table <- webpage %>%
  html_nodes(xpath=x_path) %>%
  html_table()
qbr_table <- qbr_table[[1]]
colnames(qbr_table) <- qbr_table[1,]
qbr_table <- qbr_table[-which(qbr_table$RK == "RK"), ]
qbr_table <- qbr_table %>% 
  separate(col = PLAYER, into = c("PLAYER NAME", "posteam"), sep = ", ", remove = TRUE)
lists_of_names <- strsplit(qbr_table$`PLAYER NAME`, " ")
first_letter_first_name <- substring(lists_of_names, 4, 4)
j <- 1
last_names <- NULL
for (j in 1:length(lists_of_names)) {
  last_name_j <- lists_of_names[[j]][2]
  last_names <- c(last_names, last_name_j)
  last_name_j <- NULL
}
full_name_abbr <- NULL
k <- 1
for (k in 1:length(first_letter_first_name)) {
  name_abbr_k <- paste(first_letter_first_name[k], ".", last_names[k], sep ="")
  full_name_abbr <- c(full_name_abbr, name_abbr_k)
}
# only run this line if you are clearing this table prior. 
qbr_table[ , ncol(qbr_table) + 1] <- full_name_abbr
colnames(qbr_table)[ncol(qbr_table)] = "passer_player_name"
qbr_table[which(qbr_table$posteam == "WSH"), which(colnames(qbr_table) == "posteam")] <- "WAS"

### COMBINING ESPN QBR WITH NFLSCRAPR AND AIRYARDS
qb_scrapR_qbr <- left_join(qb_summary, qbr_table, by = c("passer_player_name", "posteam"))
qb_scrapR_qbr_nonzero <- qb_scrapR_qbr %>% filter(!is.na(`TOTAL QBR`))

### WR Plots
# Top Ten Schools in Total Receiving Yards
ggplot(sums_by_school %>% top_n(10, sum_rec_yards), aes(x = College, y = sum_rec_yards)) + 
         geom_bar(stat = "identity", position = "dodge")

# Top Ten Schools (resitricted) in Catch Rate
ggplot(stats_by_school_three_plus_rec %>% top_n(10, total_catch_rate), 
       aes(x = College, y = total_catch_rate)) + 
  geom_bar(stat = "identity", position = "dodge") + ylab("Catch Rate") + ggtitle("Top Ten") + 
  scale_fill_manual(values = c("red4", "blue2", "black1", "blue2", "red1", "red6", "blue5", "blue6", "red8", "gold1"))

# Top Ten Schools by Total EPA
ggplot(sums_by_school %>% top_n(10, total_sum_epa), aes(x = College, y = total_sum_epa, fill = `n()`)) + 
  geom_bar(stat = "identity", position = "dodge")

# Top Ten Schools in Success Rate
ggplot(stats_by_school_three_plus_rec %>% top_n(10, success_rate), aes(x = College, y = success_rate)) + 
  geom_bar(stat = "identity", position = "dodge")

# Total EPA vs Targets, restricted by >= ceiling(median(sums_by_school$sum_tar))
ggplot(stats_schools_more_than_55_tar, aes(x = sum_tar, y = total_sum_epa)) + 
  geom_text(label = stats_schools_more_than_55_tar$College, check_overlap = TRUE) +
  ggtitle("Wide Receivers EPA vs Opportunity") + xlab("Total Targets") + ylab("Total EPA") + theme_minimal() + 
  labs(subtitle = paste("Data restricted to programs with ", ceiling(median(sums_by_school$sum_tar)), 
                        "+ total targets.", sep = "")) +
  geom_hline(yintercept = 0) + geom_smooth(method = "loess", se = FALSE) + theme(text = element_text(size = 17)) + 
  labs(caption = "Data from @AirYards and @nflscrapR.")

# Total EPA vs Targets, no restrictions
ggplot(sums_by_school, aes(x = sum_tar, y = total_sum_epa)) + 
  geom_text(label = sums_by_school$College, check_overlap = TRUE) +
  ggtitle("Wide Receivers EPA vs Opportunity") + xlab("Total Targets") + ylab("Total EPA on All Targets") + 
  theme_minimal() + geom_hline(yintercept = 0) + geom_smooth(method = "loess", se = FALSE) + 
  theme(text = element_text(size = 17)) + labs(caption = "Data from @AirYards and @nflscrapR.")

# EPA per Target vs Targets, restricted by >= ceiling(median(sums_by_school$sum_tar))
ggplot(stats_schools_more_than_55_tar, aes(x = sum_tar, y = total_sum_epa/sum_tar)) + 
  geom_text(label = stats_schools_more_than_55_tar$College, check_overlap = TRUE) +
  ggtitle("Wide Receivers EPA per Target") + xlab("Total Targets") + ylab("EPA/Target") + theme_minimal() + 
  labs(subtitle = paste("Data restricted to programs with ", ceiling(median(sums_by_school$sum_tar)), 
                        "+ total targets.", sep = "")) +
  geom_hline(yintercept = 0) + geom_smooth(method = "loess", se = FALSE) + theme(text = element_text(size = 17)) + 
  labs(caption = "Data from @AirYards and @nflscrapR.")

# Air EPA vs YAC EPA with y=x line and regression line
ggplot(sums_by_school, aes(x = total_sum_yac_comp_epa, y = total_sum_air_comp_epa)) + 
  geom_text(label = sums_by_school$College, check_overlap = TRUE) +
  ggtitle("Air EPA vs YAC EPA by School") + xlab("Total YAC EPA") + ylab("Total Air EPA") + theme_minimal() + 
  geom_abline(slope = 1, intercept = 0) + geom_hline(yintercept = 0) + 
  geom_smooth(method = "loess", se = FALSE) + theme(text = element_text(size = 17)) + 
  labs(caption = "Data from @AirYards and @nflscrapR.") + xlim(-10, 100) + ylim(-10, 100)

# Total Air EPA on completions vs Total YAC EPA on completions with y=x line
ggplot(sums_by_school, aes(x = total_sum_yac_comp_epa, y = total_sum_air_comp_epa)) + 
  geom_text(label = sums_by_school$College, check_overlap = TRUE) +
  ggtitle("Air EPA vs YAC EPA by School") + xlab("Total YAC EPA on Completed Passes") + 
  ylab("Total Air EPA on Completed Passes") + theme_minimal() + 
  geom_hline(yintercept = 0) + geom_abline(slope = 1, intercept = 0) + theme(text = element_text(size = 17)) + 
  labs(caption = "Data from @AirYards and @nflscrapR.") + xlim(-10, 75) + ylim(-10, 100)

# Mean Air EPA on completions vs Mean YAC EPA on completions with y=x line
ggplot(sums_by_school, aes(x = total_sum_yac_comp_epa/sum_rec, y = total_sum_air_comp_epa/sum_rec)) + 
  geom_text(label = sums_by_school$College, check_overlap = TRUE) +
  ggtitle("Mean Air EPA vs YAC EPA by School") + xlab("Mean YAC EPA on Completed Passes") + 
  ylab("Mean Air EPA on Completed Passes") + theme_minimal() + 
  geom_hline(yintercept = 0) + geom_abline(slope = 1, intercept = 0) + theme(text = element_text(size = 17)) + 
  labs(caption = "Data from @AirYards and @nflscrapR.")

### QB PLOTS
# Includes incomplete passes (better)
ggplot(qb_scrapR_qbr_nonzero, aes(x = `TOTAL QBR`, y = mean_air_epa)) + 
  geom_text(label = qb_scrapR_qbr_nonzero$passer_player_name, check_overlap = TRUE)

# Does Not Include Incomplete Passes
ggplot(qb_scrapR_qbr_nonzero, aes(x = `TOTAL QBR`, y = mean_comp_air_epa)) + 
  geom_text(label = qb_scrapR_qbr_nonzero$passer_player_name, check_overlap = TRUE)

