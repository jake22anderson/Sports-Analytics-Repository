# install.packages("rjson")
# install.packages("jpeg")
# install.packages("grid")
# install.packages("RCurl")
library(rjson)
library(jpeg)
library(grid)
library(ggplot2)
library(RCurl)
library(tidyverse)
library(dplyr)
library(moderndive)
# shot data for Stephen Curry
playerID <- 201939
shotURL <- paste("https://stats.nba.com/stats/shotchartdetail?CFID=33&CFPARAMS=2018-19&ContextFilter=&ContextMeasure=FGA&DateFrom=&DateTo=&GameID=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerID=",playerID,"&PlayerPosition=&PlusMinus=N&Position=&Rank=N&RookieYear=&Season=2018-19&SeasonSegment=&SeasonType=Regular+Season&TeamID=0&VsConference=&VsDivision=&mode=Advanced&showDetails=0&showShots=1&showZones=0", sep = "")
# import from JSON
shotData <- fromJSON(file = shotURL, method="C")
# unlist shot data, save into a data frame
curry_data_18_19 <- data.frame(matrix(unlist(shotData$resultSets[[1]][[3]]), ncol=24, byrow = TRUE))

# shot data headers
colnames(curry_data_18_19) <- shotData$resultSets[[1]][[2]]

# covert x and y coordinates into numeric
curry_data_18_19$LOC_X <- as.numeric(as.character(curry_data_18_19$LOC_X))
curry_data_18_19$LOC_Y <- as.numeric(as.character(curry_data_18_19$LOC_Y))
curry_data_18_19$SHOT_DISTANCE <- as.numeric(as.character(curry_data_18_19$SHOT_DISTANCE))

# View(curry_data_18_19)

# simple plot using EVENT_TYPE to colour the dots
# ggplot(curry_data_18_19, aes(x=LOC_X, y=LOC_Y)) +
# geom_point(aes(colour = EVENT_TYPE))

# half court image
courtImg.URL <- "https://thedatagame.files.wordpress.com/2016/03/nba_court.jpg"
court <- rasterGrob(readJPEG(getURLContent(courtImg.URL)),
                    width=unit(1,"npc"), height=unit(1,"npc"))

# plot using NBA court background and colour by shot zone
ggplot(main = "Steph Curry's Shot Chart 18-19", curry_data_18_19, aes(x=LOC_X, y=LOC_Y, color = EVENT_TYPE)) + 
  annotation_custom(court, -250, 250, -50, 420) +
  geom_point() +
  xlim(-250, 250) +
  ylim(-50, 420)

made_curry_data_18_19 <- curry_data_18_19 %>% filter(EVENT_TYPE == "Made Shot")

ggplot(main = "Steph Curry's Made Shot Chart 18-19", made_curry_data_18_19, aes(x=LOC_X, y=LOC_Y, color = SHOT_TYPE)) + 
  annotation_custom(court, -250, 250, -50, 420) +
  geom_point() +
  xlim(-250, 250) +
  ylim(-50, 420)

made_curry_data_by_zone_18_19 <- made_curry_data_18_19 %>% group_by(SHOT_ZONE_BASIC)
# View(made_curry_data_by_zone_18_19)
# changing above the break three into top of key, left wing, and right wing
made_curry_data_top_key_three_18_19 <- made_curry_data_by_zone_18_19 %>% filter(SHOT_ZONE_BASIC == "Above the Break 3")
# View(made_curry_shots_top_key_three_18_19)

# preferred plot for showing different zones
ggplot(main = "Steph Curry's Shot Chart 18-19 by Zone", made_curry_data_18_19 %>% group_by(SHOT_ZONE_BASIC), aes(x=LOC_X, y=LOC_Y, color = SHOT_ZONE_BASIC)) + 
  annotation_custom(court, -250, 250, -50, 420) +
  geom_point() +
  xlim(-250, 250) +
  ylim(-50, 420)


i = 1
for (i in 1:nrow(made_curry_data_top_key_three_18_19)) {
  if (made_curry_data_top_key_three_18_19$SHOT_ZONE_AREA[i] == "Left Side Center(LC)") {
  made_curry_data_top_key_three_18_19$SHOT_ZONE_BASIC[i] <- as.character("Left Wing 3")
  } else if (made_curry_data_top_key_three_18_19$SHOT_ZONE_AREA[i] == "Right Side Center(RC)") {
  made_curry_data_top_key_three_18_19$SHOT_ZONE_BASIC[i] <- as.character("Right Wing 3")
  } 
}

# View(made_curry_data_top_key_three_18_19)

# made_curry_shots_top_key_three_18_19$SHOT_ZONE_BASIC <- factor(made_curry_shots_top_key_three_18_19$SHOT_ZONE_BASIC)
# made_curry_shots_top_key_three_18_19$SHOT_ZONE_BASIC[made_curry_shots_top_key_three_18_19$SHOT_ZONE_AREA == "Left Side Center(LC)"] <- "Left Wing 3"

# made_curry_data_by_zone_and_grouped_18_19 %>% summarize(mean_x = mean(LOC_X), mean_y = mean(LOC_Y), sd_x = sd(LOC_X), sd_y = sd(LOC_Y))

ggplot(main = "Steph Curry's Shot Chart 18-19 by Zone", made_curry_data_by_zone_and_grouped_18_19, aes(x=mean_x, y=mean_y)) + 
  annotation_custom(court, -250, 250, -50, 420) +
  geom_point() +
  xlim(-250, 250) +
  ylim(-50, 420)

# can i predict probability of making a shot based only on location and type of shot?

curry_regression <- (glm(SHOT_MADE_FLAG ~ SHOT_ZONE_BASIC * ACTION_TYPE, data = curry_data_18_19, family = "binomial"))

(summary(curry_regression))

anova(curry_regression)
curry_regression1 <- !is.na(curry_regression)
