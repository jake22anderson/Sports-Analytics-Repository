library(rjson)
library(jpeg)
library(grid)
library(ggplot2)
library(RCurl)
# shot data for Stephen Curry
playerID <- 201939
shotURL <- paste("http://stats.nba.com/stats/shotchartdetail?CFID=33&CFPARAMS=2014-15&ContextFilter=&ContextMeasure=FGA&DateFrom=&DateTo=&GameID=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerID=",playerID,"&PlayerPosition=&PlusMinus=N&Position=&Rank=N&RookieYear=&Season=2014-15&SeasonSegment=&SeasonType=Regular+Season&TeamID=0&VsConference=&VsDivision=&mode=Advanced&showDetails=0&showShots=1&showZones=0", sep = "")
# import from JSON
shotData <- fromJSON(file = shotURL, method="C")

# unlist shot data, save into a data frame
shotDataf <- data.frame(matrix(unlist(shotData$resultSets[[1]][[3]]), ncol=24, byrow = TRUE))

# shot data headers
colnames(shotDataf) <- shotData$resultSets[[1]][[2]]

# covert x and y coordinates into numeric
shotDataf$LOC_X <- as.numeric(as.character(shotDataf$LOC_X))
shotDataf$LOC_Y <- as.numeric(as.character(shotDataf$LOC_Y))
shotDataf$SHOT_DISTANCE <- as.numeric(as.character(shotDataf$SHOT_DISTANCE))

# have a look at the data
View(shotDataf)

# simple plot using EVENT_TYPE to colour the dots
ggplot(shotDataf, aes(x=LOC_X, y=LOC_Y)) +
  geom_point(aes(colour = EVENT_TYPE))

index2 = 0
index3 = 0
index = 1
for (index in 1:1341) {
  if (shotDataf$SHOT_TYPE[index] == ("2PT Field Goal")) {
    index2 = index2 + 1
  } 
  if (shotDataf$SHOT_TYPE[index] == ("3PT Field Goal")) {
    index3= index3 + 1
  }
}
index2
index3
index

# half court image
courtImg.URL <- "https://thedatagame.files.wordpress.com/2016/03/nba_court.jpg"
court <- rasterGrob(readJPEG(getURLContent(courtImg.URL)),
                    width=unit(1,"npc"), height=unit(1,"npc"))

# plot using NBA court background and colour by shot zone
ggplot(main = "Steph Curry's Shot Chart 14-15", shotDataf, aes(x=LOC_X, y=LOC_Y)) + 
  annotation_custom(court, -250, 250, -50, 420) +
  geom_point(aes(colour = EVENT_TYPE, shape = SHOT_TYPE)) +
  xlim(-250, 250) +
  ylim(-50, 420)


# 15-16
playerID <- 201939
shotURL <- paste("https://stats.nba.com/stats/shotchartdetail?CFID=33&CFPARAMS=2015-16&ContextFilter=&ContextMeasure=FGA&DateFrom=&DateTo=&GameID=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerID=201939&PlayerPosition=&PlusMinus=N&Position=&Rank=N&RookieYear=&Season=2015-16&SeasonSegment=&SeasonType=Regular+Season&TeamID=0&VsConference=&VsDivision=&mode=Advanced&showDetails=0&showShots=1&showZones=0", sep = "")
# import from JSON
shotData <- fromJSON(file = shotURL, method="C")

# unlist shot data, save into a data frame
shotData15_16 <- data.frame(matrix(unlist(shotData$resultSets[[1]][[3]]), ncol=24, byrow = TRUE))

# shot data headers
colnames(shotData15_16) <- shotData$resultSets[[1]][[2]]

# covert x and y coordinates into numeric
shotData15_16$LOC_X <- as.numeric(as.character(shotData15_16$LOC_X))
shotData15_16$LOC_Y <- as.numeric(as.character(shotData15_16$LOC_Y))
shotData15_16$SHOT_DISTANCE <- as.numeric(as.character(shotData15_16$SHOT_DISTANCE))

# have a look at the data
View(shotData15_16)

# simple plot using EVENT_TYPE to colour the dots
ggplot(shotData15_16, aes(x=LOC_X, y=LOC_Y)) +
  geom_point(aes(colour = EVENT_TYPE))

index2 = 0
index3 = 0
index = 1
for (index in 1:1596) {
  if (shotData15_16$SHOT_TYPE[index] == ("2PT Field Goal")) {
    index2 = index2 + 1
  } 
  if (shotData15_16$SHOT_TYPE[index] == ("3PT Field Goal")) {
    index3= index3 + 1
  }
}
index2
index3
index

# half court image
courtImg.URL <- "https://thedatagame.files.wordpress.com/2016/03/nba_court.jpg"
court <- rasterGrob(readJPEG(getURLContent(courtImg.URL)),
                    width=unit(1,"npc"), height=unit(1,"npc"))

# plot using NBA court background and colour by make/miss, shape by 2/3
ggplot(main = "Steph Curry's Shot Chart 15-16", shotData15_16, aes(x=LOC_X, y=LOC_Y)) + 
  annotation_custom(court, -250, 250, -50, 420) +
  geom_point(aes(colour = EVENT_TYPE, shape = SHOT_TYPE)) +
  xlim(-250, 250) +
  ylim(-50, 420)

# 18-19
playerID <- 201939
shotURL <- paste("https://stats.nba.com/stats/shotchartdetail?CFID=33&CFPARAMS=2018-19&ContextFilter=&ContextMeasure=FGA&DateFrom=&DateTo=&GameID=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerID=201939&PlayerPosition=&PlusMinus=N&Position=&Rank=N&RookieYear=&Season=2018-19&SeasonSegment=&SeasonType=Regular+Season&TeamID=0&VsConference=&VsDivision=&mode=Advanced&showDetails=0&showShots=1&showZones=0", sep = "")
# import from JSON
shotData <- fromJSON(file = shotURL, method="C")

# unlist shot data, save into a data frame
shotData18_19 <- data.frame(matrix(unlist(shotData$resultSets[[1]][[3]]), ncol=24, byrow = TRUE))

# shot data headers
colnames(shotData18_19) <- shotData$resultSets[[1]][[2]]

# covert x and y coordinates into numeric
shotData18_19$LOC_X <- as.numeric(as.character(shotData18_19$LOC_X))
shotData18_19$LOC_Y <- as.numeric(as.character(shotData18_19$LOC_Y))
shotData18_19$SHOT_DISTANCE <- as.numeric(as.character(shotData18_19$SHOT_DISTANCE))

# have a look at the data
View(shotData18_19)

# simple plot using EVENT_TYPE to colour the dots
ggplot(shotData18_19, aes(x=LOC_X, y=LOC_Y)) +
  geom_point(aes(colour = EVENT_TYPE))

index2 = 0
index3 = 0
index = 1
for (index in seq_along(shotData18_19)) {
  if (shotData18_19$SHOT_TYPE[index] == ("2PT Field Goal")) {
    index2 = index2 + 1
  } 
  if (shotData18_19$SHOT_TYPE[index] == ("3PT Field Goal")) {
    index3= index3 + 1
  }
}
index2
index3
index

# half court image
courtImg.URL <- "https://thedatagame.files.wordpress.com/2016/03/nba_court.jpg"
court <- rasterGrob(readJPEG(getURLContent(courtImg.URL)),
                    width=unit(1,"npc"), height=unit(1,"npc"))

# plot using NBA court background and colour by make/miss, shape by 2/3
ggplot(main = "Steph Curry's Shot Chart 18-19", shotData18_19, aes(x=LOC_X, y=LOC_Y)) + 
  annotation_custom(court, -250, 250, -50, 420) +
  geom_point(aes(colour = EVENT_TYPE, shape = SHOT_TYPE)) +
  xlim(-250, 250) +
  ylim(-50, 420)


