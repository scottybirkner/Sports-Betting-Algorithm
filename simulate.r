BettingData <- read.csv(file.choose())
BettingData$HomeMargin = BettingData$Home.Score - BettingData$Away.Score

BettingData$TotalScoreActual = BettingData$Home.Score + BettingData$Away.Score

BettingData$isOver = ifelse(BettingData$TotalScoreActual >
                              BettingData$Total.Score.Close, 1, 0)
BettingData$homeWin = ifelse(BettingData$Home.Score > 
                               BettingData$Away.Score, 1, 0)
BettingData$HomeMoney = ifelse(BettingData$homeWin, 
                               100*BettingData$Home.Odds.Close - 100, -100)
BettingData$AwayMoney = ifelse(BettingData$homeWin, 
                               -100,100*BettingData$Away.Odds.Close - 100)

BettingData$homeCoverMargin = BettingData$HomeMargin + BettingData$Home.Line.Open
BettingData$homeCover = ifelse(BettingData$homeCoverMargin < 0, 0, 1)

BettingData$awayCover = ifelse(BettingData$homeCoverMargin > 0, 0, 1)
BettingData$atNE = ifelse(BettingData$Home.Team == "New England Patriots",
                          1, 0)
BettingData$atKC = ifelse(BettingData$Home.Team == "Kansas City Chiefs",
                          1, 0)
BettingData$atGB = ifelse(BettingData$Home.Team == "Green Bay Packers",
                          1, 0)
BettingData$atPIT = ifelse(BettingData$Home.Team == "Pittsburgh Steelers",
                           1, 0)
BettingData$awayCLE = ifelse(BettingData$Away.Team == "Cleveland Browns", 1, 0)
BettingData$awayDET = ifelse(BettingData$Away.Team == "Detroit Lions", 1, 0)
