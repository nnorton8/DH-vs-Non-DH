# histogram of runs per game in 2016 season
# Game log
work_dir <- "C:/Users/nnort/Desktop/MLB_RStudio"
setwd(work_dir)


gl <- load.gamelog(2016)
glheaders <- read.csv("C:\\Users\\nnort\\Desktop\\MLB_RStudio\\game_log_header.csv")
names(gl) <- names(glheaders)
battingPositionHeaders <- c("VisitorRunsScored", "HomeRunsScore","VisitorBatting1Position","VisitorBatting2Position","VisitorBatting3Position","VisitorBatting4Position","VisitorBatting5Position","VisitorBatting6Position","VisitorBatting7Position","VisitorBatting8Position","VisitorBatting9Position")
playerPosition$teamSum <- rowSums(playerPosition[,3:11])
playerPosition$TotalRunsScored <- rowSums(playerPosition[,1:2])
dhUsed <- playerPosition[playerPosition$teamSum == 54,]
dhNotUsed <- playerPosition[playerPosition$teamSum == 45,]
hist(dhUsed[,"TotalRunsScored"])
hist(dhNotUsed[,"TotalRunsScored"])
