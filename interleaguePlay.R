# Interleague Play
# Game Log
work_dir <- "C:/Users/nnort/Desktop/MLB_RStudio"
setwd(work_dir)

# download seasons to the working directory to avoid importing every time
load.gamelog <- function(season){
  download.file(
    url=paste("http://www.retrosheet.org/gamelogs/gl", season
              , ".zip", sep="")
    , destfile=paste("gl", season, ".zip", sep="")
  )
  unzip(paste("gl", season, ".zip", sep=""))
  gamelog <- read.table(paste("gl", season, ".txt", sep="")
                        , sep=",", stringsAsFactors=F)
  file.remove(paste("gl", season, ".zip", sep=""))
  file.remove(paste("gl", season, ".txt", sep=""))
  gamelog
}

nlWins <- matrix(nrow=20, ncol=3)  # (col 1 is home) and (col 2 is away) and (col 3 is total)
nlHome <- c(1:20)                  # number of nl home games
alWins <- matrix(nrow=20, ncol=3)  # (col 1 is home) and (col 2 is away) and (col 3 is total)
alHome <- c(1:20)                  # number of al home games
interleaguePValues <- c(1:20)

for (i in 1:20){
  nlWins[i,1] = 0
  nlWins[i,2] = 0
  alWins[i,3] = 0
  alWins[i,1] = 0
  alWins[i,2] = 0
  alWins[i,3] = 0
  nlHome[i] = 0
  alHome[i] = 0
}


years <- c(1:20)
DHmean <- c(1:20)
NDHmean <- c(1:20)
CI_DH <- matrix(nrow=20, ncol=2)
CI_NDH <- matrix(nrow=20, ncol=2)
pValues <- c(1:20)

for (year in 1997:2016){
  gl <- load.gamelog(year)
  
  glheaders <- read.csv("C:\\Users\\nnort\\Desktop\\MLB_RStudio\\game_log_header.csv")
  names(gl) <- names(glheaders)
  
  # headers names
  interleagueHeaders <- c("VisitingTeamLeague", "HomeTeamLeague", "VisitorRunsScored", "HomeRunsScore")
  # subset of gl2016 containing batting positions for visitor and home teams
  interleaguePlay <- gl[,interleagueHeaders]
  interleaguePlay$TotalRunsScored <- rowSums(interleaguePlay[,3:4])
    
  # (which league team won the game) and (number of AL homegames vs nl homegames)
  
  for (i in 1:nrow(interleaguePlay)){
    if (interleaguePlay[i,1] == "NL" && interleaguePlay[i,2] == "AL"){
      alHome[year - 1996] <- alHome[year - 1996] + 1
      if (interleaguePlay[i,4] > interleaguePlay[i,3]){
        alWins[year - 1996, 1] <- alWins[year - 1996, 1] + 1
      } else if (interleaguePlay[i,3] > interleaguePlay[i,4]){
        nlWins[year - 1996, 2] <- nlWins[year - 1996, 2] + 1
      }
    } else if (interleaguePlay[i,2] == "NL" && interleaguePlay[i,1] == "AL"){
      #NLRunsPerGame <- c(NLRunsPerGame,interleaguePlay[i,"TotalRunsScored"])
      nlHome[year - 1996] <- nlHome[year - 1996] + 1
      if (interleaguePlay[i,4] > interleaguePlay[i,3]){
        nlWins[year - 1996, 1] <- nlWins[year - 1996, 1] + 1
      } else if (interleaguePlay[i,3] > interleaguePlay[i,4]){
        alWins[year - 1996, 2] <- alWins[year - 1996, 2] + 1
      }
    }
  }

  ####################################################################
  # P VALUES  &  CONFIDENCE INTERVAL

  ALRunsPerGame <- numeric()
  NLRunsPerGame <- numeric()
  
  INTERLEAGUE <- subset(interleaguePlay, interleaguePlay[,1] != interleaguePlay[,2])
  ALRunsPerGame <- subset(INTERLEAGUE, INTERLEAGUE[,2] == "AL")[,5]
  NLRunsPerGame <- subset(INTERLEAGUE, INTERLEAGUE[,2] == "NL")[,5]

  DHmean[year - 1996] <- mean(ALRunsPerGame)
  NDHmean[year - 1996] <- mean(NLRunsPerGame)

  DH_SD <- sd(ALRunsPerGame)
  NDH_SD <- sd(NLRunsPerGame)
  
  n_DH <- length(ALRunsPerGame)
  n_NDH <- length(NLRunsPerGame)
  
  se_mean1_DH <- DH_SD / sqrt(n_DH)
  se_mean1_NDH <- NDH_SD / sqrt(n_NDH)

  #CONFIDENCE INTERVAL  
  CI_DH[year - 1996,1] <- DHmean[year - 1996] - 2 * se_mean1_DH
  CI_DH[year - 1996,2] <- DHmean[year - 1996] + 2 * se_mean1_DH
  CI_NDH[year - 1996,1] <- NDHmean[year - 1996] - 2 * se_mean1_NDH
  CI_NDH[year - 1996,2] <- NDHmean[year - 1996] + 2 * se_mean1_NDH

#P VALUES
  # meanScoreDifference <- DHmean[year - 1996] - NDHmean[year - 1996]
  # denominator <- sqrt(((DH_SD)^2 / n_DH) + ((DH_SD)^2 / n_NDH))
  #tDistribution <- meanScoreDifference / denominator
  pValues[year - 1996] <- t.test(ALRunsPerGame,NLRunsPerGame)[[3]]
  
  ####################################################################
  
  alWins[year - 1996,3] <- alWins[year - 1996,1] + alWins[year - 1996,2]
  nlWins[year - 1996,3] <- nlWins[year - 1996,1] + nlWins[year - 1996,2]
  # 
  # 
  # 
  alTotalWins <- c(alWins[year - 1996,3])
  nlTotalWins <- c(nlWins[year - 1996,3])
}

# 
plot(nlWins[,3], type="o", col="blue", ylim=c(90,170), axes=FALSE, ann=FALSE)

# Make x axis using years 1973:2016 labels
axis(1, at=1:20, lab=c(1997:2016))

# Make y axis with horizontal labels that display ticks at
# every 4 marks. 4*0:g_range[2] is equivalent to c(0,4,8,12).
axis(2, at=seq(90, 170, 10), lab=seq(90, 170, 10))

# Create box around plot
box()

# Graph DHmean with red dashed line and square points
lines(alWins[,3], type="o", pch=22, col="red")

# Create a title with bold font
title(main="Total Wins Per Year", font.main=4)
title(xlab="Years", col.lab=rgb(0,0.5,0))
title(ylab="Wins", col.lab=rgb(0,0.5,0))

legend(3, 165, c("NL","AL"), cex=0.8, col=c("blue","red"), pch=21:22, lty=1)

# #############################################################################################
# # -------------------------------------------------------#
# # Total AL Wins in AL parks vs Total AL Wins in NL parks #
# # -------------------------------------------------------#
# 
plot(alWins[,2], type="o", col="darkgreen", ylim=c(30,90), axes=FALSE, ann=FALSE)

# Make x axis using years 1973:2016 labels
axis(1, at=1:20, lab=c(1997:2016))

# Make y axis with horizontal labels that display ticks at
# every 4 marks. 4*0:g_range[2] is equivalent to c(0,4,8,12).
axis(2, at=seq(30, 90, 10), lab=seq(30, 90, 10))

# Create box around plot
box()

# Graph DHmean with red dashed line and square points
lines(alWins[,1], type="o", pch=22, col="orange")

# Create a title with bold font
title(main="Total Wins in AL parks vs NL parks", font.main=4)
title(xlab="Years", col.lab=rgb(0,0.5,0))
title(ylab="Number of Games", col.lab=rgb(0,0.5,0))

legend(5, 45, c("NL Parks","AL Parks"), cex=0.8, col=c("darkgreen","orange"), pch=21:22, lty=1)

# #############################################################################################
# # -------------------------------------------------------#
# # Total NL Wins in NL parks vs Total NL Wins in AL parks #
# # -------------------------------------------------------#
# 
plot(nlWins[,2], type="o", col="darkgreen", ylim=c(30,90), axes=FALSE, ann=FALSE)

# Make x axis using years 1973:2016 labels
axis(1, at=1:20, lab=c(1997:2016))

# Make y axis with horizontal labels that display ticks at
# every 4 marks. 4*0:g_range[2] is equivalent to c(0,4,8,12).
axis(2, at=seq(30, 90, 10), lab=seq(30, 90, 10))

# Create box around plot
box()

# Graph DHmean with red dashed line and square points
lines(nlWins[,1], type="o", pch=22, col="orange")

# Create a title with bold font
title(main="Total Wins in NL parks vs AL parks", font.main=4)
title(xlab="Years", col.lab=rgb(0,0.5,0))
title(ylab="Number of Games", col.lab=rgb(0,0.5,0))

legend(1, 45, c("AL Parks","NL Parks"), cex=0.8, col=c("darkgreen","orange"), pch=21:22, lty=1)


# #############################################################################################

# Graph Average Runs Scored per Game using y axis that ranges from 0 to 15 
# value in DH or NDH vector.  Turn off axes and 
# annotations (axis labels) so we can specify them ourself

plot(pValues, type="o", col="darkgreen", ylim=c(0,1), axes=FALSE, ann=FALSE)

# Make x axis using years 1973:2016 labels
axis(1, at=1:20, lab=c(1997:2016))

# Make y axis with horizontal labels that display ticks at 
# every 4 marks. 4*0:g_range[2] is equivalent to c(0,4,8,12).
axis(2, at=seq(0, 1, 0.1), lab=seq(0, 1, 0.1))

# Create box around plot
box()

segments(x0=1, x1=20, y0=0.05, y1=0.05, col="orange", lwd=0.5)

# Create a title with bold font
title(main="Average Total Runs Per Game Per Year", font.main=4)
title(xlab="Years", col.lab=rgb(0,0.5,0))
title(ylab="P Values", col.lab=rgb(0,0.5,0))

legend(3, 0.9, c("P Values","P Value of 0.05"), cex=0.8, col=c("darkgreen","orange"), pch=c(21, NA), lty=1)


# #############################################################################################
plot(NDHmean, type="o", col="blue", ylim=c(6,12), axes=FALSE, ann=FALSE)

# Make x axis using years 1973:2016 labels
axis(1, at=1:20, lab=c(1997:2016))

# Make y axis with horizontal labels that display ticks at 
# every 4 marks. 4*0:g_range[2] is equivalent to c(0,4,8,12).
axis(2, at=6:12, lab=c(6:12))

# Create box around plot
box()

# Graph DHmean with red dashed line and square points
lines(DHmean, type="o", pch=22, col="red")

#segments(x0=years-0.1, x1=years-0.1, y0=CI_NDH[,1], y1=CI_NDH[,2], col="blue", lwd=3)
#segments(x0=years+0.1, x1=years+0.1, y0=CI_DH[,1], y1=CI_DH[,2], col="red", lwd=3)

# Create a title with bold font
title(main="Average Total Runs Per Game Per Year", font.main=4)
title(xlab="Years", col.lab=rgb(0,0.5,0))
title(ylab="Average Total Runs per Game", col.lab=rgb(0,0.5,0))

legend(1, 7.5, c("DH not used","DH used"), cex=0.8, col=c("blue","red"), pch=21:22, lty=1)

