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

DHmean <- c(1:44)
NDHmean <- c(1:44)
DH_SD <- c(1:44)
NDH_SD <- c(1:44)


for (year in 1973:2016){
  gl <- load.gamelog(year)
  
  glheaders <- read.csv("C:\\Users\\nnort\\Desktop\\MLB_RStudio\\game_log_header.csv")
  names(gl) <- names(glheaders)
 
  # headers names
  battingPositionHeaders <- c("VisitorRunsScored", "HomeRunsScore","VisitorBatting1Position","VisitorBatting2Position","VisitorBatting3Position","VisitorBatting4Position","VisitorBatting5Position","VisitorBatting6Position","VisitorBatting7Position","VisitorBatting8Position","VisitorBatting9Position") #,"HomeBatting1Position","HomeBatting2Position","HomeBatting3Position","HomeBatting4Position","HomeBatting5Position","HomeBatting6Position","HomeBatting7Position","HomeBatting8Position","HomeBatting9Position")
  
  # subset of gl2016 containing batting positions for visitor and home teams
  playerPosition <- gl[,battingPositionHeaders]
 
  playerPosition$teamSum <- rowSums(playerPosition[,3:11])
  playerPosition$TotalRunsScored <- rowSums(playerPosition[,1:2])

  dhUsed <- playerPosition[playerPosition$teamSum == 54,]
  dhNotUsed <- playerPosition[playerPosition$teamSum == 45,]

  if (year == 1979){
    dhNotUsed <- dhNotUsed[-c(519),]
    dhUsed <- dhUsed[-c(615),]
  }
  
  #hist(dhUsed[,"TotalRunsScored"])
  #hist(dhNotUsed[,"TotalRunsScored"])

  NDHmean[year - 1972] <- mean(dhNotUsed[,"TotalRunsScored"])
  DHmean[year - 1972] <- mean(dhUsed[,"TotalRunsScored"])
  DH_SD[year - 1972] <- sd(dhUsed[,"TotalRunsScored"])
  NDH_SD[year - 1972] <- sd(dhNotUsed[,"TotalRunsScored"])
  print(DHmean)
  print(NDHmean)
  print(DH_SD)
  print(NDH_SD)
  
}

# Graph Average Runs Scored per Game using y axis that ranges from 0 to 15 
# value in DH or NDH vector.  Turn off axes and 
# annotations (axis labels) so we can specify them ourself
plot(NDHmean, type="o", col="blue", ylim=c(7,11), axes=FALSE, ann=FALSE)
xvals <- c(1:44)
yvals <- data$coefficients[1] + data$coefficients[2]*xvals + data$coefficients[3]*(xvals^2)
lines(xvals,yvals, col="darkblue")
lines(xvals,yvals+dhCoeff, col="orange")

# Make x axis using years 1973:2016 labels
axis(1, at=1:44, lab=c(1973:2016))

# Make y axis with horizontal labels that display ticks at 
# every 4 marks. 4*0:g_range[2] is equivalent to c(0,4,8,12).
axis(2, at=7:12, lab=c(7:12))

# Create box around plot
box()

# Graph DHmean with red dashed line and square points
lines(DHmean, type="o", pch=22, col="red")

# Create a title with bold font
title(main="Average Total Runs Per Game Per Year", font.main=4)
title(xlab="Years", col.lab=rgb(0,0.5,0))
title(ylab="Average Total Runs per Game", col.lab=rgb(0,0.5,0))

# Create a legend at (1, g_range[2]) that is slightly smaller 
# (cex) and uses the same line colors and points used by 
# the actual plots 
legend(25, 8, c("DH not used","DH used"), cex=0.8, col=c("blue","red"), pch=21:22, lty=1)

# avgDHmean <- sum(DHmean)/44
# avgNDHmean <- sum(NDHmean)/44
# avgDH_SD <- sum(DH_SD)/44
# avgNDH_SD <- sum(NDH_SD)/44
# 
# print(avgDHmean)
# print(avgNDHmean)
# print(avgDH_SD)
# print(avgNDH_SD)
# 


