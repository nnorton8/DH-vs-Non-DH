# linear regression

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

df <- data.frame("Year" = numeric(), "DH_Used" = numeric(), "RunsScored" = numeric())
marginOfVictory <- numeric()


for (year in 1973:2016){
  gl <- load.gamelog(year)
  glheaders <- read.csv("C:\\Users\\nnort\\Desktop\\MLB_RStudio\\game_log_header.csv")
  names(gl) <- names(glheaders)
  battingPositionHeaders <- c("VisitorRunsScored", "HomeRunsScore","VisitorBatting1Position","VisitorBatting2Position","VisitorBatting3Position","VisitorBatting4Position","VisitorBatting5Position","VisitorBatting6Position","VisitorBatting7Position","VisitorBatting8Position","VisitorBatting9Position")
  playerPosition <- gl[,battingPositionHeaders]
  playerPosition$teamSum <- rowSums(playerPosition[,3:11])
  
  playerPosition$marginOfVictory <- abs(playerPosition[,2] - playerPosition[,1])
  marginOfVictory <- c(marginOfVictory,playerPosition[,13])
  playerPosition$TotalRunsScored <- rowSums(playerPosition[,1:2])
  playerPosition$Year <- rep(year-1972,nrow(playerPosition))

  if (year == 1979){
    playerPosition <- playerPosition[-c(1133),]
  }
  
  playerPosition$DH_Used <- as.numeric(playerPosition[,12] == 54)
  dh <- data.frame(playerPosition[,15], playerPosition[,16], playerPosition[,14])
  df <- rbind(df,dh)
}
colnames(df) <- c("Year", "DH_Used", "RunsScored")

#df <- df[-c(34191),]

########################################################################
ALGames <- subset(df, df[,2] != 0)
NLGames <- subset(df, df[,2] != 1)
ALGames1runScored <- subset(ALGames, ALGames[,3] == 1)
ALGames2runScored <- subset(ALGames, ALGames[,3] == 2)
ALGames3runScored <- subset(ALGames, ALGames[,3] == 3)

                  
histALGames <- hist(ALGames[,3], breaks = 0:44, col=rgb(1,0,0,0.5), xlim=c(0,44),
                    main = paste("Total Runs Scored in All AL Games Distribution"),
                    ylim = c(0,6000), xlab = "Total Runs Scored in a Game",
                    ylab = "Number of Games", col.lab="darkgreen")
box()

histNLGames <- hist(NLGames[,3], breaks = 44, col=rgb(0,0,1,0.5),
                    main = paste("Total Runs Scored in All NL Games Distribution"),
                    ylim = c(0,6000), xlab = "Total Runs Scored in a Game",
                    ylab = "Number of Games", col.lab="darkgreen")

histAllGames <- hist(df[,3], breaks = 44,main = paste("Total Runs Scored in All Games Distribution"),
                     ylim = c(0,12000), xlab = "Total Runs Scored in a Game",
                     ylab = "Number of Games", col.lab="darkgreen")

hist(ALGames[,3], xlim=c(0,44), ylim=c(0,6000), breaks=0:44, col=rgb(1,0,0,0.5), xlab="Total Runs Scored in a Game", 
     ylab="Number of Games", main="Total Runs Scored in All NL vs AL Games Distribution" )
hist(NLGames[,3], breaks=44,col=rgb(0,0,1,0.5), add=T)
legend(20, 5000, legend=c("AL","NL"), col=c(rgb(1,0,0,0.5), rgb(0,0,1,0.5)), pt.cex=2, pch=15 )


histInfo <- hist(marginOfVictory, breaks = 27,main = paste("Margin of Victory"),
                 ylim = c(0,30000), xlab = "Margin of Victory", ylab = "Number of Games",
                 col.lab="darkgreen")
histInfo
xfit<-seq(min(marginOfVictory),max(marginOfVictory),length=27) 
yfit<-dnorm(xfit,mean=mean(marginOfVictory),sd=sd(marginOfVictory)) 
yfit <- yfit*diff(histInfo$mids[1:2])*length(marginOfVictory) 
box()
ftable(marginOfVictory)
########################################################################

#lines(xfit, yfit, col="blue", lwd=2)
#
#xfit <- seq(min(df[,3]),max(df[,3]),length=44) 
#yfit <- yfit*diff(b$mids[1:2])*length(44)
#yfit <- dnorm(xfit,mean=mean(df[,3]),sd=sd(df[,3])) 
#lines(xfit, yfit, col="blue", lwd=2)


plot(df[,3], type="o", col="blue", ylim=c(0,50), axes=FALSE, ann=FALSE)

# Make x axis using years 1973:2016 labels
axis(1, at=1:44, lab=c(1973:2016))

# Make y axis with horizontal labels that display ticks at 
# every 4 marks. 4*0:g_range[2] is equivalent to c(0,4,8,12).
axis(2, at=seq(0, 50, 10), lab=seq(0, 50, 10))

# Create box around plot
box()

# Create a title with bold font
title(main="Linear Regression Model", font.main=4)
title(xlab="Years", col.lab=rgb(0,0.5,0))
title(ylab="Total Runs Per Game", col.lab=rgb(0,0.5,0))

b <- boxplot(df[,3] ~ df[,1], main="Distribution of the Total Runs Per Game",
        xlab="Years", ylab="Total Runs Per Game", names=c(1973:2016))


data <- lm(RunsScored ~ Year + I(Year^2) + DH_Used , df)
yIntercept <- summary(data)[[4]][1]
xCoeff <- summary(data)[[4]][2]
x2Coeff <- summary(data)[[4]][3]
dhCoeff <- summary(data)[[4]][4]

xfit <- 1973:2016
#fit <- fitted(data)
#plot(xfit, fit, type="l")
plotPoints <- c(1:44)

for (year in 1:44){
  plotPoints[year] = yIntercept + xCoeff*year + x2Coeff*(year^2)
}

plotPoints
plot(xfit, plotPoints, type="l", col="darkgreen", ylim=c(7,11), axes=FALSE, ann=FALSE)
lines(xfit,plotPoints+dhCoeff, col="orange")

###########################################################################
plot(NDHmean, type="o", col="blue", ylim=c(7,11), axes=FALSE, ann=FALSE)
xvals <- c(1:44)
yvals <- data$coefficients[1] + data$coefficients[2]*xvals + data$coefficients[3]*(xvals^2)
lines(xvals,yvals, col="green")
lines(xvals,yvals+a[4], col="orange")

# Make x axis using years 1973:2016 labels
axis(1, at=1973:2016, lab=c(1973:2016))

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

