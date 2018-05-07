#> sessionInfo()
#R version 3.3.2 (2016-10-31)
#Platform: x86_64-apple-darwin13.4.0 (64-bit)
#Running under: macOS  10.13.4
#
#locale:
#    [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

if (dir.exists (PATH <- "~/Documents/Grinnell College/2017-2018/Spring/MAT336/StatsProject/")) {
    setwd (PATH)
    source ("helpers.R")
    library (foreach)
    library (readr)
    library (ggplot2)
} else {
    stop ("Directory not found")
}

simData <- read.csv("simulatedData.csv")
X <- as.matrix(simData[,c(1,2)])
y <- as.matrix(cbind(simData[,3]))

startx <- 0.03
finishx <- 0.04
starty <- 0.04
finishy <- 0.05
inc <- 0.0001

# get data for contours
trueCost <- foreach(x=seq(startx,finishx,inc),.combine='rbind') %:%
    foreach(y=seq(starty,finishy,inc),.combine='rbind') %do%
    c(x,y,cost(X,y,cbind(c(x,y))))
colnames(trueCost) <- c("theta1", "theta2", "cost")
trueCost <- data.frame(trueCost)

#plotly surface
trueCost1 <- foreach(x=seq(startx,finishx,inc),.combine='rbind') %:%
    foreach(y=seq(starty,finishy,inc),.combine='c') %do%
    cost(X,y,cbind(c(x,y)))

#best parameters
bestparam <- which.min(trueCost$cost)

ggplot (trueCost, aes(x = theta1, y = theta2, z = log10(cost))) + 
    geom_raster(aes(fill = cost)) + 
    #geom_contour(bins=10, color = "gray") + 
    scale_fill_continuous(low = "white", high = "black")

plotly(z = ~trueCost1) + add_surface()

trueCost[bestparam,]
summary(glm(y~x1+x2-1,data=simData,family=binomial()))
