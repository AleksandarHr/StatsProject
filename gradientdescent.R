#> sessionInfo()
#R version 3.3.2 (2016-10-31)
#Platform: x86_64-apple-darwin13.4.0 (64-bit)
#Running under: macOS  10.13.4
#
#locale:
#    [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

### Functions for Gradient Descent
###
###    costSVM (X, W, y, C, regularization)
###    

# set proper working directory
if (dir.exists (PATH <- "~/Documents/Grinnell College/2017-2018/Spring/MAT336/StatsProject/")) {
    setwd (PATH)
    source ("helpers.R")
    library (readr)
} else {
    stop ("Directory not found")
}

simData <- read.csv("simulatedData.csv")
X <- as.matrix(simData[,c(1,2)])
y <- as.matrix(cbind(simData[,3]))

gradientDescent (X, y, c(5,5), 0.00001, 0.000001)
