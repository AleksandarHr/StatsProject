#> sessionInfo()
#R version 3.3.2 (2016-10-31)
#Platform: x86_64-apple-darwin13.4.0 (64-bit)
#Running under: macOS  10.13.4
#
#locale:
#    [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

### Run simulations
###   - run 10 simulated data sets
###   - run gradient descent from 100 random initial thetas
###   - get contour plots for each

if (dir.exists (PATH <- "~/Documents/Grinnell College/2017-2018/Spring/MAT336/StatsProject/")) {
    setwd (PATH)
    source ("helpers.R")
    library (readr)
    library (foreach)
} else {
    stop ("Directory not found")
}

numData <- 10 #number of simulations
nruns <- 30 #number of gradient descent runs

glmresults <- data.frame (matrix (nrow = 0, ncol = 2))
gdresults <- data.frame (matrix (nrow = 0, ncol = 2))
gdvar <- data.frame (matrix (nrow = 0, ncol = 2))
gddist <- data.frame (matrix (nrow = 0, ncol = 2))
colnames (glmresults) <- c("theta1", "theta2")
colnames (gdresults) <- c("theta1", "theta2")
colnames (gdvar) <- c("theta1", "theta2")
colnames (gddist) <- c("theta1", "theta2")
for (i in 1:numData) {
    simData <- read_csv (paste0 ("data/simData_", i, ".csv"))
    mod <- glm (y ~ x1 + x2 - 1, data = simData, family = binomial ())
    glmresults[i,] <- as.vector (summary (mod)$coefficients)
    for (j in 1:nruns) {
        gdestimates <- read_csv (paste0 ("gradientdescent/data_", i, "/path_", j, ".csv"))
        gdresults[i,] <- c(mean (gdestimates$theta1), mean (gdestimates$theta2))
        gdvar[i,] <- c(var (gdestimates$theta1), var (gdestimates$theta2))
        gddist[i,] <- c(mean (abs (gdestimates$theta1 - glmresults$theta1[i])), 
                        mean (abs (gdestimates$theta2 - glmresults$theta2[i])))
    }
}

