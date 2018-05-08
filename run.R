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

numData <- 1 #number of simulations
n <- 10000 #number of data points
nruns <- 30 #number of gradient descent runs
epsilon = 1e-6 #gradient descent parameter
alpha = 1e-4 #gradient descent parameter

startTime <- Sys.time()
# run 10 simulated data sets
for (i in 1:numData) {
    cat (sprintf ("Simulation number : %d\n", i))
    
    #generate data
    cat (sprintf ("Generating Data\n"))
    circle <- rcircle (n)
    simData <- data.frame (x1 = circle$x,
                           x2 = circle$y,
                           y  = factor (rbinom (10000, 1, 0.5)))
    X <- as.matrix(simData[,c(1,2)])
    y <- as.matrix(cbind(simData[,3]))
    write.table (simData, paste0 ("data/simData_", i, ".csv"), sep = ',', row.names = F)
    
    #get glm estimates of theta
    cat (sprintf ("Getting glm estimates\n"))
    mod <- glm(y ~ x1 + x2 - 1, data = simData, family = binomial())
    thetaEst <- summary (mod)$coefficients[,1]
    
    #gradient descent
    cat (sprintf ("Running gradient descent\n"))
    initialTheta1 <- runif (nruns, -1, 1)
    initialTheta2 <- runif (nruns, -1, 1)
    PATH <- paste0 ("gradientdescent/data_", i)
    if (!dir.exists (PATH)) {
        dir.create (PATH)
    }
    for (j in 1:nruns) {
        initialTheta <- c(initialTheta1[j], initialTheta2[j])
        gradientDescent(X, y, initialTheta, alpha, epsilon, epochSize = 10, epochs = 500,
                        filename = paste0 (PATH, "/path_", j, ".csv"))
    }
}
Sys.time() - startTime

# #generate contour data
# cat (sprintf ("Generating contour data\n"))
# starts <- thetaEst - abs (thetaEst * 0.5)
# ends <- thetaEst + abs (thetaEst * 0.5)
# inc <- (ends - starts) / 100
# contourvals <- contourData (X, y, starts, ends, inc)
# write.table (contourvals, paste0 ("contour/contour_", i, ".csv"), sep = ',', row.names = F)
# 
# ggplot (contourvals, aes(x = theta1, y = theta2, z = log10(Loss))) + 
#     geom_raster(aes(fill = Loss)) + 
#     #geom_contour(bins=10, color = "gray") + 
#     scale_fill_continuous(low = "white", high = "black")

