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
    library (foreach)
} else {
    stop ("Directory not found")
}

# cost function J for gradient descent using hinge loss
#    and regularization function
costSVM <- function (X, W, y, C, 
                     regularization = (function (weights) 0)) {
    return (regularization (W) + C * sum (max (0, 1 - y * (X %*% W))))
}

# gradient function of J