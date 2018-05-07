#> sessionInfo()
#R version 3.3.2 (2016-10-31)
#Platform: x86_64-apple-darwin13.4.0 (64-bit)
#Running under: macOS  10.13.4
#
#locale:
#    [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8#

# set proper working directory
if (dir.exists (PATH <- "~/Documents/Grinnell College/2017-2018/Spring/MAT336/StatsProject/")) {
    setwd (PATH)
    library (foreach)
} else {
    stop ("Directory not found")
}

# sigmoid function
sigmoid <- function (X) {
    return (1/(1+exp(-1*(X))))
}

# cost function of logistic regression
cost <- function (X, y, theta) {
    hx <- sigmoid (X %*% theta)
    return (-1 * (sum (y * log (hx) + (1 - y) * log (1 - hx))))
}

# gradient of cost function
grad <- function (X, y, theta) {
    hx <- sigmoid (X %*% theta)
    return(t(X) %*% (hx - y))
}

gradientDescent <- function (X, y, startingTheta, alpha, epsilon, epochSize = 100, epochs = 10,
                             filename = NA) {
    
    if (length (startingTheta) != 2) 
        stop ("Please use 2 values for theta")
    
    delta <- rep (1 / epsilon, length (startingTheta))
    theta <- startingTheta
    epoch <- 0
    iterations <- 0
    initialCost <- cost (X, y, theta)
    
    cat (sprintf ("alpha = %f\t epsilon = %f\n", alpha, epsilon))
    cat (sprintf ("Epoch\t Iter\t Loss\n"))
    if (!is.na (filename)) {
        record <- data.frame (matrix (nrow = 0, ncol = 4))
        colnames (record) <- c("Epoch", "theta1", "theta2", "Loss")
    }
    
    while ((delta > epsilon) && (epoch < epochs)) {
        
        if ((iterations %% epochSize) == 0) {
            curCost <- cost (X, y, theta)
            cat (sprintf ("%d\t %d\t %f\n", epoch, iterations, curCost))
            epoch <- epoch + 1
            
            if (!is.na (filename)) {
                record[epoch] <- c(epoch, theta[1], theta[2], curCost)
            }
        }
        iterations <- iterations + 1
        
        # update
        step <- alpha * grad (X, y, theta)
        theta <- theta - step
        delta <- step
    }
    
    finalCost <- cost (X, y, theta)
    
    if (!is.na (filename)) {
        record[epoch + 1] <- c(epoch + 1, theta[1], theta[2], finalCost)
        write.table (record, filename, sep = ',', row.names = F)
    }
    
    cat (sprintf ("Total Iterations = %d\n", iterations))
    cat (sprintf ("Initial Loss = %f\n", initialCost))
    cat (sprintf ("Final Loss = %f\n", finalCost))
    
    return (theta)
}
