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

# Generate random data distributed sin
rcircle <- function (n) {
    rho <- sqrt (runif (n))
    theta <- runif (n, 0, 2 * pi)
    circle <- data.frame(x = rho * cos (theta),
                         y = rho * sin (theta))
    return (circle)
}

# get contour data set
contourData <- function (X, y, theta1Seq, theta2Seq) {
    
    df <- foreach (theta1 = theta1Seq, .combine = 'rbind') %:%
        foreach (theta2 = theta2Seq, .combine = 'rbind') %do% {
            c(theta1, theta2, cost (X, y, cbind (c(theta1, theta2))))
        }
        
    colnames (df) <- c("theta1", "theta2", "Loss")
    df <- data.frame (df)
    
    return (df)
}

contourPlotly <- function (X, y, theta1Seq, theta2Seq) {
    df <- foreach(theta1 = theta1Seq, .combine='rbind') %:%
        foreach(theta2 = theta2Seq, .combine='c') %do% {
            cost(X, y, cbind(c(theta1, theta2)))
        }
        
}

# sigmoid function
sigmoid <- function (X) {
    return (1/(1+exp(-1*(X))))
}

# cost function of logistic regression
cost <- function (X, y, theta) {
    #n <- nrow(X)
    hx <- sigmoid (X %*% theta)
    return (-1 * (sum (y * log (hx) + (1 - y) * log (1 - hx))))
}

# gradient of cost function
grad <- function (X, y, theta) {
    hx <- sigmoid (X %*% theta)
    return(t(X) %*% (hx - y))
}

# run gradient descent
gradientDescent <- function (X, y, startingTheta, alpha, epsilon, 
                             epochSize = 100, epochs = 10, filename = NA) {
    
    if ((length (startingTheta) != 2) && (!is.na (filename))) 
        stop ("Please use 2 values for theta")
    
    delta <- rep (1 / epsilon, length (startingTheta))
    theta <- startingTheta
    epoch <- 0
    iterations <- 0
    initialCost <- cost (X, y, theta)
    
    cat (sprintf ("alpha = %f\t epsilon = %f\n", alpha, epsilon))
    cat (sprintf ("Epoch\t Iter\t theta1\t theta2\t Loss\n"))
    if (!is.na (filename)) {
        record <- data.frame (matrix (nrow = 0, ncol = 4))
        colnames (record) <- c("Epoch", "theta1", "theta2", "Loss")
    }
    
    while ((delta > epsilon) && (epoch < epochs)) {
        
        if ((iterations %% epochSize) == 0) {
            curCost <- cost (X, y, theta)
            #cat (sprintf ("%d\t %d\t %f\t %f\t %f\n", epoch, iterations, theta[1], theta[2], curCost))
            epoch <- epoch + 1
            
            if (!is.na (filename)) {
                record[epoch,] <- c(epoch, theta[1], theta[2], curCost)
            }
        }
        iterations <- iterations + 1
        
        # update
        step <- grad (X, y, theta)
        theta <- theta - alpha * step
        delta <- abs (step)
    }
    
    finalCost <- cost (X, y, theta)
    
    if (!is.na (filename)) {
        record[epoch + 1,] <- c(epoch + 1, theta[1], theta[2], finalCost)
        write.table (record, filename, sep = ',', row.names = F)
    }
    
    cat (sprintf ("Total Iterations = %d\n", iterations))
    cat (sprintf ("Initial Loss = %f\n", initialCost))
    cat (sprintf ("Final Loss = %f\n", finalCost))
    #cat (sprintf ("theta1 = %f\n", theta[1]))
    #cat (sprintf ("theta2 = %f\n", theta[2]))
    return (theta)
}

# predict accuracy of logistic regression
accuracy <- function (X, y, BGDmodel, cutoff = 0.5) {
    predictions <- (sigmoid (X %*% BGDmodel) >= cutoff) * 1
    comparison <- predictions == y
    return (sum (comparison) / nrow (X))
}


