#> sessionInfo()
#R version 3.3.2 (2016-10-31)
#Platform: x86_64-apple-darwin13.4.0 (64-bit)
#Running under: macOS  10.13.4
#
#locale:
#    [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

### Creating randomly generated data with 2 features of a binary
###    classification. Data is distributed with equal density within
###    (x_1^2 + x_2^2) = 1 for both classes.
###    
###    rcircle (n)

if (dir.exists (PATH <- "~/Documents/Grinnell College/2017-2018/Spring/MAT336/StatsProject/")) {
    setwd (PATH)
    source ("helpers.R")
    library (ggplot2)
} else {
    stop ("Directory not found")
}

# Create data frame with n values
#    two features distributed rsin
#    randomly generated binary classification
n <- 10000
circle <- rcircle (n)
simData <- data.frame (x1 = circle$x,
                       x2 = circle$y,
                       y  = factor (rbinom (10000, 1, 0.5)))

# Plot to make sure graph is circular
ggplot (data = simData, aes (x = x1, y = x2, color = y)) + 
    geom_point () + 
    labs(x = "Variable 1",
         y = "Variable 2",
         title = "Simulated Data",
         color = "Class") + 
    theme_bw()

# Save data
write.table (simData, "simulatedData.csv", sep = ',', row.names = F)

