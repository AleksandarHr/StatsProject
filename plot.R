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
    library (ggplot2)
    library (readr)
} else {
    stop ("Directory not found")
}

j <- 3
contourvals <- read.csv(paste0 ("contour/contour_", j, ".csv"))
g <- ggplot () + 
    geom_raster(data = contourvals, aes(x = theta1, y = theta2, fill = Loss)) + 
    scale_fill_gradientn(colours = c("#1D425C","#1B4F72","#1A5276","#1F618D","#2471A3","#2980B9",
                                     "#5499C7","#7FB3D5","#A9CCE3","#D4E6F1","#EAF2F8"),
                         values = c(0:10)^3/1000)
numFiles <- length (list.files (paste0 ("gradientdescent/data_", j, "/")))
for (i in 1:numFiles) {
    gradient <- read_csv (paste0 ("gradientdescent/data_", j, "/path_", i, ".csv"))
    g <- g + geom_line (data = gradient, aes (x = theta1, y = theta2))
}
g

