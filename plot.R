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

j <- 2
contourvals <- as.matrix(read.csv(paste0 ("contour/contour_", j, ".csv")))
p <- plot_ly(type = 'contour', x = seq (-1, 1, 0.01), y = seq (-1, 1, 0.01), z = contourvals,
             colors = colorRamp(c("dark blue", "white")))
numFiles <- length (list.files (paste0 ("gradientdescent/data_", j, "/")))
for (i in 1:numFiles) {
    gradient <- read_csv (paste0 ("gradientdescent/data_", j, "/path_", i, ".csv"))
    p <- add_trace(p, data = gradient, x = ~theta1, y = ~theta2, type = 'scatter', mode = 'lines', color = I("white"))
}
p %>%
    layout(showlegend = FALSE)

