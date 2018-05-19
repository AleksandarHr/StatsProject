#> sessionInfo()
#R version 3.3.2 (2016-10-31)
#Platform: x86_64-apple-darwin13.4.0 (64-bit)
#Running under: macOS  10.13.4
#
#locale:
#    [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

### Apply gradient descent and iteratively reweighted least squares 
###    on real data to see discrepancies.

if (dir.exists (PATH <- "~/Documents/Grinnell College/2017-2018/Spring/MAT336/StatsProject/")) {
    setwd (PATH)
    source ("helpers.R")
    library (readr)
    library (dplyr)
} else {
    stop ("Directory not found")
}

#read file
titanic <- read_csv ("Titanic/train.csv")

#cleaning data
titanicClean <- titanic %>% 
    select (Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>% 
    mutate (Sex = (Sex == "male") * 1) %>% 
    filter (!is.na (Age))
X <- as.matrix (titanicClean[, -which (colnames (titanicClean) == "Survived")])
Y <- as.matrix (titanicClean[, which (colnames (titanicClean) == "Survived")])
#Data cleaning:
#   - removed categorical variables with too many levels
#   - changed Sex into a numerical binary
#   - removed all missing values (Age): 19.9% of data missing

#GLM model
glmTitanic <- glm (Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare - 1,
                   data = titanicClean, family = binomial ())
glmAcc <- sum (round (glmTitanic$fitted.values) == y) / nrow (y)

#Batch Gradient Descent Logistic Regression
set.seed (90) #reproducibility
startingTheta <- runif (n = ncol (X), min = -1, max = 1)
epsilon = 1e-5
alpha = 1e-2
epochSize = 25
epochs = 500
bgdTitanic <- gradientDescent (X, y, startingTheta, epsilon, alpha, epochSize, epochs)
bgdAcc <- accuracy (X, y, bgdTitanic)

#Euclidean distance between two sets of parameters
sqrt (sum ((glmAcc - bgdAcc)^2))
