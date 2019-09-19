library(dplyr)

# Read iris data
data(iris)

# Petal length data
petal_length <- iris$Petal.Length

# Mean
mean_petal_length <- mean(petal_length)

# Histogram
histogram_petal_length <- hist(petal_length,
                               main="Histogram of Petal Length",
                               xlab="Petal Length",
                               col="pink",
                               ylim=c(0,40),
                               las=1,
                               breaks=11)

# Statistical calculations
iris$Petal.Length %>%
  summary()