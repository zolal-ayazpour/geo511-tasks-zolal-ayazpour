library(dplyr)
data(iris)
petal_length <- iris$Petal.Length
mean_petal_length <- mean(petal_length)
histogram_petal_length <- hist(petal_length,
                               main="Histogram of Petal Length",
                               xlab="Petal Length",
                               col="pink",
                               ylim=c(0,40),
                               las=1,
                               breaks=11)
iris$Petal.Length %>%
  summary()