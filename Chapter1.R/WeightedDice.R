library(ggplot2)
library(fansi)
library(dplyr)
library(tidyverse)
library(farver)

# Creating die

die <- 1:6

dice <- sample(die, size = 2, replace = TRUE)
sum(dice)

roll <- function() {
  die <- 1:6
  dice <- sample(die, size = 2, replace = TRUE)
  sum(dice)
}

roll()


rolls.unweighted <- replicate(10000, roll())
qplot(rolls.unweighted, binwidth = 1)

# Creating and rolling weighted die

roll.weighted <- function() {
  die <- 1:6
  dice <- sample(die, size = 2, replace = TRUE, 
                 prob = c(1/8, 1/8, 1/8, 1/8, 1/8, 3/8))
  sum(dice)
}


rolls.weighted <- replicate(10000, roll.weighted())
qplot(rolls.weighted, binwidth = 1)
