install.packages(c("tidyverse", "ggridges", "patchwork", 
"viridis", "gapminder", "scales", "ggplot2"))
library(tidyverse)
library(ggridges)
library(patchwork)
library(viridis)
library(gapminder)
library(scales)
library(ggplot2)

# Import data and handle missing values - NA
tourismDataOriginal <- read.csv("tourism_customer_reviews.csv")
tourismData <- na.omit(tourismDataOriginal)
print(head(tourismData))
print(summary(tourismData))

#----------------------------
avg_rating = mean(tourismData$rating)
print(tourismData %>%
    group_by(location) %>%
    summarize(avg_rating = mean(rating)) %>%
    ggplot(aes(x = location, y = avg_rating)) +
    stat_summary(fun = mean, geom = "bar") +
    geom_col(fill = "#c2e5eb") +
    labs(title = "Location vs Rating", x = "Location", y = "Rating"))

#----------------------------
