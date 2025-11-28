install.packages(c("tidyverse", "ggridges", "patchwork", 
                   "viridis", "gapminder", "scales", "ggplot2", 
                   "tidytext", "text", "tm", "SnowballC", "wordcloud", 
                   "RColorBrewer"))
library(tidyverse)
library(ggridges)
library(patchwork)
library(viridis)
library(gapminder)
library(scales)
library(ggplot2)
library(tidytext)
library(text)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
# Import data and handle missing values - NA
tourism_data_original <- read.csv("tourism_customer_reviews.csv")
tourism_data <- na.omit(tourism_data_original)

#----------------------------
avg_rating <- mean(tourism_data$rating)
print(tourism_data |>
        group_by(location) |>
        summarize(avg_rating = mean(rating)) |>
        ggplot(aes(x = location, y = avg_rating)) +
        stat_summary(fun = mean, geom = "bar") +
        geom_col(fill = "#c2e5eb") +
        labs(title = "Location vs Rating", x = "Location", y = "Rating"))

#----------------------------
tourism_data <- tourism_data |>
  mutate(
    Category = case_when(
      rating <= 2 ~ "Bad",
      rating == 3 ~ "Mid",
      rating >= 4 ~ "Good"
    )
  )

bad_reviews_original <- tourism_data |> filter(Category == "Bad")
mid_reviews_original <- tourism_data |> filter(Category == "Mid")
good_reviews_original <- tourism_data |> filter(Category == "Good")

bad_reviews <- tolower(gsub("[[:punct:]]", 
                            "", bad_reviews_original$review_text))
mid_reviews <- tolower(gsub("[[:punct:]]", 
                            "", mid_reviews_original$review_text))
good_reviews <- tolower(gsub("[[:punct:]]", 
                             "", good_reviews_original$review_text))

#---------------------------------------