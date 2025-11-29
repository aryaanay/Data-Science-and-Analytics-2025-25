# Install packages
install.packages(c("tidyverse", "ggridges", "patchwork", 
                   "viridis", "gapminder", "scales", "ggplot2", 
                   "tidytext", "text", "tm"))
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

# Import data and handle missing values - NA
tourism_data_original <- read.csv("tourism_customer_reviews.csv")
tourism_data <- na.omit(tourism_data_original)

# Visualize Data - Average rating vs location
avg_rating <- mean(tourism_data$rating)
print(tourism_data |>
        group_by(location) |>
        summarize(avg_rating = mean(rating)) |>
        ggplot(aes(x = location, y = avg_rating)) +
        stat_summary(fun = mean, geom = "bar") +
        geom_col(fill = "#c2e5eb") +
        labs(title = "Location vs Rating", x = "Location", y = "Rating"))

# Split data by bad, mid, and good reviews
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

# Cleaning: Remove punctuation and change to all lowercase
bad_reviews <- tolower(gsub("[[:punct:]]", 
                            "", bad_reviews_original$review_text))
mid_reviews <- tolower(gsub("[[:punct:]]", 
                            "", mid_reviews_original$review_text))
good_reviews <- tolower(gsub("[[:punct:]]", 
                             "", good_reviews_original$review_text))

# Create and combine individual category data frames 
bad_reviews_df <- data.frame(Category = "Bad", text = bad_reviews)
mid_reviews_df <- data.frame(Category = "Mid", text = mid_reviews)
good_reviews_df <- data.frame(Category = "Good", text = good_reviews)
reviews_df <- bind_rows(bad_reviews_df, mid_reviews_df, good_reviews_df)

# Remove stop words aka filler/unnecessary words and additional spaces
reviews_cleaned <- Corpus(VectorSource(reviews_df$text))
reviews_cleaned <- tm_map(reviews_cleaned, removeWords, stopwords("english"))
reviews_cleaned <- sapply(reviews_cleaned, as.character)
reviews_cleaned <- gsub("\\s+", " ", reviews_cleaned)
reviews_cleaned <- trimws(reviews_cleaned)
print(reviews_cleaned)