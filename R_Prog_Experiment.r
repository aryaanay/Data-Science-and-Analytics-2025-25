# Install packages
install.packages(c("tidyverse", "ggridges", "patchwork", 
                   "viridis", "gapminder", "scales", "ggplot2", 
                   "tidytext", "text", "tm", "wordcloud"))
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
library(wordcloud)

# Import data and handle missing values - NA
tourism_data_original <- read.csv("tourism_customer_reviews.csv")
tourism_data <- na.omit(tourism_data_original)

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

stop_words <- stopwords::stopwords("en")
reviews_cleaned <- reviews_df$text
reviews_cleaned <- tolower(reviews_cleaned)
reviews_cleaned <- gsub("[[:punct:]]","", reviews_cleaned)
reviews_cleaned <- gsub("\\s+", " ", reviews_cleaned)
reviews_cleaned <- trimws(reviews_cleaned)

review_cleaned <- sapply(reviews_cleaned, function(x){
  words <- unlist(strsplit(x, " "))
  paste(setdiff(words, stop_words), collapse= " ")
})

# Cleaned reviews back in data frame
reviews_cleaned_df <- data.frame(
  Category = reviews_df$Category,
  text = review_cleaned,
  stringsAsFactors = FALSE
)

# WORD CLOUDS
text_bad <- paste(reviews_cleaned_df$text
                  [reviews_cleaned_df$Category == "Bad"], collapse=" ")
words_bad <- table(unlist(strsplit(text_bad, " ")))
wordcloud(words = names(words_bad), 
          freq = as.numeric(words_bad), scale = c(4,.5), 
          min.freq = 3, max.words = Inf, random.order=TRUE, random.color=FALSE, 
          rot.per=.1, colors="black")

text_mid <- paste(reviews_cleaned_df$text
                  [reviews_cleaned_df$Category == "Mid"], collapse=" ")
words_mid <- table(unlist(strsplit(text_mid, " ")))
wordcloud(words = names(words_mid), 
          freq = as.numeric(words_mid), scale = c(4,.5), 
          min.freq = 3, max.words = Inf, random.order=TRUE, random.color=FALSE, 
          rot.per=.1, colors="black")

text_good <- paste(reviews_cleaned_df$text
                   [reviews_cleaned_df$Category == "Good"], collapse=" ")
words_good <- table(unlist(strsplit(text_good, " ")))
wordcloud(words = names(words_good), 
          freq = as.numeric(words_good), scale = c(4,.5), 
          min.freq = 3, max.words = Inf, random.order=TRUE, random.color=FALSE, 
          rot.per=.1, colors="black")

