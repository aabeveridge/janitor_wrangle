####################################################
# Basic Janitorial and Wrangling Tutorial
# Based on chapter 1 from *Text Mining with R*
####################################################

library(tidyverse)
library(tidytext)
library(ggplot2)

####################################################
# Tidytext method of data janitorial work with text
# from Chapter 1 of *Text Mining with R*
####################################################

# read in csv file as tibble/data frame
scrape.data <- read.csv(file='gboro_patch.csv', stringsAsFactors=FALSE)

clean.data <- as_tibble(scrape.data)

# transform table into one-word-per-line tidytext format
clean.data <- clean.data %>%
  unnest_tokens(word, text)

# most frequent words
clean.data %>%
  count(word, sort = TRUE)

# remove stop words
data(stop_words)
clean.data <- clean.data %>%
  anti_join(stop_words)

# check result of stop word removal
clean.data %>%
  count(word, sort = TRUE)

# remove numbers -- NOT from *Text Mining with R*
nums <- clean.data %>% filter(str_detect(word, "^[0-9]")) %>% select(word) %>% unique()

clean.data <- clean.data %>%
  anti_join(nums, by = "word")

# remove other words -- NOT from *Text Mining with R*
uni_sw <- data.frame(word = c("pm","greensboro", "city", "release", "press", "pieces", "happening", "moment"))

clean.data <- clean.data %>%
  anti_join(uni_sw, by = "word")

# visualize top words in corpus
clean.data %>%
      count(word, sort = TRUE) %>%
      filter(n > 25) %>%
      mutate(word = reorder(word, n)) %>%
      ggplot(aes(word, n)) +
      geom_col() +
      xlab(NULL) +
      coord_flip()
