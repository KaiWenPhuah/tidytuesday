library(tidyverse)
library(wordcloud)
library(tm)

tuesdata <- tidytuesdayR::tt_load(2024, week = 8)

isc_grants <- tuesdata$isc_grants

str(isc_grants)
View(isc_grants)

# Converting the 'summary' column from the isc_grants table into a VectorSource and then storing it in a Corpus for text mining.

text_corpus <- Corpus(VectorSource(isc_grants$summary))


# Preprocessing the text corpus by converting to lowercase, removing punctuation,numbers, extra whitespace, and custom stopwords.

custom_stopwords <- c("will", "can", "many", "use", "also", "make", "need","often")

text_corpus <- text_corpus %>% 
  tm_map(content_transformer(tolower)) %>% 
  tm_map(removePunctuation) %>% 
  tm_map(removeNumbers) %>% 
  tm_map(stripWhitespace) %>% 
  tm_map(removeWords, stopwords("en")) %>% 
  tm_map(removeWords, custom_stopwords)


# A wordcloud is then created

par(mar = c(2,3,4,3))

wordcloud(text_corpus,
          scale = c(3,0.5),
          random.order = FALSE,
          max.words = 40,
          color =brewer.pal(8,"Dark2")
)

title("Keywords in Titles and Summaries of R Consortium ISC Grants", cex.main = 1.5)
