library(dplyr)
library(tidyverse)
library(janitor)
library(readr)
library(summarytools)
library(ggbeeswarm)


education <- read_csv("2208 Intern/english_education.csv")

# ANALYZE

head(education)
str(education)
colnames(education)
summary(education)
View(education)

dfSummary(education) %>%  view()

unique(education$size_flag)



# Categorize other built-up-areas in cities and inner and outer London

uk_education <- education %>% 
  mutate(size = case_when(
    size_flag == "Not BUA"~ "City",
    size_flag == "Other Small BUAs" ~ "City",
    str_detect(tolower(size_flag), "london") ~ "Inner and Outer London", # str_detect(column, "word") used to detect whether the word is in the column 
    TRUE ~size_flag
  )) 

View(uk_education)



# Get the median value for every group for the vertical line

median_education_att <- uk_education %>% 
  group_by(size) %>% 
  summarize(median = median(education_score, na.rm = TRUE))

View(average_education_att)



# Plotting the quasirandom plot for each of the groupings with the educational attaintment scores with the vertical median line shown

ggplot() +
  geom_quasirandom(data = uk_education, aes(x = education_score, y = size, color = income_flag), size = 1, dodge.width = 2, method = "pseudorandom") +
  geom_vline(data = median_education_att, aes(xintercept = median), size = 1, color = "red") +
  labs(
    title = "Smaller towns achieve better overall education attainment scores",
    subtitle = "Educational attainment score based on town size and income deprivation",
    x = "Educational Attainment Score",
    y = "Towns"
  ) +
  geom_text(data = median_education_att %>% filter(size == "Large Towns"),
            aes(median, Inf, label = "Average for size group"),
            hjust = 1.5, family = "Arial", fontface = "bold") +
  geom_curve(
    data = median_education_att %>% filter(size == "Large Towns"),
    aes(x = median - 2.2, xend = median - 0.2, y = Inf, yend = Inf),
    color = "black",
    linewidth = 1,
    curvature = -0.3,
    arrow = arrow(length = unit(0.1, "npc"))
  ) +
  coord_cartesian(clip = "off") +
  facet_wrap(~size, ncol = 1, scales = "free_y") +
  theme_minimal(base_family = "Outfit") +
  theme(
    plot.background = element_rect(fill = "grey99", color = NA),
    legend.position = c(0.88, 0.8),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_text(hjust = 0.50),
    plot.margin = margin(10, 10, 10, 10),
    plot.title = element_text(face = "bold", size = 14),
    plot.caption = element_text(margin = margin(10, 0, 0, 0)),
    panel.spacing.y = unit(1, "lines"),
    strip.text = element_text(size = 11, face = "bold", margin = margin(10, 0, 10, 0))
  )
