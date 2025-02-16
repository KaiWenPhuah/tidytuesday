library(tidyverse)
library(readr)
library(dplyr)
library(tidyr)
library(tidytext)  
library(showtext)
library(sysfonts)

tuesdata <- tidytuesdayR::tt_load('2024-02-13')

historical_spending <- tuesdata$historical_spending
gifts_age <- tuesdata$gifts_age
gifts_gender <- tuesdata$gifts_gender

View(historical_spending)
View(gifts_age)
View(gifts_gender)

str(historical_spending)
str(gifts_age)
str(gifts_gender)


# Font and Styling

font_add_google("Outfit", "outfit")
showtext_auto()


# Analyze & Visualization

# Coeff is calculated to balance out the value so that the graph can be fitted for both lines

coeff <- round(max(historical_spending$PerPerson) / max(historical_spending$PercentCelebrating))


# A line chart is plotted to discover the trends in Valentine's Day celebration and spending over time.

ggplot(data = historical_spending) +
  geom_line(aes(x = Year, y = PercentCelebrating, color = "Percent Celebrating"), linewidth = 1.2) +
  geom_line(aes(x = Year, y = PerPerson/coeff, color = "Per Person Spending"), linewidth = 1.2) +
  scale_x_continuous(name = "Years", breaks = seq(ceiling(min(historical_spending$Year)), floor(max(historical_spending$Year)), by = 2)) +
  scale_y_continuous(name = "Percentage [%]", sec.axis = sec_axis(~ . * coeff, name = "Spending [$]")) +
  labs(
    title = "Decline in Valentine's Day Celebrations Over the Years",
    subtitle = "Trends in Valentine's Day Celebration From 2010 to 2022",
    color = "Legend"  # Legend title
  ) +
  theme_minimal(base_family = "outfit") +
  theme(
    plot.title = element_text(face="bold", size = 17),
    plot.subtitle = element_text(size=12),
    axis.title = element_text(size=12, face="bold"),
    legend.position.inside = c(0.28,0.8)
  )


# Transforming gift data from wide to long format for visualization.

presents <- gifts_age %>% 
  pivot_longer(cols = c(Candy, Flowers, Jewelry, GreetingCards, EveningOut, Clothing, GiftCards), 
               names_to = "GiftsType",
               values_to = "Values") %>% 
  arrange(Values)


# Clustered bar charts are plotted to discover the trend of different age groups spending habits on Valentine's Day

ggplot()+
  geom_bar(data = presents, aes(x = reorder_within(Age, Values, GiftsType), y = Values, fill = Age), stat = "identity")+
  facet_wrap(~ GiftsType, nrow = 3, ncol = 3, strip.position = "bottom", scale = "free_x")+
  labs(
    title = "Younger Age Groups Spends more on Gifts for Valentine's Day",
    subtitle = "Analyzing Trends of Different Age Groups Allocate Spending on Valentine's Day Gifts.",
    x = "Age Groups",
    y = "Percentage [%]"
  )+
  theme_minimal(base_family = "outfit")+
  theme(
    plot.title = element_text(size = 17, face = "bold"),
    plot.subtitle = element_text(size =13),
    axis.title= element_text(size = 14, face = "bold"),
    axis.text.x = element_blank(),
    strip.text = element_text(size = 12)
  )

# Transforming gender data from wide to long format for visualization.

gender <- gifts_gender %>% 
  pivot_longer(cols = c(Candy, Flowers, Jewelry, GreetingCards, EveningOut, Clothing, GiftCards),
               names_to = "GiftsType",
               values_to = "Values")


# Clustered bar charts are plotted to discover what gifts would male and female would spend more on for their partner on Valentine's Day

ggplot()+
  geom_bar(data = gender ,aes(x = Gender, y = Values, fill = Gender), stat = "identity")+
  facet_wrap(~GiftsType, nrow = 3, ncol = 3, strip.position = "bottom")+
  labs(
    title = "Men Spend More on Jewelry and Flowers for Women on Valentine's Day ",
    subtitle = "Spending Trends on Valentine's Day Gifts Differ by Gender Across Various Categories",
    x = "Genders",
    y = "Percentage [%]"
  )+
  theme_minimal(base_family = "outfit")+
  theme(
    plot.title = element_text(size = 17, face = "bold"),
    plot.subtitle = element_text(size = 13),
    axis.title = element_text(size = 14, face = "bold"),
    strip.text = element_text(size = 12),
    axis.text.x = element_blank()
  )


















