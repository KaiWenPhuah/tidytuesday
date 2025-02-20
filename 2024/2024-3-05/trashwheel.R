library(tidyverse)
library(ggplot2)
library(showtext)
library(sysfonts)
library(scales)
library(dplyr)
library(gridExtra)
library(ggbreak)
library(tidytext)


font_add_google("Outfit", "outfit")
showtext_auto()


tuesdata <- tidytuesdayR::tt_load(2024, week = 10)

trashwheel <- tuesdata$trashwheel

View(trashwheel)
str(trashwheel)

# Pivoting the trashwheel dataset to convert trash categories into the Rubbish Type column 

trashwheel_long <- trashwheel %>% 
  pivot_longer(cols = c(PlasticBottles, Polystyrene, CigaretteButts, GlassBottles, PlasticBags, Wrappers, SportsBalls),
               names_to = "Rubbish_Type",
               values_to = "Total")

# Summarize the total amount of trash collected for each Rubbish_Type

total_trash <- trashwheel_long %>% 
  group_by(Rubbish_Type) %>% 
  summarize(Total = sum(Total, na.rm = TRUE)) %>% 
  arrange(-Total)

View(total_trash)

# A bar chart is created to visualize the total trash collected

ggplot()+
  geom_bar(data = total_trash, aes(x = Total, y = reorder(Rubbish_Type,Total), fill = Rubbish_Type), stat="identity")+
  scale_x_continuous(label = unit_format( unit = "M", scale = 1e-6))+
  scale_x_break(c(3000000,10000000))+
  labs(
    title = "Cigarette Butts was the most collected type of Rubbish Over the Years ",
    subtitle = "Trends of Various Type of Trash Being Collected from 2014 to 2023",
    x = "Total",
    y = " Rubbish_Type" 
  )  +
  theme_minimal(base_family = "outfit")+
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 14),
    axis.title = element_text(size = 14, face = "bold")
  )


unique(trashwheel_long$Name)

# Loop through indices 1 to 4 to filter data for each trashwheel

for (i in 1:4){
  trashwheel_name[[i]] <- trashwheel_long %>% 
    filter(Name == ifelse(i == 1, "Mister Trash Wheel",
                   ifelse(i == 2, "Professor Trash Wheel",
                   ifelse(i == 3, "Captain Trash Wheel",
                   ifelse(i == 4, "Gwynnda Trash Wheel", NA)))))
                          
  View(trashwheel_name[[i]])
                  
}
# Creating an empty list to stroe the data

total_trash1 <- list()

# Loop through dataset and summarize the total trash collected for each Rubbish Type

for(i in seq_along(trashwheel_name)){
  total_trash1[[i]] <- trashwheel_name[[i]] %>% 
    group_by(Rubbish_Type) %>% 
    select(Name, Rubbish_Type, Total) %>% 
    summarize(
      Name = first(Name),
      Total = sum(Total, na.rm = TRUE))
  
  View(total_trash1[[i]])
  
}

# Bind the multiple dataset stored in total_trash1 into one dataset

combined_trash <- do.call(rbind, total_trash1)

# A bar chart is created to visualize which rubbsih type is the most collected for each trashwheel

ggplot()+
  geom_bar(data = combined_trash, aes(x = Total, y = reorder_within(Rubbish_Type, Total,Name), fill = Rubbish_Type), stat= "identity")+
  scale_x_continuous(labels = unit_format(unit = "M", scale = 1e-6))+
  scale_y_reordered()+ 
  facet_wrap(~Name, scales = "free_y",  nrow= 4, ncol = 1, strip.position = "bottom")+
  labs(
    title = "Mister Trash Wheel has the highest set of trash collected",
    subtitle = "Trends of Trash Collection  across Different Locations",
    x= "Total",
    y = "Rubbish_Type"
  )+
  theme_minimal(base_family = "outfit")+
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 14),
    axis.title = element_text(size = 14, face = "bold"),
    strip.text = element_text(size = 12),
    legend.position = "none"
  )


monthly_trash <- trashwheel_long %>% 
  mutate(Month = tolower(Month)) %>% 
  group_by(Month) %>% 
  summarize(Average_Trash = round(mean(Total, na.rm = TRUE),2)) %>% 
  arrange(-Average_Trash)


# A bar chart is created to visualize the trend of the total trash collected across the 12 months

ggplot()+
  geom_bar(data = monthly_trash, aes(x = Month, y = Average_Trash, fill = Month), stat = "identity")+
  scale_x_discrete(limits = c("january", "february", "march", "april", "may", "june", "july", "august", "september", "october", "november", "december"))+
  labs(
    title = "June has the Highest Average of Total Trash Collected",
    subtitle = "Trends of Total Trash Collected for each Month from 2014 to 2023",
    x = "Month",
    y = "Average Collected Trash"
  )+
  theme_minimal(base_family = "outfit")+
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 14),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    legend.position = "none"
  )
  
unique(trashwheel_long$Rubbish_Type)

# Filtering the dataset to contain only polystyrene type of rubbish

polystyrene <- trashwheel_long %>% 
  filter(Rubbish_Type == "Polystyrene") %>% 
  group_by(Year) %>% 
  summarize(Total_Trash = sum(Total, na.rm = TRUE))


# A line chart is created to visualize the trend of polystyrene collected across the years since the ban is implemented 

ggplot()+
  geom_line(data = polystyrene, aes(x = Year, y = Total_Trash), color = "red", size=1.3)+
  geom_vline(xintercept = 2018.33,  color = "black", size = 0.8, alpha = 0.5)+
  geom_vline(xintercept = 2019.75 , color = "black", size = 0.8, alpha = 0.5)+
  geom_curve(aes(x = 2017, y = 75000 , xend = 2018.15 , yend = 90000),
             curvature = -0.5,
             arrow = arrow(length = unit (0.03, "npc")),
             color = "black", size = 1)+
  geom_curve(aes(x = 2020.90, y = 250000, xend = 2020, yend = 225000),
             curvature = -0.5,
             arrow = arrow(length = unit(0.03, "npc")),
             color = "black", size =1)+
  geom_text(aes(x = 2016, y = 65000, label = "Polystyrene Ban passed Legislation"),
                color = "black", size = 5, fontface = "bold", family = "outfit")+
  geom_text(aes(x = 2021.5, y = 260000, label = "Foam Ban went into effect"),
            color = "black", family = "outfit", size = 5, fontface = "bold")+
  geom_point(data  = polystyrene, aes(x = Year, y = Total_Trash) )+
  scale_y_continuous(breaks = seq(min(50000), max(250000), by = 50000))+
  labs(
    title = "Total Polystyrene Collected Decreased Dramatically Since 2018",
    subtitle = "Trends of Total Polystyrene Collected from 2014 to 2023",
    x = "Year",
    y = "Total Polysterene Collected"
  )+
  theme_minimal(base_family = "outfit")+
  theme(
    plot.title = element_text(size = 16, face="bold"),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold")
  )

# Filtering the dataset to contain only plastic bags type of rubbish

plastic_bags <- trashwheel_long %>% 
  filter(Rubbish_Type == "PlasticBags") %>% 
  group_by(Year) %>% 
  summarize(Total = sum(Total, na.rm = TRUE))

head(plastic_bags, 10)


# A line chart is created to visualize the trend of plastics bags collected across the years since the ban is implemented 

ggplot()+
  geom_line(data = plastic_bags, aes(x = Year, y = Total), color = "red", size = 1.3)+
  geom_point(data = plastic_bags, aes(x = Year, y = Total))+
  geom_vline(xintercept = 2021.75, color = "black", alpha = 0.5, size = 0.8)+
  scale_y_continuous(breaks = seq(min(50000), max(250000), by = 50000))+
  geom_curve(aes(x = 2019.8, y = 200000, xend = 2021.5, yend = 225000),
             curvature = -0.5,
             arrow = arrow(length = unit(0.03, "npc")),
             color = "black", size = 1)+
  geom_text(aes(x = 2019.7, y = 190000, label = "Single-Used Plastic Bags Banned"),
            color = "black", family = "outfit", size = 4.5, fontface = "bold")+
  labs(
    title = "Total Plastic Bags Collected Decreased Dramatically From 2018",
    subtitle= "Trends of Total Plastic Bags Collected from 2014 to 2023",
    x = "Year",
    y = "Total Plastic Bags Collected"
  )+
  theme_minimal(base_family = "outfit")+
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 14),
    axis.title = element_text(size = 14, face = "bold")
  )

