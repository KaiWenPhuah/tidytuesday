library(tidyverse)
library(dplyr)
library(showtext)
library(maps)
library(sf)
library(rnaturalearth)
library(maps)


font_add_google("Outfit", "outfit")
showtext_auto()

tuesdata <- tidytuesdayR::tt_load(2024, week = 17)

outer_space_objects <- tuesdata$outer_space_objects

View(outer_space_objects)


# Summarize total space objects per entity 

most_obj <- outer_space_objects %>% 
  group_by(Entity) %>% 
  summarize(Total_obj = sum(num_objects)) %>% 
  arrange(-Total_obj)


# Summarize total objects per Year

most_in_a_year <- outer_space_objects %>% 
  group_by(Year) %>% 
  summarize(Total_obj = sum(num_objects)) %>% 
  arrange(-Total_obj) 

  
# A line chart is created to visualize the number of objects sent up to space per year

ggplot()+
  geom_line(data = most_in_a_year, aes(x = Year, y = Total_obj), color = "red", linewidth = 1)+
  labs(
    title = "Number of Objects Sent to Space per Year",
    subtitle = "Trends of Objects Sent to Space from 1962 to 2024",
    x = "Year",
    y = "Total Objects"
  )+
  theme_minimal(base_family = "outfit")+
  theme(
    plot.title = element_text(size = 16, face ="bold"),
    plot.subtitle = element_text(size = 14),
    axis.title = element_text(size = 12, face = "bold")
  )


# Get unique country names from map data, replacing "USA" and "UK" with full names

world_countries <- map_data("world") %>% 
  mutate(region = str_replace_all(region, c("USA"= "United States", "UK"= "United Kingdom"))) %>% 
  distinct(region) %>% 
  pull(region)


# Filtering the dataset to only contain countries that have sent 100 or more objects

morethan_100 <-  most_obj %>% 
  filter(Total_obj >=100) %>% 
  filter(Entity %in%  world_countries)

  
# Load world map as a spatial feature object  

world_map <- ne_countries(scale = "medium", returnclass = "sf") 



# Replace "United States" with "United States of America" in the Entity column  

morethan_100_ver1 <- morethan_100 %>% 
  mutate(admin = str_replace_all(Entity, c("United States" = "United States of America" ))) 



# Extract the 'admin' column as a vector of highlighted countries.  

highlighted_country <- morethan_100_ver1 %>% 
  pull(admin)



# Create a world map highlighting countries with 100+ space objects

ggplot()+
  geom_sf(data = world_map, fill = "gray", color = "white")+
  geom_sf(data = filter(world_map, admin %in% highlighted_country ), fill = "red", color = "black")+
  labs(
    title = "Countries with 100 or above Objects Sent to Space",
    subtitle = "Trends of Countries Sending Objects to Space from 1962 to 2024"
  )+
  theme_minimal(base_family = "outfit")+
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 14)
    
  )


# Filter outer_space_objects for entities in morethan_100 and arrange by Entity and Year  

trendstop10 <- outer_space_objects %>% 
  filter(Entity %in% (morethan_100 <- morethan_100 %>% pull(Entity))) %>%   
  arrange(Entity, Year)



# Create a faceted line chart showing the number of objects sent to space per year for each country  

ggplot(data = trendstop10)+
  geom_line(aes(x = Year, y = num_objects, color = Entity), linewidth = 0.7)+
  facet_wrap(~Entity, nrow = 3, ncol = 3, strip.position = "bottom", scales = "free_y")+
  labs(
    title = "Number of Objects Sent to Space per Year",
    subtitle = "Note: Y-axis is Unique to each Country",
    x = "Year",
    y = "Number of Objects In Space"
  )+
  theme_minimal(base_family = "outfit")+
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 14),
    axis.title = element_text(size = 14, face= "bold"),
    strip.text = element_text(size = 10)
  )
  




