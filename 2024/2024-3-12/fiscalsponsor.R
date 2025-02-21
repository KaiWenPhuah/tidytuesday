library(dplyr)
library(tidyverse)
library(tidyr)
library(showtext)
library(sysfonts)
library(treemapify)

font_add_google("Outfit", "outfit")
showtext_auto()

tuesdata <- tidytuesdayR::tt_load(2024, week = 11)

fiscal_sponsor_directory <- tuesdata$fiscal_sponsor_directory

projects <- fiscal_sponsor_directory %>% 
  select(project_types) %>% 
  separate_rows(project_types, sep = "\\|") %>% 
  separate_rows(project_types, sep = "\\:") %>% 
  drop_na() %>% 
  mutate(project_types = gsub("/", " & ", project_types),
         project_types = str_remove(project_types, "\\,\\.\\+"),
         project_types = str_trim(project_types),
         project_types = str_to_lower(project_types)) %>% 
  count(project_types) %>% 
  mutate(project_types = if_else(n > 10, project_types, "other")) %>% 
  group_by(project_types) %>% 
  summarize(Total = sum(n))

View(projects)

# Creating a tree map to visualize all the types of projects supported by fiscal sponsors


ggplot()+
  geom_treemap(data = projects, aes(area = Total, fill = project_types),
                fill = "lightblue", color = "white")+
  geom_treemap_text(data = projects, aes(area = Total, label = project_types),
                    place = "centre", grow = TRUE, color = "black", family = "outfit")+
  labs(
  title = "Fiscal Sponsors",
  subtitle = "Project types supported by 370 fiscal sponsors"
  )+
  theme_minimal(base_family = "outfit")+
  theme(
    plot.title = element_text(size = 20, face = "bold", color = "white"),
    plot.subtitle = element_text(size = 18, color = "white"),
    legend.position = "none",
    plot.background = element_rect(fill = "darkblue")
    
  )
