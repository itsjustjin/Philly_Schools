#R Studio API Code
library(rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Libraries
library(tidyverse)
library(readxl)
library(janitor)
library(gghighlight)
library(ggthemes)

# Data Import and Cleaning

#Import Serious Incidents for SY18-19
url <- "https://cdn.philasd.org/offices/performance/Open_Data/School_Performance/Serious_Incidents/School%20Profiles%20Serious%20Incidents%202018-2019.xlsx"
destfile <- "../data/SY18-19.xlsx"
download.file(url, destfile, mode = "wb")

incidents_df <- read_excel(destfile, sheet = 1)

#Import School ID data for SY18-19
url2 <- "https://cdn.philasd.org/offices/performance/Open_Data/School_Information/School_List/2018-2019%20Master%20School%20List%20(20190510).csv"
destfile2 <- "../data/Schools.csv"
download.file(url2, destfile2, mode = "wb")

school_info_df <- read_csv("../data/Schools.csv")

#Clean school info
school_info_df <- school_info_df %>% 
  clean_names()

school_info_df <- school_info_df %>% 
  mutate( src_school_id = as.numeric(src_school_id))

#Join the two
incidents_joined_df <- incidents_df %>% 
  left_join(school_info_df, by = c("SCHOOL_ID" = "src_school_id" )) %>% 
  select(SCHOOL_ID, INCIDENT_TYPE, INCIDENT_COUNT, publication_name, school_level, admission_type, governance)

incidents_grouped_df <- incidents_joined_df %>% 
  group_by(publication_name, school_level) %>% 
  summarize(Total = sum(INCIDENT_COUNT)) %>% 
  arrange(desc(Total)) 

incidents_grouped_levels_df <- incidents_joined_df %>% 
  group_by(school_level) %>% 
  summarize(Total = sum(INCIDENT_COUNT)) %>% 
  arrange(desc(Total))

#Graph by School Level
ggplot(incidents_grouped_levels_df,
       aes(x = fct_reorder(school_level, Total),
           y = Total)) +
  geom_col(aes(fill = Total), fill = "darkseagreen4") +
  geom_text(aes(label = Total), hjust = -0.25, size = 4) +
  gghighlight(Total > 1600) +
  theme_tufte() +
  theme(legend.position = "none") +
  labs(title = "Philadelphia Serious Incidents by School Level",
       subtitle = "Data from School Year 2018-2019",
       y = "",
       x = "") +
  coord_flip(ylim = c(0, 1800)) 


