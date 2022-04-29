library(purrr)
library(dplyr)
library(tidyr)
library(stringr)


## loading all delim files
csv_list <- list.files(path = "scrap_files", pattern = "*.csv")

## concatenate all df into one
df <- map_df(file.path("scrap_files", csv_list), 
             read.delim, header = TRUE, sep = '|')

## load all predefined technologies

tech_list <- read.csv("technologies_list.csv")

## join the technology with job descriptions

joined_df <- tech_list %>% full_join(df, by = character())
counted_joined_df <- 
  joined_df %>%
  mutate(count = if_else(str_detect(job_desp, Technology), 1, 0)) %>% 
  group_by(Category, Technology, scrap_date) %>% 
  summarize(frequency = sum(count))


## aggregate data by counting frequency for all technologies
pivoted_df <- 
  counted_joined_df %>% 
  pivot_wider(id_cols = c(Category, Technology), 
              names_from = scrap_date, 
              values_from = frequency)


