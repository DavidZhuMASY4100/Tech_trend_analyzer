# Library
library(tidyverse)
library(rvest)
library(xml2)
library(dplyr)
library(tidyr)

# Pre-defined variable
job_id_url <- 'https://www.indeed.com/viewjob?jk='
job_search_url <- 'https://www.indeed.com/jobs?q='

# Step 1: Read in the list of job titles ----
title <- read_csv(paste0('Job_titles.csv'),
                  show_col_types = F)
output_list <- list()

A <- Sys.time()
a <- 1
for (i in title$Job_titles) {
  # Step 2: Search for the job titles ----
  print(i)
  
  url <- paste0(job_search_url, gsub(" ", "%20", tolower(i)))
  print(url)
  page <- read_html(url)
  Sys.sleep(2)
  
  # Step 3: Find the job id for each job ----
  # Extract the Job id from the search page
  id_list <- str_split_fixed(
    page %>%
      html_node(xpath = '//*[@id="mosaic-provider-jobcards"]') %>%
      html_nodes(xpath = 'a') %>%
      html_attr("id"),
    "_",
    2)[, 2]
  print(head(id_list))
  
  
  # Step 4: Go to the job page for each job id ----
  desp_list <- list()
  # title_list <- list()
  
  for (j in id_list) {
    url = paste0(job_id_url, j)
    print(url)
    page <- read_html(url)
    
    # Wait for 1 second
    Sys.sleep(2)
    # Step 5: Extract the full job description ----
    
    desp_list[[j]] <- page %>%
      html_nodes(xpath = '//*[@id="jobDescriptionText"]') %>%
      html_text()
    
    for (k in 1:length(desp_list)) {
      if (is_empty(desp_list[[k]])) {
        desp_list[[k]] <- 'N/A'
      }
    }
  }
  
  # Step 6: Store the results for each job to the output ----
  output_list[[i]] <- bind_rows(
    job_id = id_list,
    job_title = i,
    job_desp = unlist(desp_list)
  )
  Sys.sleep(2)
  
  print(paste0('----------', a, " out of ", nrow(title), " done!---------"))
  a <- a+1
}

B <- Sys.time()
print(B - A)

# Step 7: Convert List object to data frame ----
output <- data.frame()
for (l in output_list) {
  if (nrow(l) > 0) {
    output <- bind_rows(output, l)
  }
}

scrap_path <- 'scrap_files'

write_delim(output %>% mutate(scrap_date = Sys.Date()), 
            paste0(file.path(scrap_path, 
                   str_c('Job Description List ', Sys.Date(), '.csv'))), delim = "|")




