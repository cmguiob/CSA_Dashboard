# Project: CSV - Dashboard
# Date: 12.2021
# Author: Carlos Guio
# Institution: CIAT
# Objective: Prepare dataset to plot effects in PBI

# Description:
# This script reads in data of practices and services, prepares it for plotting 
# a tornado plot  and exports it as a compressed .csv file.

# Instructions: 
# This code is to be sourced from the script PBI_Effects.Rmd

# ------------------------------------ Libraries --------------------------------

library(tidyverse)
library(here)

# ------------------------------------ Read data -------------------------------


#Create temporal objects to read compressed data
temp <- tempfile() 
tempd <- tempdir()

url_data <- "https://github.com/cmguiob/CSA_Dashboard/raw/main/Data_R/Data_Q1_Q2.zip"

download.file(url_data, temp, mode="wb") ##Download. mode is necessary for windows

unzip(temp, exdir=tempd) #Unzip in temporal directory

files_names_1 <- list.files(tempd, pattern = "Q1") #Read csv file names
files_paths_1 <- paste(file.path(tempd), files_names_1[], sep = "\\") #Create paths

files_names_2 <- list.files(tempd, pattern = "Q2") #Read csv file names
files_paths_2 <- paste(file.path(tempd), files_names_2[], sep = "\\") #Create paths


# ------------------------- Prepare data ----------------------------------------

# Define Q1_practices
Q1_practices <- read_csv(files_paths_1[1]) %>%
      select(location_type, 
             location_name, 
             farmer_id, 
             gender = farmer_gender,
             year, 
             practice_type,
             subtype,
             adopted_cases) %>%
      pivot_wider(names_from = location_type, values_from = location_name) %>%
      unchop(everything()) %>%
      mutate(option = "Practices",
             farmer_id = case_when(
                  farmer_id == "337a45e573817fba21d251ecc3a37523e14cc62d073" & 
                  gender == "Female" ~ "337a45e573817fba21d251ecc3a37523e14cc62d073_F",
                  farmer_id == "3e2a4c557a0a5277638e42df23b17bd63eb41858faa" & 
                  gender == "Female" ~ "3e2a4c557a0a5277638e42df23b17bd63eb41858faa_F",
                  farmer_id == "4e6a1f557611aec6b347a05cde3a1e093fb88e883a0" & 
                  gender == "Female" ~ "4e6a1f557611aec6b347a05cde3a1e093fb88e883a0_F",
                  farmer_id == "533a330570d0a9e20fdd55e40993645a7c9838e11b0" & 
                  gender == "Female" ~ "533a330570d0a9e20fdd55e40993645a7c9838e11b0_F",
                  farmer_id == "27fa75957420473f9ad14deb5ed9b7701031f701149" & 
                  gender == "Female" ~ "27fa75957420473f9ad14deb5ed9b7701031f701149_F",
                  farmer_id == "a75acb457efe9fb18b082f3a52d2dad46a69a42cf8e" & 
                  gender == "Female" ~ "a75acb457efe9fb18b082f3a52d2dad46a69a42cf8e_F",
                  farmer_id == "112a7c157220d8de79f29b9ac7bf6af20daa0a05983" & 
                  gender == "Female" ~ "112a7c157220d8de79f29b9ac7bf6af20daa0a05983_F",
                  farmer_id == "fd7a65f575e4ac103cbe3b6832d559cbc199d82101b" & 
                  gender == "Female" ~ "fd7a65f575e4ac103cbe3b6832d559cbc199d82101b_F",
                  TRUE ~ farmer_id),
             practice_type = str_to_sentence(practice_type),
             gender = str_to_sentence(gender))

# Define Q1_services
Q1_services <- read_csv(files_paths_1[2]) %>%
         select(location_type, 
                location_name, 
                farmer_id, 
                gender = farmer_gender,
                year, 
                practice_type,
                subtype,
                adopted_cases) %>%
         pivot_wider(names_from = location_type, values_from = location_name) %>%
         unchop(everything()) %>%
         mutate(option = "Services",
                farmer_id = case_when(
                      farmer_id == "337a45e573817fba21d251ecc3a37523e14cc62d073" & 
                      gender == "Female" ~ "337a45e573817fba21d251ecc3a37523e14cc62d073_F",
                      farmer_id == "3e2a4c557a0a5277638e42df23b17bd63eb41858faa" & 
                      gender == "Female" ~ "3e2a4c557a0a5277638e42df23b17bd63eb41858faa_F",
                      farmer_id == "4e6a1f557611aec6b347a05cde3a1e093fb88e883a0" & 
                      gender == "Female" ~ "4e6a1f557611aec6b347a05cde3a1e093fb88e883a0_F",
                      farmer_id == "533a330570d0a9e20fdd55e40993645a7c9838e11b0" & 
                      gender == "Female" ~ "533a330570d0a9e20fdd55e40993645a7c9838e11b0_F",
                      farmer_id == "27fa75957420473f9ad14deb5ed9b7701031f701149" & 
                      gender == "Female" ~ "27fa75957420473f9ad14deb5ed9b7701031f701149_F",
                      farmer_id == "a75acb457efe9fb18b082f3a52d2dad46a69a42cf8e" & 
                      gender == "Female" ~ "a75acb457efe9fb18b082f3a52d2dad46a69a42cf8e_F",
                      farmer_id == "112a7c157220d8de79f29b9ac7bf6af20daa0a05983" & 
                      gender == "Female" ~ "112a7c157220d8de79f29b9ac7bf6af20daa0a05983_F",
                      farmer_id == "fd7a65f575e4ac103cbe3b6832d559cbc199d82101b" & 
                      gender == "Female" ~ "fd7a65f575e4ac103cbe3b6832d559cbc199d82101b_F",
                      TRUE ~ farmer_id),
         practice_type = str_to_sentence(practice_type),
         gender = str_to_sentence(gender)) 

# Define Q2 practices
Q2_practices <- read_csv(files_paths_2[1]) %>%
        select(year,
               site,
               farmer_id,
               gender = farmer_gender,
               practice_type,
               outcome_category,
               indicator,
               answer) %>%
        mutate(option = "Practices",
               practice_type = str_to_sentence(practice_type),
               indicator = case_when(
                     indicator == "CSA option increased production" ~ "Increased production",
                     indicator == "CSA driven decrease in vulnerability" ~ 
                       "Reduced climate vulner.",
                     indicator == "Decreased agricultural labour time" ~ "Decreased labour time",
                     indicator == "Increased agricultural labour time" ~ "Increased labour time",
                     indicator == "Participation on use of CSA income" ~ 
                       "Participated on CSA-income use",
                     TRUE ~ indicator),
               gender = str_to_sentence(gender)) 

# Define Q2_services
Q2_services <-  read_csv(files_paths_2[2]) %>%
        select(year,
               site,
               farmer_id,
               gender = farmer_gender,
               practice_type = service_type,
               outcome_category,
               indicator,
               answer) %>%
        mutate(option = "Services",
               practice_type = str_to_sentence(practice_type),
               gender = str_to_sentence(gender))


# Number of farmers by gender that implemented practices at each site
n_site_implemented <- Q1_practices %>%
        filter(!is.na(gender)) %>% 
        filter(adopted_cases == "Implemented") %>% 
        select(site, gender, farmer_id) %>% 
        group_by(site, gender) %>%
        distinct(farmer_id) %>%
        summarise(n_site = n()) %>%
        ungroup()

# Percentage of implementing farmers who answered "yes" to the indicator effect 
effect_practices <- Q2_practices %>%
        filter(answer == "yes")  %>%
        select(year, 
               site, 
               farmer_id, 
               gender, 
               outcome_category, 
               practice_type, 
               indicator) %>%
        group_by(year, 
                 site, 
                 outcome_category, 
                 practice_type, 
                 indicator, 
                 gender) %>%
        distinct() %>% 
        count() %>%
        ungroup() %>%
        left_join(n_site_implemented, by = c("site", "gender")) %>%
        mutate(percentage = round(n*100/n_site,1))


# Number of farmers by gender that accessed and used services at each site
n_site_used <- Q1_services %>%
        filter(!is.na(gender)) %>% 
        filter(adopted_cases == "Accessed and used it") %>% 
        select(site, gender, farmer_id) %>% 
        group_by(site, gender) %>%
        distinct(farmer_id) %>%
        summarise(n_site = n()) %>%
        ungroup()

# Percentage of CIS-using farmers who answered "yes" to the indicator effect 
effect_services <- Q2_services %>%
        filter(answer == "yes")  %>%
        filter(practice_type != "No service") %>%
        select(year, site, farmer_id, gender, outcome_category, practice_type, indicator) %>%
        group_by(year, 
                 site, 
                 outcome_category, 
                 practice_type, 
                 indicator, 
                 gender) %>%
        distinct() %>% 
        count() %>%
        ungroup() %>%
        left_join(n_site_used, by = c("site", "gender")) %>%
        mutate(percentage = round(n*100/n_site,1))


effect_dat <- bind_rows(effect_practices %>% mutate(option = "Practices"),
                        effect_services %>% mutate(option = "Services") )

rm(effect_practices, effect_services, n_site_implemented, n_site_used)

#------------------------Export dataset for plotting ----------------------------------------------

# Write a compressed csv file
write_csv(effect_dat, here::here("PBI","Data_PBI","effects.csv"))




