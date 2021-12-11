# Project: CSV - Dashboard
# Date: 12.2021
# Author: Carlos Guio
# Institution: CIAT
# Objective: Prepare dataset to plot uptake in PBI

# Description:
# This script reads in data of practices and services, prepares it for plotting 
# a map and a barplot  and exports it as a compressed .csv file.

# Instructions: 
# This code is to be sourced from the script PBI_Uptake.Rmd

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

files_names <- list.files(tempd, pattern = "Q1") #Read csv file names
files_paths <- paste(file.path(tempd), files_names[], sep = "\\") #Create paths


url_coords <- "https://raw.githubusercontent.com/cmguiob/CSA_Dashboard/main/Data_R/CSV_coordinates.csv"

coordinates <- read_csv(url_coords)


# ------------------------- Prepare data ----------------------------------------

#Clean
Q1_practices <- read_csv(files_paths[1]) %>%
        select(year,
               location_type, 
               location_name,
               farmer_id,
               gender = farmer_gender,
               practice_type,
               subtype,
               adopted_cases,
               description) %>%
        pivot_wider(names_from = location_type, values_from = location_name) %>%
        #unnest nested columns
        unchop(everything()) %>%
        select(-community) %>%
        distinct() %>%
        mutate(option = "Practices",
               subtype = case_when(
                    subtype == "Integrated nutrient management" ~ "Integrated management",
                    subtype == "Terraces biological measures" ~ "Terraces (bio-measures)",
                  TRUE ~ subtype),
              # Correct farmer_id which have two genders 
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
              subtype =str_to_sentence(subtype),
              gender = str_to_sentence(gender))



Q1_services <- read_csv(files_paths[2]) %>%
        select(year,
               location_type, 
               location_name, 
               farmer_id,
               gender = farmer_gender,
               practice_type,
               subtype,
               adopted_cases,
               description) %>%
        pivot_wider(names_from = location_type, values_from = location_name) %>%
        #unnest nested columns
        unchop(everything()) %>%
        select(-community) %>%
        distinct() %>%
        mutate(option = "Services",
               description = case_when(is.na(description)~ "No description available",
                                 TRUE ~ description),
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
               subtype = str_to_sentence(subtype),
               gender = str_to_sentence(gender))


Q1_options <- bind_rows(Q1_practices, Q1_services)


#Summary number of female and male headed households by site
gender_site <- Q1_options %>%
        filter(!is.na(gender)) %>%
        select(site, farmer_id, gender) %>%
        group_by(site, gender) %>%
        distinct(farmer_id) %>% 
        summarise(n_site = n()) %>%
        ungroup() %>%
        # Extra lines to account for zero 
        pivot_wider(names_from = gender, values_from = n_site) %>%
        mutate(Female = case_when(is.na(Female) ~ 0,
                            TRUE ~ as.double(Female)),
              Male = case_when(is.na(Male) ~ 0,
                          TRUE ~ as.double(Male))) %>%
        pivot_longer(cols = c("Female", "Male"), names_to = "gender", values_to = "n_site")


# Summary practices
practice_summ <- Q1_options %>%
        filter(!is.na(gender)) %>% 
        filter(!is.na(adopted_cases)) %>% 
        group_by(option, 
                 year,
                 site, 
                 practice_type, 
                 subtype,
                 description, 
                 adopted_cases) %>% 
        distinct() %>%
        count(gender) %>%
        left_join(gender_site, by = c("site", "gender"))%>%
        ungroup() %>%
        group_by(option, year, site, practice_type, gender, n_site) %>% #modif
        # Ad da new row by group
        #group_modify(~ add_row(.data = .x, adopted_cases = "Other")) %>%
        #mutate(n = case_when(is.na(n) & adopted_cases == "Other" ~ n_site - sum(n, na.rm = TRUE),
        #                     TRUE ~ as.double(n))) %>%
        mutate(percentage = case_when(n_site != 0 ~ round(n*100/n_site, 1),
                                TRUE ~ as.double(0))) 

# Top practices are not disaggregated by year
top_ad_use <- practice_summ %>%
        filter(adopted_cases == "Implemented" | adopted_cases == "Accessed and used it") %>%
        group_by(option, year, site, gender, percentage, practice_type) %>% # previous version had year
        arrange(subtype, .by_group = TRUE) %>%
        ungroup() %>%
        group_by(option, year, site, gender) %>% # previous version had year
        top_n(3, wt = percentage) %>%
        mutate(top = n():1) %>%
        # This could be used reduce noise of top practices with low percentages
        #mutate(top = case_when(percentage >= 5 ~ "*",
        #                          TRUE ~ NA_character_)) %>%
        select(option, site, practice_type, subtype, gender, adopted_cases, percentage, top) %>%  # previous version had year
        ungroup()


uptake_dat <- coordinates %>%
        #Compute coordinates for site
        group_by(country, site) %>%
        mutate(latitude_site = mean(latitude),
               longitude_site = mean(longitude)) %>%
        ungroup() %>%
        distinct(site, .keep_all = TRUE) %>%
        #Remove columns from communities
        select(-community, -longitude, -latitude)%>%
        #Join with details from communities by site and summaries
        right_join(practice_summ, by = "site") %>%
        filter(subtype != "No service") %>%
        left_join(top_ad_use, by = c("option","year","site", "practice_type", "subtype","gender","adopted_cases", "percentage"))%>%
        group_by(option, year, site, practice_type, subtype, description, gender, n_site) 


#------------------------Export dataset for plotting ----------------------------------------------

# Write a compressed csv file
write_csv(uptake_dat, here::here("PBI","Data_PBI","uptake.csv"))

# Note: to use it in PBI, manually transform the .csv to .xlsx


