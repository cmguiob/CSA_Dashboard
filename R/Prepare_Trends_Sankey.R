# Project: CSV - Dashboard
# Date: 12.2021
# Author: Carlos Guio
# Institution: CIAT
# Objective: Prepare dataset to plot trends in PBI

# Description:
# This script reads in data of practices and services, prepares it for plotting 
# a sankey or alluvial diagram  and exports it as a compressed .csv file.

# Instructions: 
# This code is to be sourced from the script PBI_Trends.Rmd

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


# ------------------------- Prepare data ----------------------------------------

Q1_practices <- read_csv(files_paths[1]) %>%
  select(year,
         location_type, 
         location_name, 
         farmer_id,
         gender = farmer_gender,
         practice_type,
         exposure_weather,
         adopted_cases,
         drivers) %>%
  filter(!is.na(drivers)) %>%
  filter(location_type == "Site")
  mutate(option = "Practices",
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
         location_type = str_to_sentence(location_type),
         gender = str_to_sentence(gender)) %>%
  mutate_if(is.character,as.factor) 

Q1_services <- read_csv(files_paths[2]) %>%
  select(year,
         location_type, 
         location_name, 
         farmer_id,
         gender = farmer_gender,
         practice_type, 
         subtype,
         exposure_weather,
         adopted_cases,
         drivers) %>%
  # Remove rows for cases where driver is NA (adopted_case is HEARD about it)
  filter(location_type == "Site") %>%
  filter(!is.na(drivers)) %>%
  filter(subtype != "No service") %>%
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
         practice_type = str_to_sentence(paste0(practice_type,"\n", "(",subtype, ")")),
         location_type = str_to_sentence(location_type),
         gender = str_to_sentence(gender))%>%
  mutate_if(is.character,as.factor) %>%
  select(-subtype)

trends_dat <- bind_rows(Q1_practices, Q1_services)


#------------------------Export dataset for plotting ----------------------------------------------

# Remove all objects but coordinates summ
rm(list =setdiff(ls(), "trends_dat"))

# Write a compressed csv file
write_csv(trends_dat, here::here("PBI","Data_PBI","trends.csv"))

# Note: to use it in PBI, manually transform the .csv to .xlsx
