# Project: CSV - Dashboard
# Date: 12.2021
# Author: Carlos Guio
# Institution: CIAT
# Objective: Prepare dataset to plot summaries by location in PBI

# Description:
# This script reads in data of practices and services, prepares it for plotting 
# a map with summaries by location and exports it as a compressed .csv file.

# Instructions: 
# This code is to be sourced from the script PBI_Locations.Rmd

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


# ------------------------- Prepare data I ----------------------------------------

# Define Q1_practices
Q1_practices <- read_csv(files_paths[1]) %>%
  select(location_type, 
         location_name, 
         hh_address,
         hh_head_gender,
         head,
         farmer_id, 
         farmer_gender,
         year, 
         exposure_weather, 
         weather_shocks,
         outcome_category,
         practice_type,
         adopted_cases,
         mitigation,
         gender_impact) %>%
  pivot_wider(names_from = location_type, values_from = location_name) %>%
  unchop(everything()) %>%
  mutate(option = "Practices",
         farmer_id = case_when(
           farmer_id == "337a45e573817fba21d251ecc3a37523e14cc62d073" & 
             farmer_gender == "Female" ~ "337a45e573817fba21d251ecc3a37523e14cc62d073_F",
           farmer_id == "3e2a4c557a0a5277638e42df23b17bd63eb41858faa" & 
             farmer_gender == "Female" ~ "3e2a4c557a0a5277638e42df23b17bd63eb41858faa_F",
           farmer_id == "4e6a1f557611aec6b347a05cde3a1e093fb88e883a0" & 
             farmer_gender == "Female" ~ "4e6a1f557611aec6b347a05cde3a1e093fb88e883a0_F",
           farmer_id == "533a330570d0a9e20fdd55e40993645a7c9838e11b0" & 
             farmer_gender == "Female" ~ "533a330570d0a9e20fdd55e40993645a7c9838e11b0_F",
           farmer_id == "27fa75957420473f9ad14deb5ed9b7701031f701149" & 
             farmer_gender == "Female" ~ "27fa75957420473f9ad14deb5ed9b7701031f701149_F",
           farmer_id == "a75acb457efe9fb18b082f3a52d2dad46a69a42cf8e" & 
             farmer_gender == "Female" ~ "a75acb457efe9fb18b082f3a52d2dad46a69a42cf8e_F",
           farmer_id == "112a7c157220d8de79f29b9ac7bf6af20daa0a05983" & 
             farmer_gender == "Female" ~ "112a7c157220d8de79f29b9ac7bf6af20daa0a05983_F",
           farmer_id == "fd7a65f575e4ac103cbe3b6832d559cbc199d82101b" & 
             farmer_gender == "Female" ~ "fd7a65f575e4ac103cbe3b6832d559cbc199d82101b_F",
           TRUE ~ farmer_id),
         practice_type = str_to_sentence(practice_type),
         farmer_gender = str_to_sentence(farmer_gender),
         hh_head_gender = str_to_sentence(hh_head_gender),
         gender_impact = case_when(practice_type == "Irrigation and groundwater pumping" ~
                                     "no",
                                   practice_type == "Water harvesting" ~ "no",
                                   practice_type == "Pasture management" ~ "no",
                                   TRUE ~ gender_impact))


# Define Q1_services
Q1_services <- read_csv(files_paths[2]) %>%
  select(location_type, 
         location_name, 
         hh_address,
         hh_head_gender,
         head,
         farmer_id, 
         farmer_gender,
         year, 
         exposure_weather,
         weather_shocks,
         practice_type,
         adopted_cases) %>%
  pivot_wider(names_from = location_type, values_from = location_name) %>%
  unchop(everything()) %>%
  mutate(option = "Services",
         farmer_id = case_when(
           farmer_id == "337a45e573817fba21d251ecc3a37523e14cc62d073" & 
             farmer_gender == "Female" ~ "337a45e573817fba21d251ecc3a37523e14cc62d073_F",
           farmer_id == "3e2a4c557a0a5277638e42df23b17bd63eb41858faa" & 
             farmer_gender == "Female" ~ "3e2a4c557a0a5277638e42df23b17bd63eb41858faa_F",
           farmer_id == "4e6a1f557611aec6b347a05cde3a1e093fb88e883a0" & 
             farmer_gender == "Female" ~ "4e6a1f557611aec6b347a05cde3a1e093fb88e883a0_F",
           farmer_id == "533a330570d0a9e20fdd55e40993645a7c9838e11b0" & 
             farmer_gender == "Female" ~ "533a330570d0a9e20fdd55e40993645a7c9838e11b0_F",
           farmer_id == "27fa75957420473f9ad14deb5ed9b7701031f701149" & 
             farmer_gender == "Female" ~ "27fa75957420473f9ad14deb5ed9b7701031f701149_F",
           farmer_id == "a75acb457efe9fb18b082f3a52d2dad46a69a42cf8e" & 
             farmer_gender == "Female" ~ "a75acb457efe9fb18b082f3a52d2dad46a69a42cf8e_F",
           farmer_id == "112a7c157220d8de79f29b9ac7bf6af20daa0a05983" & 
             farmer_gender == "Female" ~ "112a7c157220d8de79f29b9ac7bf6af20daa0a05983_F",
           farmer_id == "fd7a65f575e4ac103cbe3b6832d559cbc199d82101b" & 
             farmer_gender == "Female" ~ "fd7a65f575e4ac103cbe3b6832d559cbc199d82101b_F",
           TRUE ~ farmer_id),
         practice_type = str_to_sentence(practice_type),
         farmer_gender = str_to_sentence(farmer_gender),
         hh_head_gender = str_to_sentence(hh_head_gender),
         mitigation = "Does not apply",
         gender_impact = "Does not apply",
         outcome_category = "Does not apply") 

Q1_options <- bind_rows(Q1_practices, Q1_services)

Q1_options

#--------------------------- Prepare data II: summaries ------------------------------------

# Different summaries are prepared separately and then joined in a single data frame. Summaries are general, farmer-wise and household-wise

# General summaries ***********************************************************************

# Summarize years surveyed per site 

summ_yea <- Q1_options %>% 
  group_by(region, country, site) %>% 
  distinct(year) %>% 
  count() %>% 
  mutate(category = "Years")  %>% 
  ungroup()

details_years <- aggregate(year ~ site, unique(Q1_options %>% 
                                                 group_by(region, country, site) %>% 
                                                 distinct(year)), paste, collapse = "\n ") 


# Summarize number of communities per site 

summ_com <- Q1_options %>% 
  group_by(region, country, site) %>% 
  distinct(community) %>% 
  count() %>% 
  mutate(category = "Communities")  %>% 
  ungroup()


# Summarize practices evaluated at each site 
summ_prac <- Q1_practices %>% 
  group_by(region, country, site) %>% 
  distinct(practice_type) %>% 
  count() %>% 
  mutate(category = "Practices")  %>% 
  ungroup()

details_practices <- aggregate(practice_type ~ site, 
                               unique(Q1_practices %>%
                                        group_by(region, country, site) %>%
                                        distinct(practice_type, .keep_all = TRUE) %>%
                                        mutate(practice_type = case_when(
                                          mitigation == "yes" & gender_impact == "yes"~ 
                                            paste(practice_type,"(m & g)"),
                                          mitigation == "yes" & gender_impact == "no"~ 
                                            paste(practice_type,"(m)"),
                                          mitigation == "no" & gender_impact == "yes"~
                                            paste(practice_type,"(g)"),
                                          mitigation == "no" & gender_impact == "no"~
                                            practice_type))), 
                               paste, collapse = "\n ") 


# Household - practice summaries **********************************************************************

# Summarize number of households
summ_hh <- Q1_options %>% 
  group_by(region, country, site) %>% 
  distinct(hh_address, .keep_all = TRUE) %>% 
  count() %>% 
  mutate(category = "Households") %>% 
  ungroup()

# Summarize number of households by gender
# IMPORTANT: If grouped by hh_head_gender before distinct, more rows are obtained, because some households answered twice, both as male and female headed (each giving different answers to weather shocks, driver, adoption_case, e.g. KAF-03-013). 
# On the contrary, if distinct is used first, some households are deleted, leaving just the hh_head_gender whose row appears first (e.g. doy-03-019, KAF-03-001).
summ_hh_g <- Q1_options %>% 
  group_by(region, country, site, hh_head_gender)  %>% 
  distinct(hh_address, .keep_all = TRUE) %>% 
  count() %>% 
  mutate(category = "Households",
         hh_head_gender = case_when(
           hh_head_gender == "Female-headed" ~ "female-headed",
           hh_head_gender == "Male-headed" ~ "male-headed",
           TRUE ~ "gender not reported"
         )) %>% 
  ungroup()


# Summarize practice-implementing households
#IMPORTANT: filter is aplied here after adopted cases have been modifiyed to account for at least one implemented practice.
summ_adhh <- Q1_practices %>% 
  select(region, 
         country, 
         site, 
         hh_address, 
         practice_type, 
         adopted_cases) %>% 
  group_by(hh_address) %>%
  # For each household, if there is any "implementing" case, label household as implementing
  mutate(adopted_cases = case_when(any(str_detect(adopted_cases, "Implemented")) ~
                                     "Implemented",
                                   TRUE ~ "Didn't implement")) %>%
  filter(adopted_cases == "Implemented") %>%
  group_by(region, country, site)  %>%  
  distinct(hh_address, .keep_all = TRUE) %>% 
  count() %>% 
  mutate(category = "Households implementing")  %>% 
  ungroup()


# Details of of implementing households %
details_adoptant_hh <- summ_hh %>% 
  ungroup() %>%
  select(site, n) %>% 
  inner_join(summ_adhh %>% 
               ungroup()%>% 
               select(site,n), 
             by = "site") %>%
  mutate(adoptant_hh_perc = paste0(round(n.y*100/n.x, 0), 
                                   "% implemented\n at least 1 practice."))%>%
  select(-n.x, -n.y) 


# Summarize practice-implementing households disaggregated by gender
summ_adhh_g <- Q1_practices %>% 
  select(region, 
         country, 
         site, 
         hh_address, 
         hh_head_gender,
         adopted_cases,
         practice_type) %>% 
  group_by(hh_address) %>%
  mutate(adopted_cases = case_when(any(str_detect(adopted_cases, "Implemented")) ~
                                     "Implemented",
                                   TRUE ~ "Didn't implement")) %>%
  filter(adopted_cases == "Implemented")  %>% 
  group_by(region, country, site, hh_head_gender)  %>%  
  distinct(hh_address, .keep_all = TRUE) %>% 
  count() %>% 
  mutate(category = "Households implementing",
         hh_head_gender = case_when(
           hh_head_gender == "Female-headed" ~ "female-headed",
           hh_head_gender == "Male-headed" ~ "male-headed",
           TRUE ~ "gender unreported"
         ))  %>% 
  ungroup()

# Details of implementing households by gender %
details_adoptant_hh_g <- summ_adhh %>% 
  ungroup() %>%
  select(site, n) %>% 
  inner_join(summ_adhh_g %>% 
               ungroup()%>%
               select(site, hh_head_gender, n), 
             by =  "site") %>%
  filter(hh_head_gender == "female-headed" | 
           hh_head_gender == "male-headed") %>%
  mutate(adoptant_hh_g_perc = paste0(round(n.y*100/n.x, 0), 
                                     "%  were ",
                                     paste(hh_head_gender)))%>%
  select(-n.x, -n.y, -hh_head_gender) %>%
  aggregate(adoptant_hh_g_perc ~ site,
            data = ., 
            paste,
            collapse = "\n ") %>%
  inner_join(details_adoptant_hh) %>%
  mutate(adoptant_hh_g_perc = paste(adoptant_hh_perc, 
                                    "From them:", 
                                    adoptant_hh_g_perc, 
                                    sep= "\n")) %>%
  select(-adoptant_hh_perc)


# Household - service summaries **********************************************************************

# Summarize number households that accessed CIS
summ_acchh <- Q1_services %>% 
  filter(adopted_cases != "No accessed") %>%
  filter(!is.na(adopted_cases))  %>%
  group_by(region, country, site)  %>%  
  distinct(hh_address, .keep_all = TRUE) %>% 
  count() %>% 
  mutate(category = "Households accessing CIS")  %>% 
  ungroup()

# Details of CIS accessing-households %
details_access_hh <- summ_hh %>% 
  ungroup() %>%
  select(site, n) %>% 
  inner_join(summ_acchh %>% 
               ungroup() %>%
               select(site,n), by = "site") %>%
  mutate(access_hh_perc = paste0(round(n.y*100/n.x, 0), 
                                 "% accessed CIS"))%>%
  select(-n.x, -n.y) 

# Summarize number of CIS accessing-households by gender
summ_acchh_g <- Q1_services %>% 
  filter(adopted_cases != "No accessed") %>%
  filter(!is.na(adopted_cases)) %>% 
  group_by(region, country, site, hh_head_gender)  %>%  
  distinct(hh_address, .keep_all = TRUE) %>%  
  count() %>% 
  mutate(category = "Households accessing CIS",
         hh_head_gender = case_when(
           hh_head_gender == "Female-headed" ~ "female-headed",
           hh_head_gender == "Male-headed" ~ "male-headed",
           TRUE ~ "gender unreported"
         ))  %>% 
  ungroup()

# Details of CIS-accessing households by gender %
details_access_hh_g <- summ_acchh %>% 
  ungroup() %>%
  select(site, n) %>% 
  inner_join(summ_acchh_g %>% 
               ungroup()%>%
               select(site, hh_head_gender, n), 
             by =  "site") %>%
  mutate(access_hh_g_perc = paste0(round(n.y*100/n.x, 0), 
                                   "%  were ", 
                                   paste(hh_head_gender)))%>%
  select(-n.x, -n.y, -hh_head_gender) %>%
  aggregate(access_hh_g_perc ~ site,
            data = ., 
            paste, 
            collapse = "\n ") %>%
  inner_join(details_access_hh) %>%
  mutate(access_hh_g_perc = paste(access_hh_perc, 
                                  "From them:", 
                                  access_hh_g_perc, 
                                  sep= "\n")) %>%
  select(-access_hh_perc)


# Household - weather summaries ************************************************************

#Summarize number of households that experienced a weather shock
summ_weaxp <- Q1_practices %>% 
  filter(exposure_weather == "yes") %>%
  group_by(region, country, site) %>%  
  distinct(hh_address) %>% 
  count() %>% 
  mutate(category = "Weather schocks")  %>% 
  ungroup()

# Percentage of households that experienced a weather shock
summ_weaxp_per <- summ_hh %>% 
  select(-category) %>% 
  inner_join(summ_weaxp %>% 
               select(-category), 
             by = c("region", "country", "site")) %>%
  mutate(n = round(n.y*100/n.x, 0),
         category = "Weather shocks")%>%
  select(-n.x, -n.y)


#Summarize number of households that made changes due to weather shocks.
summ_weach <- Q1_practices %>% 
  filter(outcome_category == "Changes due to weather events" &
           exposure_weather == "yes") %>%
  group_by(region, country, site) %>%  
  distinct(hh_address) %>% 
  count() %>% 
  mutate(category = "Changes due to weather")  %>% 
  ungroup()

# Summarize number of households that made autonomous changes
summ_weaau <- Q1_practices %>% 
  filter(outcome_category == "Autonomous changes" &
           exposure_weather == "yes") %>%
  group_by(region, country, site) %>%  
  distinct(hh_address) %>% 
  count() %>% 
  mutate(category = "Autonomous changes")  %>% 
  ungroup()


# Percentage of households that made changes and experienced a weather shock
summ_weach_per <- summ_weaxp %>% 
  select(-category) %>% 
  inner_join(summ_weach %>% 
               select(-category), 
             by = c("region", "country", "site")) %>%
  mutate(perc = round(n.y*100/n.x, 0),
         category = paste(paste0(perc, "%"),
                          "made changes due to weather"))%>%
  select(-n.y, -perc) %>%
  inner_join(summ_weaau %>% 
               select(-category), 
             by = c("region", "country", "site")) %>%
  mutate(perc = round(n*100/n.x, 0),
         category = paste(category,
                          "\n and", 
                          paste0(perc, "%"),
                          "autonomous changes."))%>%
  select(-n.x, -n, -perc)


#Details weather: list of top3 and % changes due to weather
details_weashk <- Q1_practices %>% 
  filter(exposure_weather == "yes",
         weather_shocks != "Not reported") %>%
  group_by(site, weather_shocks) %>% 
  distinct(hh_address) %>% 
  count() %>% 
  ungroup() %>%
  group_by(site, n) %>%
  arrange(weather_shocks, .by_group = TRUE) %>%
  ungroup() %>%
  group_by(site) %>%
  top_n(3) %>%
  select(-n) %>%
  aggregate(weather_shocks ~ site,
            data = ., 
            paste, 
            collapse = "\n ") %>%
  left_join(summ_weach_per %>%
              select(site, category),
            by = c("site")) %>%
  mutate(weather_details = paste(category,"The most common shocks were:" ,weather_shocks, sep = "\n")) %>%
  select(-category, -weather_shocks)


# Farmers - practice summaries **********************************************************************

# Summarize number of farmers per site
summ_frm <- Q1_options %>% 
  group_by(region, country, site) %>% 
  distinct(farmer_id, .keep_all = TRUE) %>% 
  count() %>% 
  mutate(category = "Farmers")  %>% 
  ungroup()

#Summarize farmers per site by gender
summ_frm_g <- Q1_options %>% 
  group_by(region, country, site, farmer_gender)  %>% 
  distinct(farmer_id, .keep_all = TRUE) %>% 
  count() %>% 
  mutate(category = "Farmers",
         farmer_gender = case_when(
           farmer_gender == "Female" ~ "Female",
           farmer_gender == "Male" ~ "Male",
           TRUE ~ "gender  not reported"
         )) %>% 
  ungroup()

# Summarize number of farmers per site that implemented at least one practice
summ_adfrm <- Q1_practices %>% 
  select(region, 
         country, 
         site, 
         farmer_id, 
         practice_type, 
         adopted_cases) %>% 
  group_by(farmer_id) %>%
  mutate(adopted_cases = case_when(any(str_detect(adopted_cases, "Implemented")) ~
                                     "Implemented",
                                   TRUE ~ "Didn't implement")) %>%
  filter(adopted_cases == "Implemented") %>%
  group_by(region, country, site)  %>% 
  distinct(farmer_id) %>% 
  count() %>% 
  mutate(category = "Farmers implementing") %>% 
  ungroup()

# Details percentage of farmers that implemented at least one practice
details_adoptant_frm <- summ_frm %>% 
  ungroup() %>%
  select(site,n) %>% 
  inner_join(summ_adfrm %>% 
               ungroup()%>%
               select(site,n), 
             by = "site") %>%
  mutate(adoptant_frm_perc = paste0(round(n.y*100/n.x, 0), 
                                    "% implemented\n at least 1 practice."))%>%
  select(-n.x, -n.y) 

# Summarize number of implementing-farmers by gender
summ_frm_g <- Q1_practices %>% 
  select(region, 
         country, 
         site, 
         farmer_id, 
         farmer_gender,
         practice_type, 
         adopted_cases) %>% 
  group_by(farmer_id) %>%
  mutate(adopted_cases = case_when(any(str_detect(adopted_cases, "Implemented")) ~
                                     "Implemented",
                                   TRUE ~ "Didn't implement")) %>%
  filter(adopted_cases == "Implemented")  %>% 
  group_by(region, country, site, farmer_gender)  %>%  
  distinct(farmer_id, .keep_all = TRUE) %>% 
  count() %>% 
  mutate(category = "Farmers implementing",
         farmer_gender = case_when(
           farmer_gender == "Female" ~ "female",
           farmer_gender == "Male" ~ "male",
           TRUE ~ "gender unreported"
         ))  %>% 
  ungroup()

# Details of implementing farmers by gender %
details_adoptant_frm_g <- summ_frm %>% 
  ungroup() %>%
  select(site, n) %>% 
  inner_join(summ_frm_g %>% 
               ungroup()%>%
               select(site, farmer_gender, n), 
             by =  "site") %>%
  filter(farmer_gender == "female" | 
           farmer_gender == "male") %>%
  mutate(adoptant_frm_g_perc = paste0(round(n.y*100/n.x, 0), 
                                      "%  were ",
                                      paste(farmer_gender)))%>%
  select(-n.x, -n.y, -farmer_gender) %>%
  aggregate(adoptant_frm_g_perc ~ site,
            data = ., 
            paste,
            collapse = "\n ") %>%
  inner_join(details_adoptant_frm) %>%
  mutate(adoptant_frm_g_perc = paste(adoptant_frm_perc, 
                                     "From them:", 
                                     adoptant_frm_g_perc, 
                                     sep= "\n")) %>%
  select(-adoptant_frm_perc)


# Farmers - service summaries **********************************************************************


# Summarize number of CIS accessing-farmers
summ_accfrm <- Q1_services %>% 
  filter(adopted_cases != "No accessed") %>%
  filter(!is.na(adopted_cases))  %>%
  group_by(region, country, site)  %>%  
  distinct(farmer_id, .keep_all = TRUE) %>% 
  count() %>% 
  mutate(category = "Farmers accessing CIS")  %>% 
  ungroup()

# Details of CIS accessing-farmers %
details_access_frm <- summ_frm %>% 
  ungroup() %>%
  select(site, n) %>% 
  inner_join(summ_accfrm %>% 
               ungroup() %>%
               select(site,n), by = "site") %>%
  mutate(access_frm_perc = paste0(round(n.y*100/n.x, 0), 
                                  "% accessed CIS"))%>%
  select(-n.x, -n.y) 


# Joined summaries ***********************************************************************

summaries <- rbind(summ_yea, summ_com, summ_prac, summ_hh, summ_weaxp_per, summ_frm)

#Read coordinates df
url_coords <- "https://raw.githubusercontent.com/cmguiob/CSA_Dashboard/main/Data_R/CSV_coordinates.csv"
coordinates <- read_csv(url_coords)

# Get communities by site
details_communities <- aggregate(community ~ site, unique(coordinates), paste, collapse = "\n ") 

# Join summaries with coordinates
locations_dat <- coordinates %>%
  #Copute coordinates for site
  group_by(site) %>%
  mutate(latitude_site = mean(latitude),
         longitude_site = mean(longitude)) %>%
  ungroup() %>%
  distinct(site, .keep_all = TRUE) %>%
  #Remove columns from communities
  select(-community, -longitude, -latitude)%>%
  #Join with details from communities by site and summaries
  right_join(details_communities, by = "site") %>%
  right_join(details_years, by = "site") %>%
  right_join(details_practices, by = "site") %>%
  right_join(details_access_hh, by = "site") %>%
  right_join(details_adoptant_hh_g, by = "site") %>%
  right_join(details_access_frm, by = "site") %>%
  right_join(details_adoptant_frm_g, by = "site") %>%
  right_join(details_weashk, by = "site") %>%
  right_join(summaries, by = c("region", "country", "site")) %>%
  # Edit deails column
  mutate(details = case_when(
    category == "Years" ~ as.character(year),
    category == "Practices" ~ as.character(practice_type),
    category == "Households" ~ as.character(paste(paste0(access_hh_perc,","),
                                                  adoptant_hh_g_perc,
                                                  sep = "\n")),
    category == "Weather shocks" ~ as.character(weather_details),
    category == "Farmers" ~ as.character(paste(paste0(access_frm_perc,","),
                                               adoptant_frm_g_perc,
                                               sep = "\n")),
    TRUE ~ as.character(community)
  )) %>%
  select(-community, -year, -practice_type, -adoptant_hh_g_perc, -adoptant_frm_g_perc, - weather_details, - access_hh_perc, - access_frm_perc)



#--------------------------- Export dataset for plotting ----------------------------------

# Remove all objects but coordinates summ
rm(list =setdiff(ls(), "locations_dat"))

# Write a compressed csv file
write_csv(locations_dat, here::here("PBI","Data_PBI","locations.csv"))


