# Temporary script to clean output from top_script_api. Merge dual variables for vets and rename spar labels. 

library(tidyverse)

full_data_combined_api<- read_csv("Output/full_data_combined_api.csv")

# merge vet data from vet_capacity (oie_wahis) and Vets (combined_data_sheet)
total_vets <- full_data_combined_api %>% 
  filter(indicator == "Vets" | indicator == "vet_capacity") %>%
  mutate(year = as.numeric(as.character(year))) %>% 
  mutate(units = "Number") %>% 
  group_by(country,year) %>% 
  mutate(n = n()) %>% 
  filter (n==1 | n==2 & !is.na(value)) %>% #Note: Duplicate for Sudan 2017 
  select(-c(n)) %>% 
  mutate(indicator = "total_vets") %>% 
  distinct() 

full_data_combined_api_clean <- rbind(full_data_combined_api, total_vets) %>% 
  filter(indicator != "Vets" &  indicator != "vet_capacity")


# rename SPAR labels
full_data_combined_api_clean <- full_data_combined_api_clean  %>% 
  mutate(indicator = fct_recode(indicator, 
                                "C.3.1 Collaborative effort on activities to address zoonoses" = "C.3.1",
                                "C.4.1 Multisectoral collaboration mechanism for food safety events" = "C.4.1",
                                "C.5.1 Specimen referral and transport system" = "C.5.1", 
                                "C.5.3 Access to laboratory testing capacity for priority diseases" = "C.5.3",
                                "C.6.1 Early warning function: indicator- and event-based surveillance" = "C.6.1",
                                "C.6.2 Mechanism for event management (verification, risk assessment, analysis investigation)" = "C.6.2",
                                "C.11.1 Core capacity requirements at all times for designated airports, ports and ground crossings" = "C.11.1",
                                "C.11.2 Effective public health response at points of entry" = "C.11.2")) 



write.csv(full_data_combined_api_clean, "Output/full_data_combined_api_clean.csv", row.names = F)

