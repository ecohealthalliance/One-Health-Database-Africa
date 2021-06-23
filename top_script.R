## Script to combine the individual indicators and produce a single csv
## Instructions for this part: 
## Create one top-level script that reads the metadata file, 
## processes all files through their ingest function, 
## binds data sets together, and exports the full CSV.


## load the required packages
rm(list = ls())
library(tidyverse)
library(readxl)
library(stringr)
library(docxtractr)


## read in the metadata
metadat <- read_csv("gheri_africom_indicators_metadata.csv")

# select the countries that we want.
# If we want all the countries use the function below. If we want a subset, best to subset from the 
# names outputted by the function as these have been checked to ensure they match the names used in the functions

country_names <- read_xlsx("data/AFRICOM List.xlsx") %>%
  filter(CountryName != "Egypt, Arab Rep.") %>%
  mutate_if(is.character, as.factor) %>%
  mutate(CountryName = fct_recode(CountryName, 
                                  "Democratic Republic of the Congo" = "Congo, Dem. Rep." ,
                                  "Congo" = "Congo, Rep.", 
                                  "Gambia" = "Gambia, The")) %>%
  droplevels() %>%
  pull(CountryName)

## specify the years we want to look at
chosen_years <- c("2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019")

## state the column names - this is used within the functions 
colnames_list <- c("country", "indicator", "year", "value", "units")


function_names <- metadat %>%
   select(function_names) %>%
   drop_na() %>%
   pull() %>%
   unique() %>%
   paste0("ingest_indicators", sep = ".", .)#, "()")

## At present some of the functions have numerical values in the value column and others have factors.
## I can't join these as is. As such, we produce one table for each (numeric and factor) initially
## but then combine by converting values to characters and adding a column specifying the indicator type. 


number_sets <- c("ingest_indicators.animal_health_public_sector","ingest_indicators.arable_land_hectares",
                 "ingest_indicators.arable_land_percent",
                 "ingest_indicators.cfe_allocations", "ingest_indicators.combined_data_sheet",
                 "ingest_indicators.fao_import_export","ingest_indicators.fao_livestock", 
                 "ingest_indicators.fao_protein", 
                 "ingest_indicators.fisheries_production","ingest_indicators.forest_area",
                 "ingest_indicators.land_area", "ingest_indicators.malaria_cases",
                 "ingest_indicators.medical_doctors", "ingest_indicators.population", 
                 "ingest_indicators.promed", "ingest_indicators.rabies_deaths",
                 "ingest_indicators.spar","ingest_indicators.terrestrial_protected_area", 
                 "ingest_indicators.treecover_loss", 
                 "ingest_indicators.vet_capacity", "ingest_indicators.wash_hygiene", 
                 "ingest_indicators.wash_sanitation","ingest_indicators.wash_water", 
                 "ingest_indicators.yellow_fever")

factor_sets <- c("ingest_indicators.amr", "ingest_indicators.jee", 
                 "ingest_indicators.rabies_management", "ingest_indicators.taenia_solium")

function_names_number <- function_names[which(function_names %in% number_sets)]
function_names_factor <- function_names[which(function_names %in% factor_sets)]

purrr::walk(list.files(here::here("R/"), full.names = TRUE), source)

#source("R/source_functions.R")

# run all the functions and output a list of dataframes - one for each function
outlist <- list()
for(i in 1:length(function_names_number)) {
  outlist[[i]] <- do.call(function_names_number[i], args = list())
}  

full_data_number <- bind_rows(outlist)

sort(levels(full_data_number$indicator))


## Repeat for the dataframes that are factors as the value

# run all the functions and output a list of dataframes - one for each function
outlist_factor <- list()
for(i in 1:length(function_names_factor)) {
  outlist_factor[[i]] <- do.call(function_names_factor[i], args = list())
}  ## Is there a quicker/better way to do this rather than a loop? I couldn't work out a vectorised format.

full_data_factor <- bind_rows(outlist_factor)

levels(full_data_factor$indicator)


### To combine the data sets we can have the value levels as a character and then an extra column to say whether 
### the value is factor or numeric

full_data_factor_com <- full_data_factor %>%
  mutate(value = as.character(value)) %>%
  mutate(type = "factor")

full_data_number_com <- full_data_number %>%
  mutate(value = as.character(value)) %>%
  mutate(type = "integer")

full_data_combined = rbind(full_data_factor_com, full_data_number_com)

