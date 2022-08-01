## Script to combine the individual indicators and produce a single csv

## This script uses the functions that use api calls or direct downloads if they are available. 

## load the required packages
#rm(list = ls())
source("packages.R")

## read in the metadata
metadat <- read_csv("gheri_africom_indicators_metadata.csv")

# select the countries that we want.
# If we want all the countries use the function below. If we want a subset, best to subset from the 
# names outputted by the function as these have been checked to ensure they match the names used in the functions

country_names <- read_xlsx("data/AFRICOM List.xlsx") %>%
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

# for world bank data need to specify the dates we want to select in the api call
dates_to_pull <- "2010:2019"

# extract the function names from the metadata csv
function_names <- metadat %>%
  select(api_call_or_direct_download) %>%
  drop_na() %>%
  pull() %>%
  unique() %>%
  paste0("ingest_indicators", sep = ".", .)#, "()")

## At present some of the functions have numerical values in the value column and others have factors.
## Will produce one dataframe with the numerical values and one with factors. 
## These can later be combined by labelling the value as character and adding a column to state what
## the value is - integer or factor.


number_sets <- c("ingest_indicators.agri_forestry_fishing_api", "ingest_indicators.animal_health_public_sector",
                 "ingest_indicators.arable_land_api", "ingest_indicators.cfe_allocations", 
                 "ingest_indicators.combined_data_sheet_api", "ingest_indicators.eid_risk",
                 "ingest_indicators.electricity_access_api",  "ingest_indicators.iucn_mammals", 
                 "ingest_indicators.fao_import_export_api","ingest_indicators.fao_livestock_api", 
                 "ingest_indicators.fao_protein_api", 
                 "ingest_indicators.fisheries_production_api","ingest_indicators.forest_area_api",
                 "ingest_indicators.gross_national_income_api", "ingest_indicators.health_expenditure_api",
                 "ingest_indicators.land_area_api", 
                 "ingest_indicators.malaria_cases_api", #watch closely - keeps breaking
                 "ingest_indicators.medical_doctors_api", "ingest_indicators.population_api", 
                 #"ingest_indicators.promed", # removed for now - until we figure out terms of use
                 "ingest_indicators.rabies_deaths_api",
                 "ingest_indicators.spar", "ingest_indicators.terrestrial_protected_area_api",
                 "ingest_indicators.treecover_loss", "ingest_indicators.vet_capacity", 
                 "ingest_indicators.wash_hygiene_download", "ingest_indicators.wash_sanitation_download",
                 "ingest_indicators.wash_water_download", "ingest_indicators.yellow_fever_api")

factor_sets <- c("ingest_indicators.amr", "ingest_indicators.jee", 
                 "ingest_indicators.rabies_management", 
                 # "ingest_indicators.taenia_solium_api", removed for now - keeps breaking
                 "ingest_indicators.oie_simulation")

function_names_number <- function_names[which(function_names %in% number_sets)]
function_names_factor <- function_names[which(function_names %in% factor_sets)]

## error/warnings checking
# quiet_source <- quietly(source)
# 
# api_run <- purrr::map(list.files(here::here("R_api/"), full.names = TRUE), function(x){
#   quiet_source(x)
# })
# 
# api_run %>% 
#   purrr::keep(~!rlang::is_empty(.x$"warnings"))

purrr::walk(list.files(here::here("R_api/"), full.names = TRUE), source)


# run all the functions and output a list of dataframes - one for each function
tictoc::tic()
outlist <- list()
for(i in 1:length(function_names_number)) {
  outlist[[i]] <- do.call(function_names_number[i], args = list())
}  
tictoc::toc()

full_data_number_api <- bind_rows(outlist)
# check which indicators are included
sort(levels(full_data_number_api$indicator)) # this used to work, but now it's saying NULL


## Repeat for the dataframes that are factors as the value

# run all the functions and output a list of dataframes - one for each function
outlist_factor <- list()
for(i in 1:length(function_names_factor)) {
  outlist_factor[[i]] <- do.call(function_names_factor[i], args = list())
}  ## Is there a quicker/better way to do this rather than a loop? I couldn't work out a vectorised format.

full_data_factor_api <- bind_rows(outlist_factor)

levels(full_data_factor_api$indicator)


### To combine the data sets we can have the value levels as a character and then an extra column to say whether 
### the value is factor or numeric

full_data_factor_com_api <- full_data_factor_api %>%
  mutate(value = as.character(value)) %>%
  mutate(type = "factor")

full_data_number_com_api <- full_data_number_api %>%
  mutate(value = as.character(value)) %>%
  mutate(type = "integer")

full_data_combined_api = rbind(full_data_factor_com_api, full_data_number_com_api)

write.csv(full_data_combined_api, "Output/full_data_combined_api.csv", row.names = F)
write.csv(full_data_number_api, "Output/full_data_number_api.csv", row.names = F)
write.csv(full_data_factor_api, "Output/full_data_factor_api.csv", row.names = F)
