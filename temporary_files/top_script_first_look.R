## Script to combine the individual indicators and produce a single csv
## Instructions for this part: 
## Create one top-level script that reads the metadata file, 
## processes all files through their ingest function, 
## binds data sets together, and exports the full CSV.

## Going to start by looking at a subset of indicators that I know should be compatible as are
## all numeric but are from different functions

## Look at malaria, yellow fever and arable land


## Questions
## 1) Do we want the capacity to select just a subset of the indicators?
## 2) Can I add a column to the metadata table with the name of the associated function so can just read in from this? 
## 3) Some data can be taken driectly from the webpages, but others will need to be from the downloaded data sets - do 
## we want this scripts to pull a combination? API calls from those available and direct calls from the data file if not? 
## Or two scripts - one that's a mixture (as described above) and a separate one that is downloaded data only so that could be used offline? 

## load the required packages
rm(list = ls())
library(tidyverse)
library(readxl)
library(stringr)
library(docxtractr)


source("/R/")

list.files("C:/Users/Sarah Hayes/Documents/GitHub/gheri-africom-data/R", full.names = TRUE) %>% walk(source)

sourceEntireFolder <- function(folderName, verbose=FALSE, showWarnings=TRUE) { 
  files <- list.files(folderName, full.names=TRUE)
  
  # Grab only R files
  files <- files[ grepl("\\.[rR]$", files) ]
  
  if (!length(files) && showWarnings)
    warning("No R files in ", folderName)
  
  for (f in files) {
    if (verbose)
      cat("sourcing: ", f, "\n")
    ## TODO:  add caught whether error or not and return that
    try(source(f, local=FALSE, echo=FALSE), silent=!verbose)
  }
  return(invisible(NULL))
}


sourceEntireFolder(R)

getwd()

source(here::here("R"))
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

path = "R"
#Source all of the functions
miceadds::source.all("C:/Users/Sarah Hayes/Documents/GitHub/gheri-africom-data/R") # this retrieves all of the functions in the R directory


getwd()
source("R/medical_doctors.R")

# function_names <- metadat %>%
#   select(function_names) %>%
#   drop_na() %>%
#   pull() %>%
#   unique() %>%
#   paste0("ingest_indicators", sep = ".", .)#, "()")


# run all the functions and output a list of dataframes - one for each function
outlist <- list()

for(i in 1:length(function_names)) {
  outlist[[i]] <- do.call(function_names[i], args = list())
  }  ## Is there a quicker/better way to do this rather than a loop? I couldn't work out a vectorised format.

full_data <- bind_rows(outlist)

levels(full_data$indicator)
i=2
