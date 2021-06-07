## SourCe doctors data from the WHO.


## Still need the year, country and colnames data
rm(list = ls())

library(tidyverse)
library(readxl)
library(stringr)
library(docxtractr)

colnames_list <- c("country", "indicator", "year", "value", "units")

chosen_years <- c("2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019")

## Africom countries

## these are the names of the countries that we want to include in the analyses

country_names <- read_xlsx("data/AFRICOM List.xlsx") %>%
  filter(CountryName != "Egypt, Arab Rep.") %>%
  mutate_if(is.character, as.factor) %>%
  mutate(CountryName = fct_recode(CountryName, 
                                  "Democratic Republic of the Congo" = "Congo, Dem. Rep." ,
                                  "Congo" = "Congo, Rep.", 
                                  "Gambia" = "Gambia, The")) %>%
  droplevels() %>%
  pull(CountryName)

country_names

# install.packages(c("httr", "jsonlite"))

library(httr)
library(jsonlite)

ingest_indicators.medical_doctors_api <- function(){
try <- GET("https://apps.who.int/gho/athena/api/GHO/HWF_0002.json")
data = fromJSON(rawToChar(try$content))
d2 <- data[["fact"]]
dim_com <- d2$Dim

ade <- dim_com %>% reduce(left_join, by = "category") %>%
  t(.) %>%
  as.data.frame(.) %>%
  janitor::row_to_names(row_number = 1) %>%
  dplyr::select(c(YEAR, COUNTRY))

new_df <- cbind(d2, ade)

## Then need to convert the countrycode to the country name.
## Don't appear to have south Sudan in these data

full_df <- new_df %>%
  rename(ctry_code = COUNTRY) %>%
  rename(year = YEAR) %>%
  select(year, ctry_code, value) %>%
  mutate(country = countrycode::countrycode(ctry_code, origin = "iso3c", destination = "country.name")) %>%
  mutate(country =  stringi::stri_trans_general(str = country, id = "Latin-ASCII")) %>%
  mutate(year = as.factor(year)) %>%
  mutate(country = as.factor(country)) %>%
  mutate(country = fct_recode(country, 
                              "Cabo Verde" = "Cape Verde",
                                  "Congo" = "Congo - Brazzaville",
                                  "Democratic Republic of the Congo" = "Congo - Kinshasa",
                                  "Sao Tome and Principe" = "Sao Tome & Principe")) %>%
  filter(country %in% country_names) %>%
  droplevels()
  
full_df <- do.call(data.frame, full_df)
full_df_2 <- full_df %>%
  select(country, year, value.numeric) %>%
  rename(value = value.numeric) %>%
  mutate(indicator = "medical_doctors") %>%
  mutate(indicator = as.factor(indicator)) %>%
  mutate(units = "number") %>%
  relocate("country", "indicator", "year", "value", "units")

full_df_2
}


ingest_indicators.medical_doctors_api()




