# Create one function per input file, of the form ingest_indicators.indicator_name(filename). 
# Each should ingest the file containing the relevant indicator and output a tidy data frame. 
# Save each function in a separate file in an R/ directory
# Each function should yield a single tidy R data frame with columns: country, indicator, year, value, units
# Use the readxl and, if needed, the tidyxl packages for ingest.  
# The docxtractr package is a likely good option for extracting data from the docx files.

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

# Some of these may not be in the correct/matching format in the dataframes that we are feeding in so need
# to check this for each dataset

#####################################################################################################
## Yellow Fever

yf <- read_xlsx("data/Yellow Fever.xlsx", col_names = , skip = 2, 
                col_types = c("guess", "guess", "numeric", "numeric"))


yf2 <- yf %>%
  mutate_if(is.character, as.factor) %>%
  mutate(Location = fct_recode(Location, 
                        "Cote d'Ivoire" = "Côte d’Ivoire",
                        "Sudan" = "Sudan (until 2011)",
                        "Tanzania" = "United Republic of Tanzania"  )) %>%
  dplyr::filter(Location %in% country_names) %>%
  droplevels() %>%
  mutate(units = "number of cases") %>% ## need to check what these units are when we have the source URL. 
  set_names(colnames_list) %>%
  mutate(year = as.factor(year)) %>%
  filter(year %in% chosen_years)

levels(yf2$country) # we have all 53 countries here

# The YF dataframe seems relatively straightforward. Thins to confirm:
# 1) 
# 2) check units
# 3) 
# 4) alter file name if needed
# 5) check if this is one we can use directly from URL
# 6) 

ingest_indicators.yellow_fever <- function(){
  read_xlsx("data/Yellow Fever.xlsx", col_names = , skip = 2, 
            col_types = c("guess", "guess", "numeric", "numeric")) %>%
    mutate_if(is.character, as.factor) %>%
    mutate(Location = fct_recode(Location, 
                                 "Cote d'Ivoire" = "Côte d’Ivoire",
                                 "Sudan" = "Sudan (until 2011)",
                                 "Tanzania" = "United Republic of Tanzania"  )) %>%
    dplyr::filter(Location %in% country_names) %>%
    droplevels() %>%
    mutate(units = "number of cases") %>% ## need to check what these units are when we have the source URL. 
    set_names(colnames_list) %>%
    mutate(year = as.factor(year)) %>%
    filter(year %in% chosen_years)
}

ingest_indicators.yellow_fever()


######################################################################################################
# Now lets look at arable land

arable <- read_xls("data/Arable land.xls", skip = 2)# %>%
  

a2 <- arable %>%
  rename_all(~str_replace_all(., "\\s+", "")) %>%
  mutate_if(is.character, as.factor) %>%
  mutate(CountryName = fct_recode(CountryName, 
                               "Gambia" = "Gambia, The",
                               "Congo" = "Congo, Rep.",
                               "Democratic Republic of the Congo" = "Congo, Dem. Rep.")) %>%
  filter(CountryName %in% country_names) %>%
  droplevels() %>%
  select(- c(CountryCode, IndicatorCode)) %>%   # possibly remove the 1960 column as no data in it
  pivot_longer(cols = !c(CountryName, IndicatorName), names_to = "year", values_to = "value") %>%
  mutate(units = "percent") %>%
  set_names(colnames_list)

levels(a2$country)   
unique(arable$`Country Name`)

ingest_indicators.arable_land <- function(){
    read_xls("data/Arable land.xls", skip = 2) %>%
    rename_all(~str_replace_all(., "\\s+", "")) %>%
    mutate_if(is.character, as.factor) %>%  
    mutate(CountryName = fct_recode(CountryName, 
                                    "Gambia" = "Gambia, The",
                                    "Congo" = "Congo, Rep.",
                                    "Democratic Republic of the Congo" = "Congo, Dem. Rep.")) %>%
    filter(CountryName %in% country_names) %>%
    droplevels() %>%
    select(- c(CountryCode, IndicatorCode)) %>%   # possibly remove the 1960 column as no data in it
    pivot_longer(cols = !c(CountryName, IndicatorName), names_to = "year", values_to = "value") %>%
    mutate(units = "percent") %>%
    filter(year %in% chosen_years) %>%
    mutate(year = as.factor(year)) %>%
    set_names(colnames_list)
}

ingest_indicators.arable_land()

# Things to check/confirm
# 1) do we want to alter the indicator name so it doesn't include the "% of land area" info.
# 2) should year be character, number or factor?  Going for factor for now.... 


#######################################################################################################
## Malaria cases

malaria <- read_xlsx("data/Estimated Malaria Cases.xlsx", skip = 2)
  
m2 <- malaria %>%
  rename_all(~str_replace_all(., "\\s+", "")) %>%  # remove the whitespace from the column names to make easier to use
  mutate(Location =  stringi::stri_trans_general(str = Location, id = "Latin-ASCII")) %>%
  mutate_if(is.character, as.factor) %>%
  mutate(Location = fct_recode(Location, 
                               #"Cote d'Ivoire" = "Côte d’Ivoire",
                                "Tanzania" = "United Republic of Tanzania"  )) %>%
  filter(Location %in% country_names) %>%
  droplevels() %>%
  mutate(FirstTooltip = sub(" .*", "", FirstTooltip)) %>%  # some of the numbers had a CI after them in brackets. remove the string after the first space so remove the brackets
  mutate(FirstTooltip = sub(",", "" , FirstTooltip))  %>%  # and now remove the comma
  mutate(FirstTooltip = sub(",", "" , FirstTooltip))  %>%  # and any columns that are left
  mutate(FirstTooltip = as.numeric(FirstTooltip)) %>% 
  mutate(units = "number of cases") %>%
  set_names(colnames_list)
  

ingest_indicators.malaria_cases <- function(){
  read_xlsx("data/Estimated Malaria Cases.xlsx", skip = 2) %>%
    mutate(Location =  stringi::stri_trans_general(str = Location, id = "Latin-ASCII")) %>%
    mutate_if(is.character, as.factor) %>%
    mutate(Location = fct_recode(Location, 
                                 #"Cote d'Ivoire" = "Côte d’Ivoire",
                                 "Tanzania" = "United Republic of Tanzania"  )) %>%
    filter(Location %in% country_names) %>%
    droplevels() %>%
    rename_all(~str_replace_all(., "\\s+", "")) %>%  # remove the whitespace from the column names to make easier to use
    mutate(FirstTooltip = sub(" .*", "", FirstTooltip)) %>%  # some of the numbers had a CI after them in brackets. remove the string after the first space so remove the brackets
    mutate(FirstTooltip = sub(",", "" , FirstTooltip))  %>%  # and now remove the comma
    mutate(FirstTooltip = sub(",", "" , FirstTooltip))  %>%  # and any columns that are left
    mutate(FirstTooltip = as.numeric(FirstTooltip)) %>% 
    mutate(units = "number of cases") %>%
    set_names(colnames_list)
}

ingest_indicators.malaria_cases()


## To check
## 1) year as factor, charactor or number - going for factor at present as makes it easier when use mutate_if in some functions
## 2) OK for this to be the name of the indicator? 


################################################################################################
## Taenia solium

ts <- read_xlsx("data/Taenia solium- Presence of porcine cysticercosis.xlsx", skip = 1)

ts2 <- ts %>% 
  mutate_if(is.character, as.factor) %>%
  mutate(Location = fct_recode(Location, 
                               "Cote d'Ivoire" = "Côte d’Ivoire",
                               "Tanzania" = "United Republic of Tanzania"  )) %>%
  filter(Location %in% country_names) %>%
  droplevels() %>%
  mutate(units = "presence or absence") %>%
  set_names(colnames_list)

levels(ts2$country)

ingest_indicators.taenia_solium <- function(){
  read_xlsx("data/Taenia solium- Presence of porcine cysticercosis.xlsx", skip = 1) %>%
    mutate_if(is.character, as.factor) %>%
    mutate(Location = fct_recode(Location, 
                                 "Cote d'Ivoire" = "Côte d’Ivoire",
                                 "Tanzania" = "United Republic of Tanzania"  )) %>%
    filter(Location %in% country_names) %>%
    mutate(units = "presence or absence") %>%
    set_names(colnames_list) %>%
    mutate(value = fct_recode(value, 
                              "0" = "Absent", 
                              "1" = "Present",
                              "2" = "Inconsistent information", 
                              "3" = "No data")) %>%   # have recoded the factor as a number as discussed with Catherine. This can be changed if required
    mutate(value = as.numeric(as.character(value))) 
}


ingest_indicators.taenia_solium()

## To do
## 1) year as character, factor or number.  - factor at present
## 2)  
## 3) what to do with value as it is a character/factor in this dataframe rather than a number.


############################################################################################################
## Fisheries 

fish <- read_csv("data/Capture fisheries production (metric tons).csv", skip = 4)

colnames(fish)

f2 <- fish %>%
  rename_all(~str_replace_all(., "\\s+", "")) %>%
  mutate_if(is.character, as.factor) %>%
  mutate(CountryName = fct_recode(CountryName, 
                                  "Gambia" = "Gambia, The",
                                  "Congo" = "Congo, Rep.",
                                  "Democratic Republic of the Congo" = "Congo, Dem. Rep.")) %>%
  filter(CountryName %in% country_names) %>%
  droplevels() %>%
  select(-c(CountryCode, IndicatorCode)) %>%
  pivot_longer(cols = !c(CountryName, IndicatorName), names_to = "year", values_to = "value") %>%
  mutate(units = "metric tons") %>%
  set_names(colnames_list)
   
#levels(f2$CountryName)  

ingest_indicators.fisheries_production <- function(){
  read_csv("data/Capture fisheries production (metric tons).csv", skip = 4) %>%
    rename_all(~str_replace_all(., "\\s+", "")) %>%
    mutate_if(is.character, as.factor) %>%
    mutate(CountryName = fct_recode(CountryName, 
                                    "Gambia" = "Gambia, The",
                                    "Congo" = "Congo, Rep.",
                                    "Democratic Republic of the Congo" = "Congo, Dem. Rep.")) %>%
    filter(CountryName %in% country_names) %>%
    droplevels() %>%
    select(-c(CountryCode, IndicatorCode)) %>%
    pivot_longer(cols = !c(CountryName, IndicatorName), names_to = "year", values_to = "value") %>%
    mutate(units = "metric tons") %>%
    set_names(colnames_list) %>%
    filter(year %in% chosen_years) %>%
    mutate(year = as.factor(year))
}

ingest_indicators.fisheries_production()

## To do
## 1) 
## 2) do we want metric tons included in indicator name
## 3) year as factor, character or number


###################################################################################################
## Forest cover and loss

forest <- read_xlsx("data/Forest cover and loss.xlsx", col_names = F)

# create new column names as a composite of the first two rows
new_names <- paste0(as.character(forest[1,]), as.character(forest[2,]))

names(forest) <- new_names          # assign the new column names
forest <- forest[3:nrow(forest), ]    # subset off the first two rows

## Will wait and find out which of these are wanted before proceeding. 




####################################################################################################
## Rabies deaths

rabies <- read.csv("data/Rabies Deaths_WHO.csv", skip = 1, check.names = F) # check.names allows us to import the 
# numeric column names without it adding an X.

r2 <- rabies %>%
  mutate_if(is.character, as.factor) %>%
  mutate(Country = fct_recode(Country, 
                               "Cote d'Ivoire" = "CÃ´te d'Ivoire",
                               "Tanzania" = "United Republic of Tanzania"  )) %>%
  filter(Country %in% country_names) %>%
  droplevels() %>%
  pivot_longer(cols = !Country, names_to = "year", values_to = "value") %>%
  mutate(value = na_if(value, "No data")) %>%  # change any values that say 'no data' to NA
  mutate(value = as.numeric(value)) %>%
  mutate(units = "number of deaths") %>%
  mutate(indicator = "rabies deaths") %>%
  relocate(after = c(Country, indicator, year, value, units)) %>%  ## rearrange the columns to the order that we want
  set_names(colnames_list) %>%
  mutate(year = as.factor(year))


ingest_indicators.rabies_deaths <- function(){
  read.csv("data/Rabies Deaths_WHO.csv", skip = 1, check.names = F) %>%
    mutate_if(is.character, as.factor) %>%
    mutate(Country = fct_recode(Country, 
                                "Cote d'Ivoire" = "CÃ´te d'Ivoire",
                                "Tanzania" = "United Republic of Tanzania"  )) %>%
    filter(Country %in% country_names) %>%
    droplevels() %>%
    pivot_longer(cols = !Country, names_to = "year", values_to = "value") %>%
    mutate(value = na_if(value, "No data")) %>%  # change any values that say 'no data' to NA
    mutate(value = as.numeric(value)) %>%
    mutate(units = "number of deaths") %>%
    mutate(indicator = "rabies deaths") %>%
    relocate(after = c(Country, indicator, year, value, units)) %>%  ## rearrange the columns to the order that we want
    set_names(colnames_list) %>%
    mutate(year = as.factor(year))
}

ingest_indicators.rabies_deaths()


## To do
## 1) check if year is factor, character or number



#########################################################################################################
## Terrestrial protected areas 

tpa <- read_csv("data/Terrestrial protected areas (% of total land area).csv", skip = 4)


tpa2 <- tpa %>%
  rename_all(~str_replace_all(., "\\s+", "")) %>%
  mutate_if(is.character, as_factor) %>% 
  mutate(CountryName = fct_recode(CountryName, 
                                  "Gambia" = "Gambia, The",
                                  "Congo" = "Congo, Rep.",
                                  "Democratic Republic of the Congo" = "Congo, Dem. Rep.")) %>%
  filter(CountryName %in% country_names) %>%
  droplevels() %>%
  select(-c(CountryCode, IndicatorCode)) %>%
  pivot_longer(cols = !c(CountryName, IndicatorName), names_to = "year", values_to = "value") %>%
  mutate(units = "percent") %>%
  set_names(colnames_list) %>%
  filter(year %in% chosen_years) %>%
  mutate(year = as.factor(year))


ingest_indicators.terrestrial_protected_area <- function(){
  read_csv("data/Terrestrial protected areas (% of total land area).csv", skip = 4) %>%
    rename_all(~str_replace_all(., "\\s+", "")) %>%
    mutate_if(is.character, as_factor) %>% 
    mutate(CountryName = fct_recode(CountryName, 
                                    "Gambia" = "Gambia, The",
                                    "Congo" = "Congo, Rep.",
                                    "Democratic Republic of the Congo" = "Congo, Dem. Rep.")) %>%
    filter(CountryName %in% country_names) %>%
    droplevels() %>%
    select(-c(CountryCode, IndicatorCode)) %>%
    pivot_longer(cols = !c(CountryName, IndicatorName), names_to = "year", values_to = "value") %>%
    mutate(units = "percent") %>%
    set_names(colnames_list) %>%
    filter(year %in% chosen_years) %>%
    mutate(year = as.factor(year))
}

ingest_indicators.terrestrial_protected_area()


## To do
## 1) year as factor, character or number
## 2) 


##############################################################################################################
## World Bank Population

pop <- read_csv("data/Population_World Bank.csv", skip = 3)

p2 <- pop %>%
  rename_all(~str_replace_all(., "\\s+", "")) %>%
  mutate_if(is.character, as.factor) %>%
  mutate(CountryName = fct_recode(CountryName, 
                                  "Gambia" = "Gambia, The",
                                  "Congo" = "Congo, Rep.",
                                  "Democratic Republic of the Congo" = "Congo, Dem. Rep.")) %>%
  filter(CountryName %in% country_names) %>%
  droplevels() %>%
  select(-c(CountryCode, IndicatorCode)) %>%
  pivot_longer(cols = !c(CountryName, IndicatorName), names_to = "year", values_to = "value") %>%
  mutate(units = "number") %>%
  set_names(colnames_list) %>%
  filter(year %in% chosen_years) %>%
  mutate(year = as.factor(year))


ingest_indicators.population <- function(){
    read_csv("data/Population_World Bank.csv", skip = 3) %>%
    rename_all(~str_replace_all(., "\\s+", "")) %>%
    mutate_if(is.character, as.factor) %>%
    mutate(CountryName = fct_recode(CountryName, 
                                    "Gambia" = "Gambia, The",
                                    "Congo" = "Congo, Rep.",
                                    "Democratic Republic of the Congo" = "Congo, Dem. Rep.")) %>%
    filter(CountryName %in% country_names) %>%
    droplevels() %>%
    select(-c(CountryCode, IndicatorCode)) %>%
    pivot_longer(cols = !c(CountryName, IndicatorName), names_to = "year", values_to = "value") %>%
    mutate(units = "number") %>%
    set_names(colnames_list) %>%
    filter(year %in% chosen_years) %>%
    mutate(year = as.factor(year))
  }

ingest_indicators.population()


## To do:
## 1) Year as factor, character or number


###############################################################################################
## Medical doctors

md <- read_csv("data/Medical doctor_WHO Global Health Observatory.csv")

colnames(md)

md2 <- md %>%
  select(c(Indicator, Location, Period, Value)) %>%
  mutate_if(is.character, as.factor) %>%
  mutate(Location = fct_recode(Location, 
                              "Cote d'Ivoire" = "Côte d’Ivoire",
                              "Tanzania" = "United Republic of Tanzania"  )) %>% # South Sudan not in doctors data
  filter(Location %in% country_names) %>%
  droplevels() %>%
  mutate(units = "number") %>%
  relocate(after = c(Location, Indicator, Period, Value, units)) %>% 
  set_names(colnames_list) %>%
  filter(year %in% chosen_years) %>%
  mutate(year = as.factor(year))

ingest_indicators.medical_doctors <- function(){
  read_csv("data/Medical doctor_WHO Global Health Observatory.csv") %>%
    select(c(Indicator, Location, Period, Value)) %>%
    mutate_if(is.character, as.factor) %>%
    mutate(Location = fct_recode(Location, 
                                 "Cote d'Ivoire" = "Côte d’Ivoire",
                                 "Tanzania" = "United Republic of Tanzania"  )) %>% # South Sudan not in doctors data
    filter(Location %in% country_names) %>%
    droplevels() %>%
    mutate(units = "number") %>%
    relocate(after = c(Location, Indicator, Period, Value, units)) %>% 
    set_names(colnames_list) %>%
    filter(year %in% chosen_years) %>%
    mutate(year = as.factor(year))
  
}

ingest_indicators.medical_doctors()


################################################################################################
### AMR indcators

amr <- read_xls("data/AMR self assessment survey responses 2019-2020 (Excel format).xls")

amr_indicators <- c("Country name", "4.1 Multi-sector and One Health collaboration/coordination", 
                    "4.2 Which sectors are actively involved in developing and implementing the AMR National Action Plan- [Food Production]",
                    "4.2 Which sectors are actively involved in developing and implementing the AMR National Action Plan- [Environment]",
                    "5.1 Country progress with development of a national action plan on AMR", 
                    "5.2 Is your country’s national action plan on AMR linked to any other existing action plans, strategies or targets related to - [Malaria]",
                    "5.3 If you have published your AMR national action plan, please insert a link here.",
                    "5.4 Country legislations on antimicrobial use [Country has laws or regulations that prohibits the use of antibiotics for growth promotion in the absence of risk analysis.]",
                    "5.4 Country legislations on antimicrobial use [Country has legislation on marketing of pesticides including antimicrobial pesticides, such as bactericides and fungicides used in plant production.]",
                    "6.1 Raising awareness and understanding of AMR risks and response", 
                    "6.1.1 For the level selected above, please indicate the extent of involvement of the sectors below. [Animal Health (terrestrial and aquatic)]",
                    "6.3 Training and professional education on AMR in the veterinary sector",
                    "6.5 Progress with strengthening veterinary services", 
                    "7.2 National monitoring system for antimicrobials intended to be used in animals (terrestrial and aquatic) (sales/use)",
                    "7.5 (b) AMR surveillance is routinely undertaken in animals for the following categories: [Animal (terrestrial and/or aquatic) isolates linked to animal disease.]",
                    "7.5 (b) AMR surveillance is routinely undertaken in animals for the following categories: [Zoonotic pathogenic bacteria]", 
                    "8.2 Good health, management and hygiene practices to reduce the use of antimicrobials and minimize development and transmission of AMR in animal production (terrestrial and aquatic)",
                    "9.2 Optimizing antimicrobial use in animal health (terrestrial and aquatic)",
                    "10. National assessment of risks for AMR transmission in the environment and pollution control. Legislation and/or regulations to prevent contamination of the environment with antimicrobials [Discharges from intensive animal (terrestrial and aquatic) production (liquid waste and manure) a) disposal into the environment][Are risk reduction actions underway-]")



a2 <- amr %>% select(all_of(amr_indicators)) %>%  
  #rename_all(~str_replace_all(., "\\s+", "")) %>%
  rename(Countryname = 'Country name') %>%
  mutate_if(is.character, as.factor) %>%
  mutate(Countryname = fct_recode(Countryname, 
                               "Cote d'Ivoire" = "Côte d'Ivoire",
                               "Tanzania" = "United Republic of Tanzania"  )) %>% # South Sudan not in doctors data
  filter(Countryname %in% country_names) %>%
  droplevels() %>%
  pivot_longer(cols = !(Countryname)) %>%
  mutate(year = "unknown") %>%
  mutate(units = "none") %>%
  relocate(after = c(Countryname, name, year, value, units)) %>%
  set_names(colnames_list) %>%
  mutate(year = as.factor(year)) %>%
  mutate(indicator = as.factor(indicator))


ingest_indicators.amr <- function(){
  amr_indicators <- c("Country name", "4.1 Multi-sector and One Health collaboration/coordination", 
                      "4.2 Which sectors are actively involved in developing and implementing the AMR National Action Plan- [Food Production]",
                      "4.2 Which sectors are actively involved in developing and implementing the AMR National Action Plan- [Environment]",
                      "5.1 Country progress with development of a national action plan on AMR", 
                      "5.2 Is your country’s national action plan on AMR linked to any other existing action plans, strategies or targets related to - [Malaria]",
                      "5.3 If you have published your AMR national action plan, please insert a link here.",
                      "5.4 Country legislations on antimicrobial use [Country has laws or regulations that prohibits the use of antibiotics for growth promotion in the absence of risk analysis.]",
                      "5.4 Country legislations on antimicrobial use [Country has legislation on marketing of pesticides including antimicrobial pesticides, such as bactericides and fungicides used in plant production.]",
                      "6.1 Raising awareness and understanding of AMR risks and response", 
                      "6.1.1 For the level selected above, please indicate the extent of involvement of the sectors below. [Animal Health (terrestrial and aquatic)]",
                      "6.3 Training and professional education on AMR in the veterinary sector",
                      "6.5 Progress with strengthening veterinary services", 
                      "7.2 National monitoring system for antimicrobials intended to be used in animals (terrestrial and aquatic) (sales/use)",
                      "7.5 (b) AMR surveillance is routinely undertaken in animals for the following categories: [Animal (terrestrial and/or aquatic) isolates linked to animal disease.]",
                      "7.5 (b) AMR surveillance is routinely undertaken in animals for the following categories: [Zoonotic pathogenic bacteria]", 
                      "8.2 Good health, management and hygiene practices to reduce the use of antimicrobials and minimize development and transmission of AMR in animal production (terrestrial and aquatic)",
                      "9.2 Optimizing antimicrobial use in animal health (terrestrial and aquatic)",
                      "10. National assessment of risks for AMR transmission in the environment and pollution control. Legislation and/or regulations to prevent contamination of the environment with antimicrobials [Discharges from intensive animal (terrestrial and aquatic) production (liquid waste and manure) a) disposal into the environment][Are risk reduction actions underway-]")
  read_xls("data/AMR self assessment survey responses 2019-2020 (Excel format).xls") %>%
    select(all_of(amr_indicators)) %>%  
    #rename_all(~str_replace_all(., "\\s+", "")) %>%
    rename(Countryname = 'Country name') %>%
    mutate_if(is.character, as.factor) %>%
    mutate(Countryname = fct_recode(Countryname, 
                                    "Cote d'Ivoire" = "Côte d'Ivoire",
                                    "Tanzania" = "United Republic of Tanzania"  )) %>% # South Sudan not in doctors data
    filter(Countryname %in% country_names) %>%
    droplevels() %>%
    pivot_longer(cols = !(Countryname)) %>%
    mutate(year = "unknown") %>%
    mutate(units = "none") %>%
    relocate(after = c(Countryname, name, year, value, units)) %>%
    set_names(colnames_list) %>%
    mutate(year = as.factor(year)) %>%
    mutate(indicator = as.factor(indicator))
}

ingest_indicators.amr()


### To do 
## 1) Should we include the WHO Region? 
## 2) How do we want to deal with the indicator names - they are very long! 
## 3) Value -leave as a factor with all the info? Change to a number and have a chart saying what each number means? 
## 4) What year? 
## 5) Units? 


###########################################################################################
## 

#install.packages("docxtractr")
library(docxtractr)

cfe <- read_docx("data/CFE allocations-2015-20jan2021.docx") 
class(cfe)   ## check that it is a docx file
cfe2 <- docx_extract_all_tbls(cfe) # a list of the tables as dataframes

#fourlists <- cfe2[c(2:5)]

# If I extract all the lists in one and combine I get issues at the first list doesn't match the other 4 as it splits the money
# column into the number and a dollar sign in the last 4 but not the first one.

# So first extract list 1 and modify
onelist <- cfe2[[1]]

new_names <- as.character(onelist[2,])
names(onelist) <- new_names      # assign the new column names
onelist <- onelist[3:nrow(onelist), ]    # subset off the first two rows so now have the correct column names
onelist$Amount <- sub('.', '', onelist$Amount)  # remove the first character which is a $ sign
onelist$Amount <- str_replace_all(onelist$Amount, ",", "") # and then remove the commas

## Next the other 4 data frames
fourlist <- plyr::ldply(cfe2[c(2:5)], rbind)%>%
  select(-V5) 
fourlist$V6 <-   str_replace_all(fourlist$V6, ",", "")
colnames(fourlist) <- colnames(onelist)

cfe_all <- rbind(onelist, fourlist)
colnames(cfe_all)
#cfe_all[,"Type"] <- paste0("cfe_", cfe_all[, "Type"]) # make a new name for the indicator that has cfe but then says what for
cfe_all <- cfe_all %>%
  mutate(year = str_replace(Date, "^.+-", "")) %>%
  mutate(year = paste0("20", year )) %>%  # convert to full years
  filter(year %in% chosen_years) %>%
  mutate(year = as.factor(year)) %>%
  mutate(Country = as.factor(Country)) %>%
  mutate(Country = fct_recode(Country, 
                                  "Sao Tome and Principe" = "São Tomé and Príncipe",
                                  "Congo" = "Republic of Congo",
                                  "Democratic Republic of the Congo" = "Democratic Republic of Congo")) %>%
  filter(Country %in% country_names) %>%
  droplevels() %>%
  select(Country, Amount, year) %>%
  mutate(Amount = as.numeric(Amount)) %>%
  group_by(Country, year) %>%
  summarise(value = sum(Amount)) %>%
  #mutate(indicator = paste0("cfe_", Type)) %>%
  #select(Country, Amount, year, indicator) %>%
  mutate(indicator =  as.factor("cfe allocation")) %>%
  mutate(units = "USD") %>%
  relocate(Country, indicator, year, value, units)%>%
  set_names(colnames_list)


ingest_indicators.cfe_allocations <- function(){
  cfe <- read_docx("data/CFE allocations-2015-20jan2021.docx") 
  cfe2 <- docx_extract_all_tbls(cfe) # a list of the tables as dataframes
  onelist <- cfe2[[1]]
  new_names <- as.character(onelist[2,])
  names(onelist) <- new_names      # assign the new column names
  onelist <- onelist[3:nrow(onelist), ]    # subset off the first two rows so now have the correct column names
  onelist$Amount <- sub('.', '', onelist$Amount)  # remove the first character which is a $ sign
  onelist$Amount <- str_replace_all(onelist$Amount, ",", "") # and then remove the commas
  fourlist <- plyr::ldply(cfe2[c(2:5)], rbind)%>%
    select(-V5) 
  fourlist$V6 <-   str_replace_all(fourlist$V6, ",", "")
  colnames(fourlist) <- colnames(onelist)
  cfe_all <- rbind(onelist, fourlist)
  #colnames(cfe_all)
  cfe_all <- cfe_all %>%
    mutate(year = str_replace(Date, "^.+-", "")) %>%
    mutate(year = paste0("20", year )) %>%  # convert to full years
    filter(year %in% chosen_years) %>%
    mutate(year = as.factor(year)) %>%
    mutate(Country = as.factor(Country)) %>%
    mutate(Country = fct_recode(Country, 
                                "Sao Tome and Principe" = "São Tomé and Príncipe",
                                "Congo" = "Republic of Congo",
                                "Democratic Republic of the Congo" = "Democratic Republic of Congo")) %>%
    filter(Country %in% country_names) %>%
    droplevels() %>%
    select(Country, Amount, year) %>%
    mutate(Amount = as.numeric(Amount)) %>%
    group_by(Country, year) %>%
    summarise(value = sum(Amount)) %>%
    #mutate(indicator = paste0("cfe_", Type)) %>%
    #select(Country, Amount, year, indicator) %>%
    mutate(indicator =  as.factor("cfe allocation")) %>%
    mutate(units = "USD") %>%
    relocate(Country, indicator, year, value, units)%>%
    set_names(colnames_list)
  cfe_all
}

ingest_indicators.cfe_allocations()

### To do:
## 1) 
## 2) check that the unit is USD (and not some other dollars)
## 3) 


##################################################################################################
##### combined dataset

combi <- read_xlsx("data/Combined data sheet_African countries.xlsx")
combi <- combi[-1,] # remove the first row as it is empty.
# Now going to split the dataframe into 2 dataframes because need to make some tweaks to some of the columns. 

## ** might actually be easier to just select the columns that we want as there are a few that are outside of the eyars that we
## are looking to select. Will come back to this one. 

df1 <- select(combi, !c(Element, ))

c2 <- combi %>% 
  slice(-1) %>%  
  pivot_longer(cols = AnimalReport:Publicationspercountry, names_to = "indicator", values_to = "value")
str(combi)

combi$Elements




###############################################################################################
### JEE Indicators

jee <- read_xlsx("data/JEE scores.xlsx")

colnames(jee)

jee_indicators <- c("Country", "JEE Year", "P.4.3 Mechanisms for responding to infectious and potential zoonotic diseases are established and functional (v1) / P.4.2 Mechanisms for responding to infectious and potential zoonotic diseases established and functional (v2)",
                    "P.6.1 Whole-of-government biosafety and biosecurity system is in place for human, animal and agriculture facilities",   
                    "P.6.2 Biosafety and biosecurity training and practices",
                    "P.7.2 National vaccine access and delivery",
                    "D.1.1 Laboratory testing for detection of priority diseases",
                    "D.1.2 Specimen referral and transport system",
                    "D.1.4 Laboratory quality system",
                    "D.3.1 System for e cient reporting to FAO, OIE and WHO",
                    "D.3.2 Reporting network and protocols in country",
                    "D.4.2 FETP1 or other applied epidemiology training programme in place (v1)/ D.4.4 FETP or other applied epidemiology training programme is in place (v2)",
                    "D.4.3 Workforce strategy (v1)/ D.4.1 An up-to-date multisectoral workforce strategy is in place (v2)",
                    "R.3.1 Public health and security authorities (e.g. law enforcement, border control, customs) are linked during a suspect or con rmed biological event (v1)/ R.3.1 Public health and security authorities (e.g. law enforcement, border control, customs) linked during a suspect or confirmed biological, chemical or radiological event",
                    "R.5.2 Internal and partner communication and coordination (v1)/ R.5.2 Internal and partner coordination for emergency risk communication (v2)",
                    "PoE.1 Routine capacities established at points of entry",
                    "PoE.2 E ective public health response at points of entry")

j2 <- jee %>%
  select(all_of(jee_indicators)) %>%
  mutate_if(is.character, as.factor) %>%
  mutate(Country = fct_recode(Country, 
                                  "Gambia" = "Gambia, The",
                                  "Congo" = "Congo, Rep.",
                                  "Democratic Republic of the Congo" = "Congo, Dem. Rep.")) %>%
  filter(Country %in% country_names) %>%
  droplevels() %>%
  rename(year = "JEE Year") %>%
  pivot_longer(cols =!c(Country, year), names_to = "indicator", values_to = "value") %>%
  mutate(units = NA) %>%
  mutate(year = fct_recode(year, "2016" = "2016 - some with 2 assessments Country vs external experts")) %>%
  droplevels() %>%
  mutate(indicator = as.factor(indicator)) %>%
  relocate(Country, indicator, year, value, units) %>%
  set_names(colnames_list)


ingest_indicators.jee <- function(){
   jee_indicators <- c("Country", "JEE Year", "P.4.3 Mechanisms for responding to infectious and potential zoonotic diseases are established and functional (v1) / P.4.2 Mechanisms for responding to infectious and potential zoonotic diseases established and functional (v2)",
                      "P.6.1 Whole-of-government biosafety and biosecurity system is in place for human, animal and agriculture facilities",   
                      "P.6.2 Biosafety and biosecurity training and practices",
                      "P.7.2 National vaccine access and delivery",
                      "D.1.1 Laboratory testing for detection of priority diseases",
                      "D.1.2 Specimen referral and transport system",
                      "D.1.4 Laboratory quality system",
                      "D.3.1 System for e cient reporting to FAO, OIE and WHO",
                      "D.3.2 Reporting network and protocols in country",
                      "D.4.2 FETP1 or other applied epidemiology training programme in place (v1)/ D.4.4 FETP or other applied epidemiology training programme is in place (v2)",
                      "D.4.3 Workforce strategy (v1)/ D.4.1 An up-to-date multisectoral workforce strategy is in place (v2)",
                      "R.3.1 Public health and security authorities (e.g. law enforcement, border control, customs) are linked during a suspect or con rmed biological event (v1)/ R.3.1 Public health and security authorities (e.g. law enforcement, border control, customs) linked during a suspect or confirmed biological, chemical or radiological event",
                      "R.5.2 Internal and partner communication and coordination (v1)/ R.5.2 Internal and partner coordination for emergency risk communication (v2)",
                      "PoE.1 Routine capacities established at points of entry",
                      "PoE.2 E ective public health response at points of entry")
  read_xlsx("data/JEE scores.xlsx")  %>%
    select(all_of(jee_indicators)) %>%
    mutate_if(is.character, as.factor) %>%
    mutate(Country = fct_recode(Country, 
                                "Gambia" = "Gambia, The",
                                "Congo" = "Congo, Rep.",
                                "Democratic Republic of the Congo" = "Congo, Dem. Rep.")) %>%
    filter(Country %in% country_names) %>%
    droplevels() %>%
    rename(year = "JEE Year") %>%
    pivot_longer(cols =!c(Country, year), names_to = "indicator", values_to = "value") %>%
    mutate(units = NA) %>%
    mutate(year = fct_recode(year, "2016" = "2016 - some with 2 assessments Country vs external experts")) %>%
    droplevels() %>%
    mutate(indicator = as.factor(indicator)) %>%
    relocate(Country, indicator, year, value, units) %>%
    set_names(colnames_list)
}

ingest_indicators.jee()

# do we want to hae the value as a factor as it currently is? Or have it as a number with a key? 

##########################################################################################################
### Promed mentions

pro <- read_xlsx("data/PROMED_AL_databaseFRSPPO.xlsx")

pro2 <- pro %>%
  mutate_if(is.character, as.factor) %>%
  mutate(COUNTRY = fct_recode(COUNTRY, 
                              "Cote d'Ivoire" = "Côte d'Ivoire",
                              "Congo" = "Republic of Congo",
                              "Democratic Republic of the Congo" = "DRCongo")) %>%
   filter(COUNTRY %in% country_names) %>%
  droplevels() %>%
  rename(value = "N events 2015-2021") %>%
  select(COUNTRY, value) %>%
  mutate(indicator = "promed events 2015-2021") %>%
  mutate(units = "number") %>%
  mutate(year = "2015-2021") %>%
  relocate(COUNTRY, indicator, year, value, units) %>%
  set_names(colnames_list)


ingest_indicators.promed <- function(){
   read_xlsx("data/PROMED_AL_databaseFRSPPO.xlsx") %>%
    mutate_if(is.character, as.factor) %>%
    mutate(COUNTRY = fct_recode(COUNTRY, 
                                "Cote d'Ivoire" = "Côte d'Ivoire",
                                "Congo" = "Republic of Congo",
                                "Democratic Republic of the Congo" = "DRCongo")) %>%
    filter(COUNTRY %in% country_names) %>%
    droplevels() %>%
    rename(value = "N events 2015-2021") %>%
    select(COUNTRY, value) %>%
    mutate(indicator = as.factor("promed events 2015-2021")) %>%
    mutate(units = "number") %>%
    mutate(year = as.factor("2015-2021")) %>%
    relocate(COUNTRY, indicator, year, value, units) %>%
    set_names(colnames_list)
}

ingest_indicators.promed()



########################################################################################################
### Rabies management

rab_man <- read_xlsx("data/Rabies Management_CDC.xlsx")

rm2 <- rab_man %>%
  mutate_if(is.character, as.factor) %>%
  mutate(Country = fct_recode(Country, 
                              "Gambia" = "Gambia, The",
                              "Congo" = "Congo, Rep.",
                              "Democratic Republic of the Congo" = "Congo, Dem. Rep.")) %>%
  filter(Country %in% country_names) %>%
  droplevels() %>%
  select(-c(CountryCode, `Lyssavirus free`, `Rabies virus free`, `Canine (dog) rabies free`)) %>%
  pivot_longer(cols = !Country, names_to = "indicator", values_to = "value") %>%
  mutate(units = NA) %>%
  mutate(year = NA) %>%
  mutate(indicator = as.factor(indicator)) %>%
  relocate(Country, indicator, year, value, units) %>%
  set_names(colnames_list)



ingest_indicators.rabies_management <- function(){
  read_xlsx("data/Rabies Management_CDC.xlsx") %>%
    mutate_if(is.character, as.factor) %>%
    mutate(Country = fct_recode(Country, 
                                "Gambia" = "Gambia, The",
                                "Congo" = "Congo, Rep.",
                                "Democratic Republic of the Congo" = "Congo, Dem. Rep.")) %>%
    filter(Country %in% country_names) %>%
    droplevels() %>%
    select(-c(CountryCode, `Lyssavirus free`, `Rabies virus free`, `Canine (dog) rabies free`)) %>%
    pivot_longer(cols = !Country, names_to = "indicator", values_to = "value") %>%
    mutate(units = NA) %>%
    mutate(year = as.factor(NA)) %>%
    mutate(indicator = as.factor(indicator)) %>%
    relocate(Country, indicator, year, value, units) %>%
    set_names(colnames_list)
}

ingest_indicators.rabies_management()

### To do
## 1) check what we are doing with year and units and the fact that the outcome is a factor.


#####################################################################################################
### SPAR 

spar <- read_xlsx("data/SPAR.xlsx", skip = 13)
colnames(spar)

s2 <- spar %>%
  select(`State Party of IHR`, C.3.1, C.4.1, C.5.1, C.6.1, C.6.2, C.11.1, C.11.2) %>%
  mutate_if(is.character, as.factor) %>%
  mutate(`State Party of IHR` = fct_recode(`State Party of IHR`, 
                                           "Cote d'Ivoire" = "Côte d'Ivoire",
                                           "Tanzania" = "United Republic of Tanzania"  )) %>%
  filter(`State Party of IHR` %in% country_names) %>%
  droplevels() %>%
  rename(country = `State Party of IHR`) %>%
  pivot_longer(cols = !country, names_to = "indicator", values_to = "value") %>%
  mutate(units = NA) %>%
  mutate(year = as.factor(NA)) %>%
  mutate(indicator = as.factor(indicator)) %>%
  relocate(country, indicator, year, value, units) %>%
  set_names(colnames_list)


ingest_indicators.spar <- function(){
  read_xlsx("data/SPAR.xlsx", skip = 13) %>%
    select(`State Party of IHR`, C.3.1, C.4.1, C.5.1, C.6.1, C.6.2, C.11.1, C.11.2) %>%
    mutate_if(is.character, as.factor) %>%
    mutate(`State Party of IHR` = fct_recode(`State Party of IHR`, 
                                             "Cote d'Ivoire" = "Côte d'Ivoire",
                                             "Tanzania" = "United Republic of Tanzania"  )) %>%
    filter(`State Party of IHR` %in% country_names) %>%
    droplevels() %>%
    rename(country = `State Party of IHR`) %>%
    pivot_longer(cols = !country, names_to = "indicator", values_to = "value") %>%
    mutate(units = NA) %>%
    mutate(year = as.factor(NA)) %>%
    mutate(indicator = as.factor(indicator)) %>%
    relocate(country, indicator, year, value, units) %>%
    set_names(colnames_list)
}


ingest_indicators.spar()

##################################################################################################
### WASH

wash_water <- read_xlsx("data/WASH-data-tables.xlsx", sheet = 2)

w2 <- wash_water %>%
  select(`DRINKING WATER...1`, Year...2, NATIONAL...5, RURAL...10) %>% # first select the columns we want
  set_names("country", "year", "water_national_at_least_basic", "water_rural_at_least_basic") %>%
  slice(-c(1:2)) %>%
  mutate_if(is.character, as.factor) %>%
  mutate(country = fct_recode(country,
                              "Cote d'Ivoire" = "Côte d'Ivoire",
                              "Tanzania" = "United Republic of Tanzania"  )) %>%
  filter(country %in% country_names) %>%
  droplevels() %>%
  mutate(year = as.factor(year)) %>%
  mutate(water_national_at_least_basic =  as.character(water_national_at_least_basic)) %>%
  mutate(water_rural_at_least_basic = as.character(water_rural_at_least_basic)) %>%
  mutate(water_rural_at_least_basic = sub(">", "", water_rural_at_least_basic)) %>%
  mutate(water_rural_at_least_basic = sub("-", "", water_rural_at_least_basic)) %>%
  mutate(water_national_at_least_basic = sub(">", "", water_national_at_least_basic)) %>%
  mutate(water_national_at_least_basic = sub("-", "", water_national_at_least_basic)) %>%
  mutate_if(is.character, as.numeric) %>%
  mutate_at(c("water_national_at_least_basic", "water_rural_at_least_basic"), round, 1) %>%
  pivot_longer(cols = !c(country, year), names_to = "indicator", values_to = "value") %>%
  mutate(indicator = as.factor(indicator)) %>%
  mutate(units = "percent") %>%
  relocate(country, indicator, year, value, units) %>%
  filter(year %in% chosen_years)


ingest_indicators.wash_water <- function(){
  read_xlsx("data/WASH-data-tables.xlsx", sheet = 2) %>%
    select(`DRINKING WATER...1`, Year...2, NATIONAL...5, RURAL...10) %>% # first select the columns we want
    set_names("country", "year", "water_national_at_least_basic", "water_rural_at_least_basic") %>%
    slice(-c(1:2)) %>%
    mutate_if(is.character, as.factor) %>%
    mutate(country = fct_recode(country,
                                "Cote d'Ivoire" = "Côte d'Ivoire",
                                "Tanzania" = "United Republic of Tanzania"  )) %>%
    filter(country %in% country_names) %>%
    droplevels() %>%
    mutate(year = as.factor(year)) %>%
    mutate(water_national_at_least_basic =  as.character(water_national_at_least_basic)) %>%
    mutate(water_rural_at_least_basic = as.character(water_rural_at_least_basic)) %>%
    mutate(water_rural_at_least_basic = sub(">", "", water_rural_at_least_basic)) %>%
    mutate(water_rural_at_least_basic = sub("-", "", water_rural_at_least_basic)) %>%
    mutate(water_national_at_least_basic = sub(">", "", water_national_at_least_basic)) %>%
    mutate(water_national_at_least_basic = sub("-", "", water_national_at_least_basic)) %>%
    mutate_if(is.character, as.numeric) %>%
    mutate_at(c("water_national_at_least_basic", "water_rural_at_least_basic"), round, 1) %>%
    pivot_longer(cols = !c(country, year), names_to = "indicator", values_to = "value") %>%
    mutate(indicator = as.factor(indicator)) %>%
    mutate(units = "percent") %>%
    relocate(country, indicator, year, value, units) %>%
    filter(year %in% chosen_years)
}

ingest_indicators.wash_water()


##############################################################################################################
### wash sanitation (national at least basic

wash_san <- read_xlsx("data/WASH-data-tables.xlsx", sheet = 3)

ws2 <- wash_san %>%
  select(name, year, wat_bas_n) %>%
  mutate(name = as.factor(name)) %>%
  mutate(year = as.factor(year)) %>%
  rename(country = name) %>%
  mutate(country = fct_recode(country,
                              "Cote d'Ivoire" = "Côte d'Ivoire",
                              "Tanzania" = "United Republic of Tanzania"  )) %>%
  filter(country %in% country_names) %>%
  droplevels() %>%
  filter(year %in% chosen_years) %>%
  mutate(units = "percent") %>%
  mutate(indicator = "sanitation_national_at_least_basic") %>%
  mutate(indicator = as.factor(indicator)) %>%
  rename(value = wat_bas_n) %>%
  relocate(country, indicator, year, value, units) %>%
  mutate_at("value", round, 1)


ingest_indicators.wash_sanitation <- function(){
  read_xlsx("data/WASH-data-tables.xlsx", sheet = 3) %>%
    select(name, year, wat_bas_n) %>%
    mutate(name = as.factor(name)) %>%
    mutate(year = as.factor(year)) %>%
    rename(country = name) %>%
    mutate(country = fct_recode(country,
                                "Cote d'Ivoire" = "Côte d'Ivoire",
                                "Tanzania" = "United Republic of Tanzania"  )) %>%
    filter(country %in% country_names) %>%
    droplevels() %>%
    filter(year %in% chosen_years) %>%
    mutate(units = "percent") %>%
    mutate(indicator = "sanitation_national_at_least_basic") %>%
    mutate(indicator = as.factor(indicator)) %>%
    rename(value = wat_bas_n) %>%
    relocate(country, indicator, year, value, units) %>%
    mutate_at("value", round, 1)
}


ingest_indicators.wash_sanitation()


#########################################################################################################
### WASH hygiene - national at least basic

wash_hyg <- read_xlsx("data/WASH-data-tables.xlsx", sheet = 4)

wh2 <- wash_hyg %>%
  select(SANITATION...1, Year...2, NATIONAL...5) %>%
  slice(-c(1:2)) %>%
  set_names("country", "year", "hygiene_national_at_least_basic") %>%
  mutate_if(is.character, as.factor) %>%
  mutate(country = fct_recode(country,
                              "Cote d'Ivoire" = "Côte d'Ivoire",
                              "Tanzania" = "United Republic of Tanzania"  )) %>%
  filter(country %in% country_names) %>%
  droplevels()  %>%
  mutate(year = as.factor(year)) %>%
  mutate(hygiene_national_at_least_basic =  as.character(hygiene_national_at_least_basic)) %>%
  mutate(hygiene_national_at_least_basic = sub(">", "", hygiene_national_at_least_basic)) %>%
  mutate(hygiene_national_at_least_basic = sub("-", "", hygiene_national_at_least_basic)) %>%
  mutate_if(is.character, as.numeric) %>%
  mutate_at("hygiene_national_at_least_basic", round, 1) %>%
  rename(value = "hygiene_national_at_least_basic") %>%
  mutate(indicator = "hygiene_national_at_least_basic") %>%
  mutate(indicator = as.factor(indicator)) %>%
  mutate(units = "percent") %>%
  relocate(country, indicator, year, value, units) %>%
  filter(year %in% chosen_years)


ingest_indicators.wash_hygiene <- function(){
  read_xlsx("data/WASH-data-tables.xlsx", sheet = 4) %>%
    select(SANITATION...1, Year...2, NATIONAL...5) %>%
    slice(-c(1:2))  %>%
    set_names("country", "year", "hygiene_national_at_least_basic") %>%
    mutate_if(is.character, as.factor) %>%
    mutate(country = fct_recode(country,
                                "Cote d'Ivoire" = "Côte d'Ivoire",
                                "Tanzania" = "United Republic of Tanzania"  )) %>%
    filter(country %in% country_names) %>%
    droplevels()  %>%
    mutate(year = as.factor(year)) %>%
    mutate(hygiene_national_at_least_basic =  as.character(hygiene_national_at_least_basic)) %>%
    mutate(hygiene_national_at_least_basic = sub(">", "", hygiene_national_at_least_basic)) %>%
    mutate(hygiene_national_at_least_basic = sub("-", "", hygiene_national_at_least_basic)) %>%
    mutate_if(is.character, as.numeric) %>%
    mutate_at("hygiene_national_at_least_basic", round, 1) %>%
    rename(value = "hygiene_national_at_least_basic") %>%
    mutate(indicator = "hygiene_national_at_least_basic") %>%
    mutate(indicator = as.factor(indicator)) %>%
    mutate(units = "percent") %>%
    relocate(country, indicator, year, value, units) %>%
    filter(year %in% chosen_years)
}

ingest_indicators.wash_hygiene()


#########################################################################################################
##  Vet Capacity

vet <- read_xlsx("data/Vet capacity_OIE.xlsx", sheet = 1)

v1 <- vet %>% 
  set_names("country", "2019", "vet_capacity_18", "2018", "vet_capacity_17", "2017", "vet_capacity_16", 
            "2016", "vet_capacity_15", "2015") %>%
  select(country, "2019", "2018", "2017", "2016", "2015") %>%
  mutate(country = as.factor(country)) %>%
  mutate(country = fct_recode(country,
                              "Cote d'Ivoire" = "Cote D'Ivoire",
                              "Democratic Republic of the Congo" = "Congo (Dem. Rep. of the)",
                              "Congo" = "Congo (Rep. of the)",
                              "Eswatini" = "Swaziland")) %>%  ## I think this is correct
  filter(country %in% country_names) %>%
  droplevels() %>%
  pivot_longer(cols = !country, names_to = "year", values_to = "value") %>%
  mutate(value = sub("...", "", value)) %>%
  mutate(year = as.factor(year)) %>%
  mutate(value = as.numeric(value)) %>%
  mutate(units = "number") %>%
  mutate(indicator = "vet_capacity") %>%
  mutate(indicator = as.factor(indicator)) %>%
  relocate(country, indicator, year, value, units)


ingest_indicators.vet_capacity <- function(){
  read_xlsx("data/Vet capacity_OIE.xlsx", sheet = 1) %>% 
    set_names("country", "2019", "vet_capacity_18", "2018", "vet_capacity_17", "2017", "vet_capacity_16", 
              "2016", "vet_capacity_15", "2015") %>%
    select(country, "2019", "2018", "2017", "2016", "2015") %>%
    mutate(country = as.factor(country)) %>%
    mutate(country = fct_recode(country,
                                "Cote d'Ivoire" = "Cote D'Ivoire",
                                "Democratic Republic of the Congo" = "Congo (Dem. Rep. of the)",
                                "Congo" = "Congo (Rep. of the)",
                                "Eswatini" = "Swaziland")) %>%  ## I think this is correct
    filter(country %in% country_names) %>%
    droplevels() %>%
    pivot_longer(cols = !country, names_to = "year", values_to = "value") %>%
    mutate(value = sub("...", "", value)) %>%
    mutate(year = as.factor(year)) %>%
    mutate(value = as.numeric(value)) %>%
    mutate(units = "number") %>%
    mutate(indicator = "vet_capacity") %>%
    mutate(indicator = as.factor(indicator)) %>%
    relocate(country, indicator, year, value, units)
}

ingest_indicators.vet_capacity()


##############################################################################################
## animal health personnel public sector

animal_health <- read_xlsx("data/Vet capacity_OIE.xlsx", sheet = 2)


ah2 <- animal_health %>% 
  set_names("country", "2019", "animal_health_public_sector_18", "2018", "animal_health_public_sector_18",
            "2017", "animal_health_public_sector_18", 
            "2016", "animal_health_public_sector_18", "2015") %>%
  select(country, "2019", "2018", "2017", "2016", "2015") %>%
  mutate(country = as.factor(country)) %>%
  mutate(country = fct_recode(country,
                              "Cote d'Ivoire" = "Cote D'Ivoire",
                              "Democratic Republic of the Congo" = "Congo (Dem. Rep. of the)",
                              "Congo" = "Congo (Rep. of the)",
                              "Eswatini" = "Swaziland")) %>%  ## I think this is correct
  filter(country %in% country_names) %>%
  droplevels() %>%
  pivot_longer(cols = !country, names_to = "year", values_to = "value") %>%
  mutate(value = sub("...", "", value)) %>%
  mutate(year = as.factor(year)) %>%
  mutate(value = as.numeric(value)) %>%
  mutate(units = "number") %>%
  mutate(indicator = "animal_health_public_sector") %>%
  mutate(indicator = as.factor(indicator)) %>%
  relocate(country, indicator, year, value, units)


ingest_indicators.animal_health_public_sector <- function(){
  read_xlsx("data/Vet capacity_OIE.xlsx", sheet = 2) %>%
    set_names("country", "2019", "animal_health_public_sector_18", "2018", "animal_health_public_sector_18",
              "2017", "animal_health_public_sector_18", 
              "2016", "animal_health_public_sector_18", "2015") %>%
    select(country, "2019", "2018", "2017", "2016", "2015") %>%
    mutate(country = as.factor(country)) %>%
    mutate(country = fct_recode(country,
                                "Cote d'Ivoire" = "Cote D'Ivoire",
                                "Democratic Republic of the Congo" = "Congo (Dem. Rep. of the)",
                                "Congo" = "Congo (Rep. of the)",
                                "Eswatini" = "Swaziland")) %>%  ## I think this is correct
    filter(country %in% country_names) %>%
    droplevels() %>%
    pivot_longer(cols = !country, names_to = "year", values_to = "value") %>%
    mutate(value = sub("...", "", value)) %>%
    mutate(year = as.factor(year)) %>%
    mutate(value = as.numeric(value)) %>%
    mutate(units = "number") %>%
    mutate(indicator = "animal_health_public_sector") %>%
    mutate(indicator = as.factor(indicator)) %>%
    relocate(country, indicator, year, value, units)
}

ingest_indicators.animal_health_public_sector()
