## In this code I am going to look at trying to source the data with an API. 
## Starting with the WHO data.

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

# for world bank data need to specify the dates we want
dates_to_pull <- "2011:2020"

# install.packages(c("httr", "jsonlite"))

library(httr)
library(jsonlite)


try <- GET("https://apps.who.int/gho/athena/api/GHO/HWF_0002.json")

# try2 <- GET("https://apps.who.int/gho/athena/api/GHO/HWF_0002?format=csv")

# view(try)
# view(try2) # these look the same

data = fromJSON(rawToChar(try$content))
# data2 <-(rawToChar(try2$content))

d2 <- data[["fact"]]
#view(d2)

class(d2) ## looks like i need to extract the info from the Dim column
d3 <- d2[1:10,]

# d3 %>% 
#  mutate(Dim = Dim[[]]$code)# %>%
#  separate(Dim, into = c("p", "y", "r", "t", "s"), convert = TRUE)
#
#dim <- map(d3$Dim, toString)
#  
#dims_list <- d3$Dim

#ab <-
#lapply(dims_list, function(x) x%>% select(code)) %>%
#  separate(into = c("p", "y", "r", "t", "s"), convert = TRUE)

#d4 <- d3  %>% mutate(Dim_new = map(Dim, function(x) x%>% select(code)))# %>%
#  separate(Dim_new, into = c("p", "y", "r", "t", "s"), convert = TRUE)

#d2$Dim[[2]]$code

# one option would be to separate out the dims column, make it into a data set for 

dim_com <- d3$Dim

ade <- dim_com %>% reduce(left_join, by = "category") %>%
  t(.) %>%
  as.data.frame(.) %>%
  janitor::row_to_names(row_number = 1) %>%
  dplyr::select(c(YEAR, COUNTRY))

new_df <- cbind(d3, ade)

#https://ghoapi.azureedge.net/api/Indicator?$filter=contains(IndicatorName,%20%27doctor%27)

###Try and put the different bits together

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

## If did it this way would then need to convert the countrycode to the country name.
## Doesn't seem like the easiest option as I've got it at present

###########################################################################################################
## Look at a different data set to see if just as complex!

codeswho <- GET("https://apps.who.int/gho/athena/api/GHO.json")

cw <- fromJSON(rawToChar(codeswho$content))
# cw  ## can use this to look up the codes. Although need to know the exact title WHO use. e.g. rabies deaths is under "Reported number of human rabies deaths"
## look at cw - dimension - code

cw2 <- cw$dimension$code
class(cw2)
a <- cw2[[1]]
b <- cw2$display

rabs <- GET("https://apps.who.int/gho/athena/api/GHO/NTD_RAB2.json")
rabs_cont <- fromJSON(rawToChar(rabs$content))
rabs_cont <- rabs_cont[["fact"]]

rab_dat <- rabs_cont$Dim

rab_dat2<- rab_dat %>% reduce(left_join, by = "category") %>%
  t(.) %>%
  as.data.frame(.) %>%
  janitor::row_to_names(row_number = 1) %>%
  dplyr::select(c(YEAR, COUNTRY))

rab_df <- cbind(rabs_cont, rab_dat2) %>%
  rename(countrycode = COUNTRY)

## similar issue - would still need to use country codes to convert to country. 

rab_df$country <- countrycode::countrycode(rab_df$countrycode, origin = "iso3c", destination = "country.name")

## Then need to select the neccessary columns and write a function to do the rest. 



##############################################################################

## Going to look and see if can extract the FAO data direct from site

## This is usingan example code from the website http://api.data.fao.org/1.0/docs/data_access.html

## However this doesn't seem to work

fao <- GET("http://data.fao.org/ref/f64b7351-11ce-41f1-8366-77bc060f43b6/logo?authKey=d30aebf0-ab2a-11e1-afa6-0800200c9a66&version=1.0")


fao2 <- GET("http://data.fao.org/ref/f64b7351-11ce-41f1-8366-77bc060f43b6/logo?authKey=d30aebf0-ab2a-11e1-afa6-0800200c9a66&version=1.0.json")

fao_cont <- rawToChar(fao$content)
fao_cont
fao_cont2 <- fromJSON(rawToChar(fao2$content))


# Alternative would be to do a bulk download as mentioned by Emma

temp <- tempfile()
download.file("http://fenixservices.fao.org/faostat/static/bulkdownloads/Trade_LiveAnimals_E_All_Data.zip",temp, mode = "wb")
unzip(temp, "Trade_LiveAnimals_E_All_Data.csv")#)
dd <- read.table("Trade_LiveAnimals_E_All_Data.csv", sep="," , header=T)#skip=2
unlink(temp)

fao_import_export <- dd %>%
  filter(Element %in% c("Import Quantity", "Export Quantity")) %>%
  select(! ends_with("F")) %>%  # remove the duplicate year columns
  rename_all(~stringr::str_replace(.,"^Y","")) %>% # remove the Y in front of the column names so just have the years
  filter(Item %in% c("Buffaloes", "Camelids, other", "Camels", "Catlle", "Chickens", "Sheep", "Goats", "Pigs")) %>%
  select(! c(Element.Code, Item.Code, Area.Code)) %>%
  pivot_longer(cols = "1961":"2019", names_to = "Year", values_to = "value") %>%
  mutate(indicator = paste(Element, Item, sep="_")) %>%
  filter(Year %in% chosen_years) %>%
  mutate(Area = as.factor(Area)) %>%
  mutate(Area = fct_recode(Area, 
                               "Cote d'Ivoire" = "Côte d'Ivoire",
                               "Tanzania" = "United Republic of Tanzania")) %>%
  filter(Area %in% country_names) %>%
  droplevels() %>%
  select(Area, Unit, Year, value, indicator) %>%
  rename(year = Year, country = Area, units = Unit) %>%
  relocate(country, indicator, year, value, units) %>%
  mutate(year = as.factor(year))


##### combine into a function

ingest_indicators.fao_import_export <- function(){
  temp <- tempfile()
  download.file("http://fenixservices.fao.org/faostat/static/bulkdownloads/Trade_LiveAnimals_E_All_Data.zip",temp, mode = "wb")
  unzip(temp, "Trade_LiveAnimals_E_All_Data.csv",  exdir = "data")#)
  dd <- read.table("data/Trade_LiveAnimals_E_All_Data.csv", sep="," , header=T)#skip=2
  unlink(temp)
  fao_import_export <- dd %>%
    filter(Element %in% c("Import Quantity", "Export Quantity")) %>%
    select(! ends_with("F")) %>%  # remove the duplicate year columns
    rename_all(~stringr::str_replace(.,"^Y","")) %>% # remove the Y in front of the column names so just have the years
    filter(Item %in% c("Buffaloes", "Camelids, other", "Camels", "Cattle", "Chickens", "Sheep", "Goats", "Pigs")) %>%
    select(! c(Element.Code, Item.Code, Area.Code)) %>%
    pivot_longer(cols = "1961":"2019", names_to = "Year", values_to = "value") %>%
    mutate(indicator = paste(Element, Item, sep="_")) %>%
    filter(Year %in% chosen_years) %>%
    mutate(Area = as.factor(Area)) %>%
    mutate(Area = fct_recode(Area, 
                             "Cote d'Ivoire" = "Côte d'Ivoire",
                             "Tanzania" = "United Republic of Tanzania")) %>%
    filter(Area %in% country_names) %>%
    droplevels() %>%
    select(Area, Unit, Year, value, indicator) %>%
    rename(year = Year, country = Area, units = Unit) %>%
    relocate(country, indicator, year, value, units) %>%
    mutate(year = as.factor(year))
  fao_import_export
}

ingest_indicators.fao_import_export()


###############################################################################################################
## Now repeat for the other fao indicators that are needed 


ingest_indicators.fao_livestock <- function(){
  temp2 <- tempfile()
  download.file("http://fenixservices.fao.org/faostat/static/bulkdownloads/Production_Livestock_E_All_Data.zip",temp2, mode = "wb")
  unzip(temp2, "Production_Livestock_E_All_Data.csv", exdir = "data")#)
  dd <- read.table("data/Production_Livestock_E_All_Data.csv", sep="," , header=T)#skip=2
  unlink(temp2)
  fao_livestock <- dd %>%
    select(! ends_with("F")) %>%  # remove the duplicate year columns
    rename_all(~stringr::str_replace(.,"^Y","")) %>% # remove the Y in front of the column names so just have the years
    filter(Item %in% c("Buffaloes", "Camelids, other", "Camels", "Cattle", "Chickens", "Sheep", "Goats", "Pigs")) %>%
    select(! c(Element.Code, Item.Code, Area.Code)) %>%
    pivot_longer(cols = "1961":"2019", names_to = "Year", values_to = "value") %>%
    mutate(indicator = paste(Element, Item, sep="_")) %>%
    filter(Year %in% chosen_years) %>%
    mutate(Area = as.factor(Area)) %>%
    mutate(Area =  stringi::stri_trans_general(str = Area, id = "Latin-ASCII")) %>%
    mutate_if(is.character, as.factor) %>%
    mutate(Area = fct_recode(Area, 
                                 #"Cote d'Ivoire" = "Côte d’Ivoire",
                                 "Tanzania" = "United Republic of Tanzania"  )) %>%
    #mutate(Area = fct_recode(Area, 
    #                         "Cote d'Ivoire" = "Côte d'Ivoire",
    #                         "Tanzania" = "United Republic of Tanzania")) %>%
    filter(Area %in% country_names) %>%
    droplevels() %>%
    select(Area, Unit, Year, value, indicator) %>%
    rename(year = Year, country = Area, units = Unit) %>%
    relocate(country, indicator, year, value, units) %>%
    mutate(year = as.factor(year))
  fao_livestock
}

ingest_indicators.fao_livestock()



