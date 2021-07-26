# Looking more closely at any data sets with data that doesn't match between the api and non-api ones

rm(list =ls())
library(tidyverse)

api_data <- read.csv("Output/full_data_combined_api.csv", stringsAsFactors = T)
api_data <- api_data[,-1]
data <- read.csv("Output/full_data_combined.csv", stringsAsFactors = T)
data <- data[,-1]

diff1 <- anti_join(api_data, data, by = NULL) %>%
  droplevels()
levels(diff1$indicator)

diff2 <- anti_join(data, api_data, by = NULL) %>%
  droplevels()
levels(diff2$indicator)

electric_api <- api_data[which(api_data$indicator == "Access to electricity (% of population)"),]
electric <- data[which(data$indicator == "Access to electricity (% of population)"),]
# api has more years and the results are rounded to 2 d.p. whereas in combined data set they are rounded to whole numbers. 

agff_api <- api_data[which(api_data$indicator == "Agriculture, forestry, and fishing, value added (% of GDP)"),]
agff <- data[which(data$indicator == "Agriculture, forestry, and fishing, value added (% of GDP)"),]
# api has 2010-2019 and non-api has 2011-2018. api not rounded, non-api rounded to 2 dp. 

an_prot_api <- api_data[which(api_data$indicator == "AnimalProtein"),]
an_prot <- data[which(data$indicator == "AnimalProtein"),]
# api has more years - 2010-2015 vs 2015 only in non-api

arable_api <- api_data[which(api_data$indicator == "Arable land (hectares)"),]
arable <- data[which(data$indicator == "Arable land (hectares)"),]
## api has 2019 whereas non-api doesn't. However the 2019 are all NA

fish_api <- api_data[which(api_data$indicator == "Capture fisheries production (metric tons)"),]
fish <- data[which(data$indicator == "Capture fisheries production (metric tons)"),]
anti_join(fish, fish_api)
anti_join(fish_api, fish)
table(fish$country)
table(fish_api$country)
# difference in rounding for the Tunisia entries is the only difference

health_api <- api_data[which(api_data$indicator == "Current health expenditure per capita (current US$)"),]
health<- data[which(data$indicator == "Current health expenditure per capita (current US$)"),]
# api has 2010-2019 and no rounding whereas non-api has 2011-2016 and is rounded


# mal_cases_api <- api_data[which(api_data$indicator == "Estimated number of malaria cases"),]
# mal_cases <- data[which(data$indicator == "Estimated number of malaria cases"),]
# anti_join(mal_cases, mal_cases_api)
# anti_join(mal_cases_api, mal_cases)
# # issue with label for units - now resolved.

forest_api <- api_data[which(api_data$indicator == "Forest area (% of land area)"),]
forest_cases <- data[which(data$indicator == "Forest area (% of land area)"),]
# only difference is in rounding

gni_api <- api_data[which(api_data$indicator == "GNI per capita, Atlas method (current US$)"),]
gni_cases <- data[which(data$indicator == "GNI per capita, Atlas method (current US$)"),]
anti_join(gni_api, gni_cases)
anti_join(gni_cases, gni_api)

merge_gni <-full_join(gni_api, gni_cases)
# there are some differences in the values between the two datasets - api and non-api. Not sure why...
# the combined data set values are different from those pulled from the WordlBank website and those on the webpage
# where did the ones in the combined data sheet come from? 

land_api <- api_data[which(api_data$indicator == "Land area (sq. km)"),]
land <- data[which(data$indicator == "Land area (sq. km)"),]
anti_join(land, land_api)
anti_join(land_api, land)
# Data entry missing in non api for Sudan for 2010 - that's the only difference

# docs_api <- api_data[which(api_data$indicator == "Medical doctors (number)"),]
# docs <- data[which(data$indicator == "Medical doctors (number)"),]
# hadn't filtered for year - fixed now.

pop_api <- api_data[which(api_data$indicator == "Population, total"),]
pop <- data[which(data$indicator == "Population, total"),]
anti_join(pop, pop_api)
anti_join(pop_api, pop)
# some slight differences in the values. 

prot_api <- api_data[which(api_data$indicator == "Protein"),]
prot <- data[which(data$indicator == "Protein"),]
# api has multiple years whereas non-api has just 2015.

# rabs_api <- api_data[which(api_data$indicator == "rabies deaths"),]
# rabs <- data[which(data$indicator == "rabies deaths"),]
# anti_join(rabs, rabs_api)
# anti_join(rabs_api, rabs)
# # The values are different between the two data sets
# # Issue with this code that needs resolving - fixed!

pa_api <- api_data[which(api_data$indicator == "Terrestrial protected areas (% of total land area)"),]
pa <- data[which(data$indicator == "Terrestrial protected areas (% of total land area)"),]
anti_join(pa, pa_api)
# Just rounding that is different here

# yf_api <- api_data[which(api_data$indicator == "Yellow fever - number of reported cases"),]
# yf <- data[which(data$indicator == "Yellow fever - number of reported cases"),]
# anti_join(yf, yf_api)
# anti_join(yf_api, yf)
# table(yf_api$country)
# table(yf$country)
# # Cote D'ivoire is missing from non-api - now fixed

