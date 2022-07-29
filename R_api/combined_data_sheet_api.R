ingest_indicators.combined_data_sheet_api <- function(){
  combi <- read_xlsx("data/Combined data sheet_African countries.xlsx")
  combi <- combi[-1,] # remove the first row as it is empty.
  
  selected_cols <- c("CountryName", 
                     # "AnimalReport", "HumanReport", "AnimalandHuman", 
                     "Vets2018", "Vets2017", "Vets2016",
                     "Vets2015", "Vets2014", "Vets2013", "Vets2012", "Vets2011", 
                     # "AnimalProtein201113", "TotalProtein201113", "PercentAnProtein20112013",
                     "Stability2011", "Stability2012", "Stability2013",
                     "Stability2014", "Stability2015",  
                     #"RVFany",
                     #"SheepGoats2011", "SheepGoats2012", "SheepGoats2013", "SheepGoats2014", 
                     #"SheepGoats2015", "SheepGoats2016", "SheepGoats2017",
                     #"Cattle2011", "Cattle2012", "Cattle2013", "Cattle2014",
                     #"Cattle2015",  "Cattle2016", "Cattle2017",  
                     #"Publicationspercountry", 
                     "Malaria2017", "Malaria2016","Malaria2015",
                     "Malaria2014", "Malaria2013", "Malaria2012", "Malaria2011")
  
  df1 <- select(combi, all_of(selected_cols)) %>%
    mutate(CountryName = as.factor(CountryName)) %>%
    mutate(CountryName = fct_recode(CountryName, 
                                    "Gambia" = "Gambia, The",
                                    "Congo" = "Congo, Rep.",
                                    "Democratic Republic of the Congo" = "Congo, Dem. Rep.")) %>%
    filter(CountryName %in% country_names) %>%
    droplevels() %>%
    mutate_if(is.character, as.numeric) %>%
    #rename(RVFAnimalReport = AnimalReport, RVFHumanReport = HumanReport, RVFAnimalandHuman = AnimalandHuman) %>% #, 
           #PercentAnimalProtein20112013 = PercentAnProtein20112013 ) %>%
    pivot_longer(cols = !CountryName, names_to = "indicator", values_to = "value") %>%
    mutate_at("value", round, 1) %>%
    mutate(year = extract_numeric(indicator)) %>%
    mutate(indicator = tm::removeNumbers(indicator)) %>%
    mutate(indicator = as.factor(indicator)) %>%
    mutate(units =  NA) %>%
    mutate(year = as.factor(year)) %>%
    rename(country = CountryName) %>%
    relocate(country, indicator, year, value, units) %>%
    mutate(indicator = fct_recode(indicator, 
                                  "Malaria incidence per 1000 population at risk" = "Malaria")) %>% 
    mutate(units = case_when(indicator == "Vets" ~ "Number",
                             #indicator == "AnimalProtein" ~ "gr/caput/day",
                             #indicator == "TotalProtein" ~ "gr/caput/day",
                             #indicator == "PercentAnimalProtein" ~ "percent",
                             indicator == "Stability" ~ "Aggregate indicator in  units of a normal  standard distribution (~ - 2.5-2.5) - Political Stability and  Absence of Violence/Terrorism",
                             #indicator == "SheepGoats" ~ "Livestock Units",
                             #indicator == "Cattle" ~ "Livestock Units",
                             indicator == "Malaria incidence per 1000 population at_risk" ~ "Number",
                             indicator == "Publicationspercountry" ~ "Number"))
  df1
}

#ingest_indicators.combined_data_sheet_api()
