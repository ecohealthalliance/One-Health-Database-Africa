ingest_indicators.combined_data_sheet <- function(){
  combi <- read_xlsx("data/Combined data sheet_African countries.xlsx")
  combi <- combi[-1,] # remove the first row as it is empty.
  
  selected_cols <- c("CountryName", "AnimalReport", "HumanReport", "AnimalandHuman", "Vets2018", "Vets2017", "Vets2016",
                     "Vets2015", "Vets2014", "Vets2013", "Vets2012", "Vets2011", "Electric2011",  "Electric2012",
                     "Electric2013", "Electric2014", "Electric2015" , "Electric2016", "Electric2017",
                     #"AnimalProtein201113", "TotalProtein201113", "PercentAnProtein20112013",
                     "Stability2011", "Stability2012", "Stability2013",
                     "Stability2014", "Stability2015", "GNI2011", "GNI2012", "GNI2013", "GNI2014", "GNI2015",
                     "GNI2016", "GNI2017", "GNI2018", "RVFany",
                     #"SheepGoats2011", "SheepGoats2012", "SheepGoats2013", "SheepGoats2014", 
                     #"SheepGoats2015", "SheepGoats2016", "SheepGoats2017",
                     #"Cattle2011", "Cattle2012", "Cattle2013", "Cattle2014", 
                     #"Cattle2015",  "Cattle2016", "Cattle2017",
                     "HealthSpend2011", "HealthSpend2012", "HealthSpend2013", "HealthSpend2014",
                     "HealthSpend2015", "HealthSpend2016", "AgGDP2011", "AgGDP2012", "AgGDP2013", "AgGDP2014",
                     "AgGDP2015", "AgGDP2016", "AgGDP2017", "AgGDP2018", "Malaria2017", "Malaria2016","Malaria2015",
                     "Malaria2014", "Malaria2013", "Malaria2012", "Malaria2011", "Publicationspercountry")
  
  df1 <- select(combi, all_of(selected_cols)) %>%
    mutate(CountryName = as.factor(CountryName)) %>%
    mutate(CountryName = fct_recode(CountryName, 
                                    "Gambia" = "Gambia, The",
                                    "Congo" = "Congo, Rep.",
                                    "Democratic Republic of the Congo" = "Congo, Dem. Rep.")) %>%
    filter(CountryName %in% country_names) %>%
    droplevels() %>%
    mutate_if(is.character, as.numeric) %>%
    rename(RVFAnimalReport = AnimalReport, RVFHumanReport = HumanReport, RVFAnimalandHuman = AnimalandHuman) %>% #, 
         #  PercentAnimalProtein20112013 = PercentAnProtein20112013 ) %>%
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
                                  "Malaria incidence per 1000 population at risk" = "Malaria",
                                  "GNI per capita, Atlas method (current US$)" = "GNI", 
                                  "Agriculture, forestry, and fishing, value added (% of GDP)" = "AgGDP",
                                  "Access to electricity (% of population)" = "Electric",
                                  "Current health expenditure per capita (current US$)" = "HealthSpend"
                                  )) %>% 
    # rename indicators so the two top scripts produce matching results
    mutate(units = case_when(indicator == "Electric" ~ "percent_population_with_access_to_electricity",
                             indicator == "Vets" ~ "Number",
                            # indicator == "AnimalProtein" ~ "gr/caput/day",
                            # indicator == "TotalProtein" ~ "gr/caput/day",
                            # indicator == "PercentAnimalProtein" ~ "percent",
                             indicator == "Stability" ~ "Aggregate indicator in  units of a normal  standard distribution (~ - 2.5-2.5) - Political Stability and  Absence of Violence/Terrorism",
                             indicator == "GNI" ~ "USD per capita",
                             #indicator == "SheepGoats" ~ "Livestock Units",
                             #indicator == "Cattle" ~ "Livestock Units",
                             indicator == "HealthSpend" ~ "USD per capita",
                             indicator == "AgGDP"  ~ "percent of GDP",
                             indicator == "Malaria incidence per 1000 population at_risk" ~ "Number",
                             indicator == "Publicationspercountry" ~ "Number"))
  df1
}
