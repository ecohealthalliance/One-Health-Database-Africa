ingest_indicators.iucn_mammals <- function(){
 
read_csv("data/mammals.csv") %>%
    mutate(country = fct_recode (country,
                                 "Democratic Republic of the Congo" = "Congo, The Democratic Republic of the",
                                 "Egypt, Arab Rep." = "Egypt",
                                 "Tanzania" = "Tanzania, United Republic of")) %>%
    filter(country %in% country_names) %>%
    pivot_longer(cols =!c(country), names_to = "indicator", values_to = "value") %>%
    mutate(indicator = fct_recode(indicator, 
                                  "mammal species_all"= "mammals", 
                                  "mammal species_extinct_introduced_vagrant_removed" = "mammals_extinct_introduced_vagrant_removed")) %>% 
    mutate(year = NA,
           units = "number")
}
  