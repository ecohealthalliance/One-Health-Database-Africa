
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
                                "Eswatini" = "Swaziland",
                                "Egypt, Arab Rep." = "Egypt")) %>%  
    filter(country %in% country_names) %>%
    droplevels() %>%
    pivot_longer(cols = !country, names_to = "year", values_to = "value") %>%
    mutate(value = extract_numeric(value)) %>%
    mutate(year = as.factor(year)) %>%
    mutate(value = as.numeric(value)) %>%
    mutate(units = "number") %>%
    mutate(indicator = "vet_capacity") %>%
    mutate(indicator = as.factor(indicator)) %>%
    relocate(country, indicator, year, value, units)
}

#ingest_indicators.vet_capacity()
