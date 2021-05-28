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
