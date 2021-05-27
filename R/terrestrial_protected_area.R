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