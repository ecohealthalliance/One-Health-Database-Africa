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