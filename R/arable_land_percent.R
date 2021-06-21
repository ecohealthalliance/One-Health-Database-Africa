ingest_indicators.arable_land_percent <- function(){
  read_xls("data/Arable land (percent).xls", skip = 2) %>%
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
