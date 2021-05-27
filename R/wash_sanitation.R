ingest_indicators.wash_sanitation <- function(){
  read_xlsx("data/WASH-data-tables.xlsx", sheet = 3) %>%
    select(name, year, wat_bas_n) %>%
    mutate(name = as.factor(name)) %>%
    mutate(year = as.factor(year)) %>%
    rename(country = name) %>%
    mutate(country = fct_recode(country,
                                "Cote d'Ivoire" = "Côte d'Ivoire",
                                "Tanzania" = "United Republic of Tanzania"  )) %>%
    filter(country %in% country_names) %>%
    droplevels() %>%
    filter(year %in% chosen_years) %>%
    mutate(units = "percent") %>%
    mutate(indicator = "sanitation_national_at_least_basic") %>%
    mutate(indicator = as.factor(indicator)) %>%
    rename(value = wat_bas_n) %>%
    relocate(country, indicator, year, value, units) %>%
    mutate_at("value", round, 1)
}