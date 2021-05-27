ingest_indicators.wash_hygiene <- function(){
  read_xlsx("data/WASH-data-tables.xlsx", sheet = 4) %>%
    select(SANITATION...1, Year...2, NATIONAL...5) %>%
    slice(-c(1:2))  %>%
    set_names("country", "year", "hygiene_national_at_least_basic") %>%
    mutate_if(is.character, as.factor) %>%
    mutate(country = fct_recode(country,
                                "Cote d'Ivoire" = "Côte d'Ivoire",
                                "Tanzania" = "United Republic of Tanzania"  )) %>%
    filter(country %in% country_names) %>%
    droplevels()  %>%
    mutate(year = as.factor(year)) %>%
    mutate(hygiene_national_at_least_basic =  as.character(hygiene_national_at_least_basic)) %>%
    mutate(hygiene_national_at_least_basic = sub(">", "", hygiene_national_at_least_basic)) %>%
    mutate(hygiene_national_at_least_basic = sub("-", "", hygiene_national_at_least_basic)) %>%
    mutate_if(is.character, as.numeric) %>%
    mutate_at("hygiene_national_at_least_basic", round, 1) %>%
    rename(value = "hygiene_national_at_least_basic") %>%
    mutate(indicator = "hygiene_national_at_least_basic") %>%
    mutate(indicator = as.factor(indicator)) %>%
    mutate(units = "percent") %>%
    relocate(country, indicator, year, value, units) %>%
    filter(year %in% chosen_years)
}