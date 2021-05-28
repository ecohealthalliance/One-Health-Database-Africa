ingest_indicators.animal_health_public_sector <- function(){
  read_xlsx("data/Vet capacity_OIE.xlsx", sheet = 2) %>%
    set_names("country", "2019", "animal_health_public_sector_18", "2018", "animal_health_public_sector_18",
              "2017", "animal_health_public_sector_18", 
              "2016", "animal_health_public_sector_18", "2015") %>%
    select(country, "2019", "2018", "2017", "2016", "2015") %>%
    mutate(country = as.factor(country)) %>%
    mutate(country = fct_recode(country,
                                "Cote d'Ivoire" = "Cote D'Ivoire",
                                "Democratic Republic of the Congo" = "Congo (Dem. Rep. of the)",
                                "Congo" = "Congo (Rep. of the)",
                                "Eswatini" = "Swaziland")) %>%  ## I think this is correct
    filter(country %in% country_names) %>%
    droplevels() %>%
    pivot_longer(cols = !country, names_to = "year", values_to = "value") %>%
    mutate(value = extract_numeric(value)) %>%
    mutate(year = as.factor(year)) %>%
    mutate(value = as.numeric(value)) %>%
    mutate(units = "number") %>%
    mutate(indicator = "animal_health_public_sector") %>%
    mutate(indicator = as.factor(indicator)) %>%
    relocate(country, indicator, year, value, units)
}
