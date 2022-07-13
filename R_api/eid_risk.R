ingest_indicators.eid_risk <- function(){
  read_csv("data/hs2_country.csv") %>%
    mutate(country = recode(country,
                            "Cape Verde" = "Cabo Verde",
                            "COTE D IVOIRE" = "Cote d'Ivoire",
                            "Cen African Rep" = "Central African Republic",
                            "Egypt" = "Egypt, Arab Rep.",
                            "Eq Guinea" = "Equatorial Guinea",
                            "Congo (Dem. Rep.)" = "Democratic Republic of the Congo",
                            "Guinea Bissau" = "Guinea-Bissau",
                            "Mauretania" = "Mauritania",
                            "Moracco" = "Morocco",
                            "Morocco (includes Western Sahara)" = "Western Sahara",
                            "Sao Tome" = "Sao Tome and Principe",
                            "Swaziland" = "Eswatini",
                            "Tunesia" = "Tunisia"
    )) %>%
    filter(country %in% country_names) %>%
    # mutate(source = "EID risk (Hotspot 2)",
    #      variable_name = "EID risk",
    #      value = bsm_weight_pop,
    #      year = NA) %>% 
    select(country, variable_name, year,  value) %>% 
    rename(indicator = variable_name)
}

# ingest_indicators.eid_risk()  
