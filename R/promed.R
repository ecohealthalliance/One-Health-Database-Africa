ingest_indicators.promed <- function(){
  read_xlsx("data/PROMED_AL_databaseFRSPPO.xlsx") %>%
    mutate_if(is.character, as.factor) %>%
    mutate(COUNTRY = fct_recode(COUNTRY, 
                                "Cote d'Ivoire" = "Côte d'Ivoire",
                                "Congo" = "Republic of Congo",
                                "Democratic Republic of the Congo" = "DRCongo")) %>%
    filter(COUNTRY %in% country_names) %>%
    droplevels() %>%
    rename(value = "N events 2015-2021") %>%
    select(COUNTRY, value) %>%
    mutate(indicator = as.factor("promed events 2015-2021")) %>%
    mutate(units = "number") %>%
    mutate(year = as.factor("2015-2021")) %>%
    relocate(COUNTRY, indicator, year, value, units) %>%
    set_names(colnames_list)
}
