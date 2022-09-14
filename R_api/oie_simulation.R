ingest_indicators.oie_simulation <- function(){
  read_xlsx("data/OIE_simulation_exercises.xlsx")  %>%
    mutate_if(is.character, as.factor) %>%
    mutate(country = fct_recode(country, 
                                #"Gambia" = "Gambia, The",
                                #"Congo" = "Congo, Rep.",
                                #"Democratic Republic of the Congo" = "Congo, Dem. Rep.", 
                                "Eswatini" = "Swaziland")) %>%
    filter(country %in% country_names) %>% 
    droplevels() %>%
    rename(year = "Year") %>%
    pivot_longer(cols =!c(country, year), names_to = "indicator", values_to = "value") %>%
    mutate(units = NA) %>%
    droplevels() %>%
    mutate(indicator = as.factor(indicator)) %>%
    mutate(indicator = "OIE Simulation Exercise") %>% 
    mutate(year = as.factor(year)) %>% 
    relocate(country, indicator, year, value, units) %>%
    set_names(colnames_list)
}