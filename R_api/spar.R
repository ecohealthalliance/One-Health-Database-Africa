ingest_indicators.spar <- function(){
  read_xlsx("data/SPAR.xlsx", skip = 13) %>%
    select(`State Party of IHR`, C.3.1, C.4.1, C.5.1, C.5.3, C.6.1, C.6.2, C.11.1, C.11.2) %>%
    mutate_if(is.character, as.factor) %>%
    mutate(`State Party of IHR` =  stringi::stri_trans_general(str = `State Party of IHR`, id = "Latin-ASCII")) %>%
    mutate(`State Party of IHR` = fct_recode(`State Party of IHR`, 
                                             #"Cote d'Ivoire" = "Côte d'Ivoire",
                                             "Tanzania" = "United Republic of Tanzania"  )) %>%
    filter(`State Party of IHR` %in% country_names) %>%
    droplevels() %>%
    rename(country = `State Party of IHR`) %>%
    pivot_longer(cols = !country, names_to = "indicator", values_to = "value") %>%
    mutate(units = NA) %>%
    mutate(year = as.factor(NA)) %>%
    mutate(indicator = as.factor(indicator)) %>%
    relocate(country, indicator, year, value, units) %>%
    set_names(colnames_list)
}

#ingest_indicators.spar()
