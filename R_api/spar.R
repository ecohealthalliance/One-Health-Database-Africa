ingest_indicators.spar <- function(){
  read_xlsx("data/SPAR.xlsx", skip = 13) %>%
    select(`State Party of IHR`, C.3.1, C.4.1, C.5.1, C.5.3, C.6.1, C.6.2, C.11.1, C.11.2) %>%
    mutate_if(is.character, as.factor) %>%
    mutate(`State Party of IHR` =  stringi::stri_trans_general(str = `State Party of IHR`, id = "Latin-ASCII")) %>%
    mutate(`State Party of IHR` = fct_recode(`State Party of IHR`, 
                                             #"Cote d'Ivoire" = "Côte d'Ivoire",
                                             "Tanzania" = "United Republic of Tanzania",
                                             "Egypt, Arab Rep." = "Egypt")) %>%
    filter(`State Party of IHR` %in% country_names) %>%
    droplevels() %>%
    rename(country = `State Party of IHR`) %>%
    pivot_longer(cols = !country, names_to = "indicator", values_to = "value") %>%
    mutate(indicator = fct_recode(indicator, 
                                  "C.3.1 Collaborative effort on activities to address zoonoses" = "C.3.1",
                                  "C.4.1 Multisectoral collaboration mechanism for food safety events" = "C.4.1",
                                  "C.5.1 Specimen referral and transport system" = "C.5.1", 
                                  "C.5.3 Access to laboratory testing capacity for priority diseases" = "C.5.3",
                                  "C.6.1 Early warning function: indicator- and event-based surveillance" = "C.6.1",
                                  "C.6.2 Mechanism for event management (verification, risk assessment, analysis investigation)" = "C.6.2",
                                  "C.11.1 Core capacity requirements at all times for designated airports, ports and ground crossings" = "C.11.1",
                                  "C.11.2 Effective public health response at points of entry" = "C.11.2")) %>% 
    mutate(units = NA) %>%
    mutate(year = as.factor(NA)) %>%
    mutate(indicator = as.factor(indicator)) %>%
    relocate(country, indicator, year, value, units) %>%
    set_names(colnames_list)
}

#ingest_indicators.spar()
