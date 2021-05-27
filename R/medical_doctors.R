ingest_indicators.medical_doctors <- function(){
  read_csv("data/Medical doctor_WHO Global Health Observatory.csv") %>%
    select(c(Indicator, Location, Period, Value)) %>%
    mutate_if(is.character, as.factor) %>%
    mutate(Location = fct_recode(Location, 
                                 "Cote d'Ivoire" = "Côte d’Ivoire",
                                 "Tanzania" = "United Republic of Tanzania"  )) %>% # South Sudan not in doctors data
    filter(Location %in% country_names) %>%
    droplevels() %>%
    mutate(units = "number") %>%
    relocate(after = c(Location, Indicator, Period, Value, units)) %>% 
    set_names(colnames_list) %>%
    filter(year %in% chosen_years) %>%
    mutate(year = as.factor(year))
  
}