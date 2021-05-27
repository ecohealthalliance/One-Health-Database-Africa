ingest_indicators.taenia_solium <- function(){
  read_xlsx("data/Taenia solium- Presence of porcine cysticercosis.xlsx", skip = 1) %>%
    mutate_if(is.character, as.factor) %>%
    mutate(Location = fct_recode(Location, 
                                 "Cote d'Ivoire" = "Côte d’Ivoire",
                                 "Tanzania" = "United Republic of Tanzania"  )) %>%
    filter(Location %in% country_names) %>%
    mutate(units = "presence or absence") %>%
    set_names(colnames_list) %>%
    mutate(value = fct_recode(value, 
                              "0" = "Absent", 
                              "1" = "Present",
                              "2" = "Inconsistent information", 
                              "3" = "No data")) %>%   # have recoded the factor as a number as discussed with Catherine. This can be changed if required
    mutate(value = as.numeric(as.character(value))) 
}