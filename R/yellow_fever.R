
ingest_indicators.yellow_fever <- function(){
  read_xlsx("data/Yellow Fever.xlsx", col_names = , skip = 2, 
            col_types = c("guess", "guess", "numeric", "numeric")) %>%
    mutate_if(is.character, as.factor) %>%
    mutate(Location = fct_recode(Location, 
                                 "Cote d'Ivoire" = "Côte d’Ivoire",
                                 "Sudan" = "Sudan (until 2011)",
                                 "Tanzania" = "United Republic of Tanzania"  )) %>%
    dplyr::filter(Location %in% country_names) %>%
    droplevels() %>%
    mutate(units = "number of cases") %>% ## need to check what these units are when we have the source URL. 
    set_names(colnames_list) %>%
    mutate(year = as.factor(year)) %>%
    filter(year %in% chosen_years)
}
