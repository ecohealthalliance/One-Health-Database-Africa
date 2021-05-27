ingest_indicators.malaria_cases <- function(){
  read_xlsx("data/Estimated Malaria Cases.xlsx", skip = 2) %>%
    mutate(Location =  stringi::stri_trans_general(str = Location, id = "Latin-ASCII")) %>%
    mutate_if(is.character, as.factor) %>%
    mutate(Location = fct_recode(Location, 
                                 #"Cote d'Ivoire" = "Côte d’Ivoire",
                                 "Tanzania" = "United Republic of Tanzania"  )) %>%
    filter(Location %in% country_names) %>%
    droplevels() %>%
    rename_all(~str_replace_all(., "\\s+", "")) %>%  # remove the whitespace from the column names to make easier to use
    mutate(FirstTooltip = sub(" .*", "", FirstTooltip)) %>%  # some of the numbers had a CI after them in brackets. remove the string after the first space so remove the brackets
    mutate(FirstTooltip = sub(",", "" , FirstTooltip))  %>%  # and now remove the comma
    mutate(FirstTooltip = sub(",", "" , FirstTooltip))  %>%  # and any columns that are left
    mutate(FirstTooltip = as.numeric(FirstTooltip)) %>% 
    mutate(units = "number of cases") %>%
    set_names(colnames_list)
}

