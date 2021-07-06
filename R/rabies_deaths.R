ingest_indicators.rabies_deaths <- function(){
  read.csv("data/Rabies Deaths_WHO.csv", skip = 1, check.names = F) %>%
    mutate_if(is.character, as.factor) %>%
    mutate(Country =  stringi::stri_trans_general(str = Country, id = "Latin-ASCII")) %>%
    mutate(Country = fct_recode(Country, 
                                #"Cote d'Ivoire" = "CA´te d'Ivoire",
                                "Tanzania" = "United Republic of Tanzania" )) %>%
    filter(Country %in% country_names) %>%
    droplevels() %>%
    pivot_longer(cols = !Country, names_to = "year", values_to = "value") %>%
    mutate(value = as.character(value)) %>%
    mutate(value = na_if(value, "No data")) %>%  # change any values that say 'no data' to NA
    mutate(value = as.numeric(value)) %>%
    mutate(units = "number") %>%
    mutate(indicator = "rabies deaths") %>%
    mutate(indicator = as.factor(indicator)) %>%
    relocate(after = c(Country, indicator, year, value, units)) %>%  ## rearrange the columns to the order that we want
    set_names(colnames_list) %>%
    mutate(year = as.factor(year))
}

# ingest_indicators.rabies_deaths()
