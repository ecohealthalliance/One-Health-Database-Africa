ingest_indicators.fao_protein <- function(){
  
  fao_protein <- read.csv("data/FAO_protein_supply.csv") %>%
    select(c(Area, Item, Area, Year, Unit, Value)) %>%
    mutate(year = as.factor(stringr::str_extract(Year, "\\d{4}"))) %>%
    select(-Year) %>%
    mutate(Item = as.factor(Item)) %>%
    mutate(Item = fct_recode(Item, Protein = "Average protein supply (g/cap/day) (3-year average)",
                             AnimalProtein = "Average supply of protein of animal origin (g/cap/day) (3-year average)")) %>%
    pivot_wider(names_from = Item, values_from = Value) %>%
    mutate(percent_animal_prot = (AnimalProtein/Protein)*100) %>%
    mutate(percent_animal_prot = round(percent_animal_prot, 1)) %>%
    pivot_longer(cols = c("Protein", "AnimalProtein", "percent_animal_prot"), names_to = "indicator", values_to = "value") %>%
    filter(year %in% chosen_years) %>%
    mutate(Area =  stringi::stri_trans_general(str = Area, id = "Latin-ASCII")) %>%
    mutate_if(is.character, as.factor) %>%
    mutate(Area = fct_recode(Area, 
                            "Cote d'Ivoire" = "CA´te d'Ivoire",
                           "Tanzania" = "United Republic of Tanzania"  )) %>%
    filter(Area %in% country_names) %>%
    droplevels() %>%
    mutate(Unit = as.character(Unit)) %>%
    rename(country = Area, units = Unit) %>%
    relocate(country, indicator, year, value, units) %>%
    mutate(indicator = fct_recode(indicator, "Percent animal protein" = "percent_animal_prot"))
  fao_protein[which(fao_protein$indicator == "Percent animal protein"),"units"] <- "percent"
  fao_protein
}

# ingest_indicators.fao_protein()
