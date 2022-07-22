ingest_indicators.fao_protein_api <- function(){
  temp2 <- tempfile()
  download.file("http://fenixservices.fao.org/faostat/static/bulkdownloads/Food_Security_Data_E_All_Data.zip",temp2, mode = "wb")
  unzip(temp2, "Food_Security_Data_E_All_Data.csv", exdir = "data")#)
  dd <- read.table("data/Food_Security_Data_E_All_Data.csv", sep="," , header=T)#skip=2
  unlink(temp2)
  
  
  fao_protein <- dd %>%
    filter(Item %in% c("Average protein supply (g/cap/day) (3-year average)", 
                          "Average supply of protein of animal origin (g/cap/day) (3-year average)"))#  %>%
 
   names(fao_protein) = gsub(pattern = "Y*", replacement = "", x = names(fao_protein))
    
  fp2 <- fao_protein %>%
    mutate_all(funs(na_if(., ""))) %>% # make empty columns NA and then remove
    select_if(~!all(is.na(.))) %>%
    select(!ends_with("F")) %>%
    select(!c(contains("Code"), Element)) %>%
    pivot_longer(cols = !c(Area, Item, Unit), names_to = "year", values_to = "value") %>%
    mutate(year = as.factor(stringr::str_extract(year, "\\d{4}"))) %>%
    #select(-Year) %>%
    mutate(Item = as.factor(Item)) %>%
    mutate(value = as.numeric(value)) %>%
    mutate(Item = fct_recode(Item, Protein = "Average protein supply (g/cap/day) (3-year average)",
                             AnimalProtein = "Average supply of protein of animal origin (g/cap/day) (3-year average)")) %>%
    pivot_wider(names_from = Item, values_from = value) %>%
    mutate(percent_animal_prot = (AnimalProtein/Protein)*100) %>%
    mutate(percent_animal_prot = round(percent_animal_prot, 1)) %>%
    pivot_longer(cols = c("Protein", "AnimalProtein", "percent_animal_prot"), names_to = "indicator", values_to = "value") %>%
    filter(year %in% chosen_years) %>%
    mutate(Area =  stringi::stri_trans_general(str = Area, id = "Latin-ASCII")) %>%
    mutate_if(is.character, as.factor) %>%
    mutate(Area = fct_recode(Area, 
                             #"Cote d'Ivoire" = "CA´te d'Ivoire",
                             "Tanzania" = "United Republic of Tanzania",
                             "Egypt, Arab Rep." = "Egypt")) %>%
    filter(Area %in% country_names) %>%
    droplevels() %>%
    mutate(Unit = as.character(Unit)) %>%
    rename(country = Area, units = Unit) %>%
    relocate(country, indicator, year, value, units) %>%
    mutate(indicator = fct_recode(indicator, "Percent animal protein" = "percent_animal_prot"))
  fp2[which(fp2$indicator == "Percent animal protein"), "units"] <- "percent"
  fp2
}

# ingest_indicators.fao_protein_api()
