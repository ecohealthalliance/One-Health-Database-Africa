ingest_indicators.fao_livestock <- function(){
  temp2 <- tempfile()
  download.file("http://fenixservices.fao.org/faostat/static/bulkdownloads/Production_Livestock_E_All_Data.zip",temp2, mode = "wb")
  unzip(temp2, "Production_Livestock_E_All_Data.csv", exdir = "data")#)
  dd <- read.table("data/Production_Livestock_E_All_Data.csv", sep="," , header=T)#skip=2
  unlink(temp2)
  fao_livestock <- dd %>%
    select(! ends_with("F")) %>%  # remove the duplicate year columns
    rename_all(~stringr::str_replace(.,"^Y","")) %>% # remove the Y in front of the column names so just have the years
    filter(Item %in% c("Buffaloes", "Camelids, other", "Camels", "Cattle", "Chickens", "Sheep", "Goats", "Pigs")) %>%
    select(! c(Element.Code, Item.Code, Area.Code)) %>%
    pivot_longer(cols = "1961":"2019", names_to = "Year", values_to = "value") %>%
    mutate(indicator = paste(Element, Item, sep="_")) %>%
    filter(Year %in% chosen_years) %>%
    mutate(Area = as.factor(Area)) %>%
    mutate(Area =  stringi::stri_trans_general(str = Area, id = "Latin-ASCII")) %>%
    mutate_if(is.character, as.factor) %>%
    mutate(Area = fct_recode(Area, 
                             #"Cote d'Ivoire" = "Côte d’Ivoire",
                             "Tanzania" = "United Republic of Tanzania"  )) %>%
    #mutate(Area = fct_recode(Area, 
    #                         "Cote d'Ivoire" = "Côte d'Ivoire",
    #                         "Tanzania" = "United Republic of Tanzania")) %>%
    filter(Area %in% country_names) %>%
    droplevels() %>%
    select(Area, Unit, Year, value, indicator) %>%
    rename(year = Year, country = Area, units = Unit) %>%
    relocate(country, indicator, year, value, units) %>%
    mutate(year = as.factor(year))
  fao_livestock
}