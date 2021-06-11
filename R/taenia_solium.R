
ingest_indicators.taenia_solium <- function(){
read_xlsx("data/Taenia solium- Presence of porcine cysticercosis.xlsx", skip = 1) %>%
  rename(country = Location) %>%
  #mutate(country = countrycode::countrycode(ctry_code, origin = "iso3c", destination = "country.name")) %>%
  mutate(country =  stringi::stri_trans_general(str = country, id = "Latin-ASCII")) %>%
  #mutate(year = as.factor(year)) %>%
  mutate(country = as.factor(country)) %>%
  mutate(country = fct_recode(country, 
                             # "Cote d'Ivoire" = "Côte d’Ivoire",
                              "Tanzania" = "United Republic of Tanzania"  )) %>%
  filter(country %in% country_names) %>%
  droplevels() %>%
  rename(year = Period, indicator = Indicator, value = "First Tooltip") %>%
  mutate_if(is.character, as.factor) %>%
  mutate(units = "presence or absence")
}

ingest_indicators.taenia_solium()

