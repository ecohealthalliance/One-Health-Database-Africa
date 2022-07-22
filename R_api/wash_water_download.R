
ingest_indicators.wash_water_download <- function(){
download.file("https://data.unicef.org/wp-content/uploads/2017/07/WASH-data-tables.xlsx",
              destfile = "./temp.xlsx", mode = "wb")
wash <- read_xlsx("./temp.xlsx", sheet = 2) %>%
  select(`DRINKING WATER...1`, Year...2, NATIONAL...5, RURAL...10) %>% # first select the columns we want
  set_names("country", "year", "water_national_at_least_basic", "water_rural_at_least_basic") %>%
  slice(-c(1:2)) %>%
  mutate_if(is.character, as.factor) %>%
  mutate(country =  stringi::stri_trans_general(str = country, id = "Latin-ASCII")) %>%
  mutate(country = fct_recode(country,
                           #   "Cote d'Ivoire" = "Côte d'Ivoire",
                              "Tanzania" = "United Republic of Tanzania",
                           "Egypt, Arab Rep." = "Egypt")) %>%
  filter(country %in% country_names) %>%
  droplevels() %>%
  mutate(year = as.factor(year)) %>%
  mutate(water_national_at_least_basic =  as.character(water_national_at_least_basic)) %>%
  mutate(water_rural_at_least_basic = as.character(water_rural_at_least_basic)) %>%
  mutate(water_rural_at_least_basic = sub(">", "", water_rural_at_least_basic)) %>%
  mutate(water_rural_at_least_basic = sub("-", "", water_rural_at_least_basic)) %>%
  mutate(water_national_at_least_basic = sub(">", "", water_national_at_least_basic)) %>%
  mutate(water_national_at_least_basic = sub("-", "", water_national_at_least_basic)) %>%
  mutate_if(is.character, as.numeric) %>%
  mutate_at(c("water_national_at_least_basic", "water_rural_at_least_basic"), round, 1) %>%
  pivot_longer(cols = !c(country, year), names_to = "indicator", values_to = "value") %>%
  mutate(indicator = as.factor(indicator)) %>%
  mutate(units = "percent") %>%
  relocate(country, indicator, year, value, units) %>%
  filter(year %in% chosen_years)

wash
}

