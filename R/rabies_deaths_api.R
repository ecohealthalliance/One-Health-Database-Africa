
ingest_indicators.rabies_deaths_api <- function(){
  
rabs <- GET("https://apps.who.int/gho/athena/api/GHO/NTD_RAB2.json")
rabs_cont <- fromJSON(rawToChar(rabs$content))
rabs_cont <- rabs_cont[["fact"]]

rab_dat <- rabs_cont$Dim

rab_dat2<- rab_dat %>% reduce(left_join, by = "category") %>%
  t(.) %>%
  as.data.frame(.) %>%
  janitor::row_to_names(row_number = 1) %>%
  dplyr::select(c(YEAR, COUNTRY))

rab_df <- cbind(rabs_cont, rab_dat2) %>%
  rename(countrycode = COUNTRY)

rab_df$country <- countrycode::countrycode(rab_df$countrycode, origin = "iso3c", destination = "country.name")

full_df <- rab_df %>%
  rename(year = YEAR) %>%
  select(year, country, value) %>%
  mutate(country =  stringi::stri_trans_general(str = country, id = "Latin-ASCII")) %>%
  mutate(year = as.factor(year)) %>%
  mutate(country = as.factor(country)) %>%
  mutate(country = fct_recode(country, 
                              "Cabo Verde" = "Cape Verde",
                              "Congo" = "Congo - Brazzaville",
                              "Democratic Republic of the Congo" = "Congo - Kinshasa",
                              "Sao Tome and Principe" = "Sao Tome & Principe")) %>%
  filter(country %in% country_names) %>%
  droplevels()

full_df <- do.call(data.frame, full_df)
full_df_2 <- full_df %>%
  select(country, year, value.numeric) %>%
  rename(value = value.numeric) %>%
  mutate(indicator = "rabies_deaths") %>%
  mutate(indicator = as.factor(indicator)) %>%
  mutate(units = "number") %>%
  relocate("country", "indicator", "year", "value", "units")

full_df_2

}

ingest_indicators.rabies_deaths_api()
