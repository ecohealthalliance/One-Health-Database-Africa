
ingest_indicators.medical_doctors_api <- function(){
pull_data  <- GET("https://apps.who.int/gho/athena/api/GHO/HWF_0002.json")
data = fromJSON(rawToChar(pull_data$content))
d2 <- data[["fact"]]

new_df <- d2 %>% 
  mutate(country = map(Dim, function(x){
    x %>% 
      filter(category == "COUNTRY") %>% 
      pull(code)
  })) %>% 
  mutate(year = map(Dim, function(x){
    x %>% 
      filter(category == "YEAR") %>% 
      pull(code)
  })) %>%
  select(country, year, value) %>%
  unnest(., c(country, year), keep_empty = T) %>%
  do.call(data.frame, .) %>%
  select(country, year, value.numeric)


full_df <- new_df %>%
  rename(ctry_code = country) %>%
  mutate(country = countrycode::countrycode(ctry_code, origin = "iso3c", destination = "country.name")) %>%
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
  
full_df_2 <- full_df %>%
  select(country, year, value.numeric) %>%
  rename(value = value.numeric) %>%
  mutate(indicator = "Medical doctors (number)") %>%
  mutate(indicator = as.factor(indicator)) %>%
  mutate(units = "number") %>%
  relocate("country", "indicator", "year", "value", "units")

full_df_2
}


#ingest_indicators.medical_doctors_api()




