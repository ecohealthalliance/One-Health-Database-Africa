
ingest_indicators.arable_land_api <- function(){
pull_data <- GET("http://api.worldbank.org/V2/country/all/indicator/AG.LND.ARBL.HA;AG.LND.ARBL.ZS?source=2&date=2011:2020&per_page=10000&format=json")
data = fromJSON(rawToChar(pull_data$content))

d2 <- data[[2]]
d2_full <- do.call(data.frame, d2)
data_f <- d2_full %>% 
  rename_all(~str_replace_all(., "\\s+", "")) %>%
  mutate(country.value = as.factor(country.value)) %>%  
  mutate(country.value = fct_recode(country.value, 
                                  "Gambia" = "Gambia, The",
                                  "Congo" = "Congo, Rep.",
                                  "Democratic Republic of the Congo" = "Congo, Dem. Rep.")) %>%
  filter(country.value %in% country_names) %>%
  droplevels() %>%
  select(c(country.value, indicator.value, date, value)) %>%   # possibly remove the 1960 column as no data in it
  rename(year = date, country = country.value, indicator = indicator.value) %>%
  filter(year %in% chosen_years) %>%
  mutate(year = as.factor(year)) %>%
  mutate(units = case_when(
    endsWith(indicator, "(% of land area)") ~ "percent",
    endsWith(indicator, "(hectares)") ~ "hectares"))

final_df <- data_f %>%
  mutate(indicator = as.factor(indicator)) %>%
  mutate(value = as.numeric(value))

final_df
}

ingest_indicators.arable_land_api()
