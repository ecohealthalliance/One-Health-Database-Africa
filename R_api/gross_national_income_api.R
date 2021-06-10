

ingest_indicators.gross_national_income <- function(){
  pull_data <- GET(paste("http://api.worldbank.org/V2/country/all/indicator/NY.GNP.PCAP.CD?date=",
                         dates_to_pull, "&per_page=10000&format=json", sep = ""))
  
  data = fromJSON(rawToChar(pull_data$content))
  
  d2 <- data[[2]]
  d2_full <- do.call(data.frame, d2)
  data_f <- d2_full %>% 
    rename_all(~str_replace_all(., "\\s+", "")) %>%
    mutate_if(is.character, as.factor) %>%  
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
    mutate(units = "USD")
  
  data_f
}

ingest_indicators.gross_national_income()
