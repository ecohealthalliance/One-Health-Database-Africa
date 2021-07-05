

ingest_indicators.population_api <- function(){
  pull_data <- GET(paste("http://api.worldbank.org/V2/country/all/indicator/SP.POP.TOTL?date=",
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
    select(c(country.value, indicator.value, date, value)) %>%   
    rename(year = date, country = country.value, indicator = indicator.value) %>%
    filter(year %in% chosen_years) %>%
    mutate(year = as.factor(year)) %>%
    mutate(units = "Number")
  
  data_f
  
  df2 <- data_f %>% 
    filter(year %in% c("2010", "2019")) %>%
    mutate(year = as.numeric(as.character(year))) %>%
    group_by(country) %>%
    arrange(year) %>%
    mutate(pct.chg = (value - lag(value))/lag(value)*100) %>%
    mutate(pct.chg = round(pct.chg, 1)) %>%
    select(!c(value,indicator, units)) %>%
    rename(value = pct.chg) %>%
    filter(year == 2019) %>%
    mutate(year = as.factor(year)) %>%
    mutate(indicator = "percent change in population 2010 - 2019") %>%
    mutate(indicator = as.factor(indicator)) %>%
    mutate(units = "Percent") %>%
    relocate(country, indicator, year, value, units)
  
  df3 <- rbind(data_f, df2)
  df3
  
}

#ingest_indicators.population_api()
