
ingest_indicators.yellow_fever_api <- function(){
  pull_data <- GET("https://apps.who.int/gho/athena/api/GHO/WHS3_50.json")
  data = fromJSON(rawToChar(pull_data$content))
  d2 <- data[["fact"]]
  dim_com <- d2$Dim
  ln <- as.numeric(length(dim_com))
  dim_com2 <- dim_com[10:ln]
  d2 <- d2[10:ln,]
  
  # I think the data I want starts at list 10 so need to trim 
  ade <- dim_com2 %>% reduce(left_join, by = "category") %>%
    t(.) %>%
    as.data.frame(.) %>%
    janitor::row_to_names(row_number = 1) %>%
    dplyr::select(c(YEAR, COUNTRY))
  
  new_df <- cbind(d2, ade)
  
  ## Then need to convert the countrycode to the country name.
  
  full_df <- new_df %>%
    rename(ctry_code = COUNTRY) %>%
    rename(year = YEAR) %>%
    select(year, ctry_code, value) %>%
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
  
  full_df <- do.call(data.frame, full_df)
  full_df_2 <- full_df %>%
    select(country, year, value.numeric) %>%
    rename(value = value.numeric) %>%
    mutate(indicator = "yellow_fever_cases") %>%
    mutate(indicator = as.factor(indicator)) %>%
    mutate(units = "number") %>%
    relocate("country", "indicator", "year", "value", "units")
  
  full_df_2
}

tictoc::tic()
ingest_indicators.yellow_fever_api()
tictoc::toc()



