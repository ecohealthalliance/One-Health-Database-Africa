"IHRSPAR_CAPACITY03"

pull_data <- GET("https://apps.who.int/gho/athena/api/GHO/IHRSPAR_CAPACITY03.json")
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




