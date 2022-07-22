ingest_indicators.iucn_mammals <- function(){
 read_csv("data/mammals.csv") %>%
    mutate(country = fct_recode (country,
                                 "Democratic Republic of the Congo" = "Congo, The Democratic Republic of the",
                                 "Egypt, Arab Rep." = "Egypt",
                                 "Tanzania" = "Tanzania, United Republic of")) %>%
    filter(country %in% country_names) %>%
    rename(value = mammals) %>% 
    mutate(year = NA, 
         indicator = "mammals_count",
         units = "number")
}
  