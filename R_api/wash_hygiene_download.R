ingest_indicators.wash_hygiene_download <- function(){
  download.file("https://data.unicef.org/wp-content/uploads/2017/07/WASH-data-tables.xlsx",
                destfile = "./temp.xlsx", mode = "wb")
  hygiene <- read_xlsx("./temp.xlsx", sheet = 6 ) %>%
    select(HYGIENE, Year, NATIONAL) %>%
    slice(-c(1:2))  %>%
    set_names("country", "year", "hygiene_national_at_least_basic") %>%
    mutate_if(is.character, as.factor) %>%
    mutate(country =  stringi::stri_trans_general(str = country, id = "Latin-ASCII")) %>%
    mutate(country = fct_recode(country,
#                                "Cote d'Ivoire" = "Côte d'Ivoire",
                                "Tanzania" = "United Republic of Tanzania",
                                "Egypt, Arab Rep." = "Egypt")) %>%
    filter(country %in% country_names) %>%
    droplevels()  %>%
    mutate(year = as.factor(year)) %>%
    mutate(hygiene_national_at_least_basic =  as.character(hygiene_national_at_least_basic)) %>%
    mutate(hygiene_national_at_least_basic = sub(">", "", hygiene_national_at_least_basic)) %>%
    mutate(hygiene_national_at_least_basic = sub("-", "", hygiene_national_at_least_basic)) %>%
    mutate_if(is.character, as.numeric) %>%
    mutate_at("hygiene_national_at_least_basic", round, 1) %>%
    rename(value = "hygiene_national_at_least_basic") %>%
    mutate(indicator = "Access to basic hygiene (national)") %>%
    mutate(indicator = as.factor(indicator)) %>%
    mutate(units = "percent") %>%
    relocate(country, indicator, year, value, units) %>%
    filter(year %in% chosen_years)
  
  hygiene
}

#ingest_indicators.wash_hygiene_download()
