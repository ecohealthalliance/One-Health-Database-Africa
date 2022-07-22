ingest_indicators.wash_sanitation_download <- function(){
  download.file("https://data.unicef.org/wp-content/uploads/2017/07/WASH-data-tables.xlsx",
                destfile = "./temp.xlsx", mode = "wb")
  sani <- read_xlsx("./temp.xlsx", sheet = 4 )  %>%
    select(`SANITATION...1`, Year...2, NATIONAL...5) %>%
    set_names("country", "year", "sanitation_national_at_least_basic") %>%
    slice(-c(1:2)) %>%
    #mutate(name = as.factor(name)) %>%
    mutate(country = as.factor(country)) %>%
    mutate(year = as.factor(year)) %>%
    # rename(country = name) %>%
    mutate(country =  stringi::stri_trans_general(str = country, id = "Latin-ASCII")) %>%
    mutate(country = fct_recode(country,
                                #"Cote d'Ivoire" = "Côte d'Ivoire",
                                "Tanzania" = "United Republic of Tanzania",
                                "Egypt, Arab Rep." = "Egypt")) %>%
    filter(country %in% country_names) %>%
    droplevels() %>%
    filter(year %in% chosen_years) %>%
    mutate(units = "percent") %>%
    mutate(indicator = "sanitation_national_at_least_basic") %>%
    mutate(indicator = as.factor(indicator)) %>%
    rename(value = "sanitation_national_at_least_basic") %>%
    # rename(value = wat_bas_n) %>%
    relocate(country, indicator, year, value, units) %>%
    mutate(value = as.numeric(value)) %>% 
    mutate_at("value", round, 1)
  
  sani
}

#ingest_indicators.wash_sanitation_download()
