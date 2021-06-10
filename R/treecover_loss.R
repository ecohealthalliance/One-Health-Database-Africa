

ingest_indicators.treecover_loss <- function(){
 read.csv("data/treecover_loss_primary_forests_2001_tropics_only_ha.csv")  %>%
   mutate(country = countrycode::countrycode(iso, origin = "iso3c", destination = "country.name")) %>%
   mutate(country =  stringi::stri_trans_general(str = country, id = "Latin-ASCII")) %>%
   mutate(country = as.factor(country)) %>%
   rename(value = "umd_tree_cover_loss__ha") %>%
   mutate(indicator = as.factor("tree_cover_loss")) %>%
   mutate(country = fct_recode(country, 
                                   "Congo" = "Congo - Brazzaville",
                                   "Democratic Republic of the Congo" = "Congo - Kinshasa")) %>%
   filter(country %in% country_names) %>%
   droplevels() %>%
   rename(year = "umd_tree_cover_loss__year") %>%
   mutate(units = "hectares") %>%
   filter(year %in% chosen_years) %>%
   mutate(year = as.factor(year)) %>%
   select(c(country, indicator, year, value, units)) %>%
   mutate(value = round(value, 2))
  }

#ingest_indicators.treecover_loss()
