ingest_indicators.population <- function(){
df1 <-  read_csv("data/Population_World Bank.csv", skip = 3) %>%
    rename_all(~str_replace_all(., "\\s+", "")) %>%
    mutate_if(is.character, as.factor) %>%
    mutate(CountryName = fct_recode(CountryName, 
                                    "Gambia" = "Gambia, The",
                                    "Congo" = "Congo, Rep.",
                                    "Democratic Republic of the Congo" = "Congo, Dem. Rep.")) %>%
    filter(CountryName %in% country_names) %>%
    droplevels() %>%
    select(-c(CountryCode, IndicatorCode)) %>%
    pivot_longer(cols = !c(CountryName, IndicatorName), names_to = "year", values_to = "value") %>%
    mutate(units = "Number") %>%
    set_names(colnames_list) %>%
    filter(year %in% chosen_years) %>%
    mutate(year = as.factor(year))


df2 <- df1 %>% 
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

df3 <- rbind(df1, df2)
df3

}
