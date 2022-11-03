
ingest_indicators.jee <- function(){
  jee_indicators <- c("Country", "JEE Year", 
                      "P.4.1 Surveillance systems in place for priority zoonotic diseases/pathogens (v1)/ P.4.1 Coordinated surveillance systems in place in the animal health and public health sectors for zoonotic diseases/pathogens identified as joint priorities (v2)",
                      "P.4.3 Mechanisms for responding to infectious and potential zoonotic diseases are established and functional (v1) / P.4.2 Mechanisms for responding to infectious and potential zoonotic diseases established and functional (v2)",
                      "P.6.1 Whole-of-government biosafety and biosecurity system is in place for human, animal and agriculture facilities",   
                      "P.6.2 Biosafety and biosecurity training and practices",
                      "P.7.2 National vaccine access and delivery",
                      "D.1.1 Laboratory testing for detection of priority diseases",
                      "D.1.2 Specimen referral and transport system",
                      "D.1.4 Laboratory quality system",
                      "D.3.1 System for e cient reporting to FAO, OIE and WHO",
                      "D.3.2 Reporting network and protocols in country",
                      "D.4.2 FETP1 or other applied epidemiology training programme in place (v1)/ D.4.4 FETP or other applied epidemiology training programme is in place (v2)",
                      "D.4.3 Workforce strategy (v1)/ D.4.1 An up-to-date multisectoral workforce strategy is in place (v2)",
                      "R.3.1 Public health and security authorities (e.g. law enforcement, border control, customs) are linked during a suspect or con rmed biological event (v1)/ R.3.1 Public health and security authorities (e.g. law enforcement, border control, customs) linked during a suspect or confirmed biological, chemical or radiological event",
                      "R.5.2 Internal and partner communication and coordination (v1)/ R.5.2 Internal and partner coordination for emergency risk communication (v2)",
                      "PoE.1 Routine capacities established at points of entry",
                      "PoE.2 E ective public health response at points of entry")
  
  read_xlsx("data/JEE scores.xlsx")  %>%
    select(all_of(jee_indicators)) %>%
    mutate_if(is.character, as.factor) %>%
    mutate(Country = fct_recode(Country, 
                                "Gambia" = "Gambia, The",
                                "Congo" = "Congo, Rep.",
                                "Democratic Republic of the Congo" = "Congo, Dem. Rep.")) %>%
    filter(Country %in% country_names) %>%
    droplevels() %>%
    rename(year = "JEE Year") %>%
    pivot_longer(cols =!c(Country, year), names_to = "indicator", values_to = "value") %>%
    mutate(units = NA) %>%
    mutate(year = fct_recode(year, "2016" = "2016 - some with 2 assessments Country vs external experts")) %>%
    droplevels() %>%
    mutate(indicator = as.factor(indicator)) %>%
    mutate(indicator = fct_recode(indicator, 
                                "D.3.1 System for efficient reporting to FAO, OIE and WHO" = "D.3.1 System for e cient reporting to FAO, OIE and WHO",
                                "D.4.4 FETP or other applied epidemiology training programme is in place (v2)" = "D.4.2 FETP1 or other applied epidemiology training programme in place (v1)/ D.4.4 FETP or other applied epidemiology training programme is in place (v2)",
                                "R.3.1 Public health and security authorities (e.g. law enforcement, border control, customs) are linked during a suspect or confirmed biological event (v1)/ R.3.1 Public health and security authorities (e.g. law enforcement, border control, customs) linked during a suspect or confirmed biological, chemical or radiological event"="R.3.1 Public health and security authorities (e.g. law enforcement, border control, customs) are linked during a suspect or con rmed biological event (v1)/ R.3.1 Public health and security authorities (e.g. law enforcement, border control, customs) linked during a suspect or confirmed biological, chemical or radiological event",
                                "PoE.2 Effective public health response at points of entry" = "PoE.2 E ective public health response at points of entry")) %>% 
    relocate(Country, indicator, year, value, units) %>%
    set_names(colnames_list)
}

ingest_indicators.jee()
