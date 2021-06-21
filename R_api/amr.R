ingest_indicators.amr <- function(){
  amr_indicators <- c("Countryname", "4.1 Multi-sector and One Health collaboration/coordination", 
                      "4.2 Which sectors are actively involved in developing and implementing the AMR National Action Plan- [Food Production]",
                      "4.2 Which sectors are actively involved in developing and implementing the AMR National Action Plan- [Environment]",
                      "5.1 Country progress with development of a national action plan on AMR", 
                      "5.2 Is your countrys national action plan on AMR linked to any other existing action plans, strategies or targets related to - [Malaria]",
                      "5.3 If you have published your AMR national action plan, please insert a link here.",
                      "5.4 Country legislations on antimicrobial use [Country has laws or regulations that prohibits the use of antibiotics for growth promotion in the absence of risk analysis.]",
                      "5.4 Country legislations on antimicrobial use [Country has legislation on marketing of pesticides including antimicrobial pesticides, such as bactericides and fungicides used in plant production.]",
                      "6.1 Raising awareness and understanding of AMR risks and response", 
                      "6.1.1 For the level selected above, please indicate the extent of involvement of the sectors below. [Animal Health (terrestrial and aquatic)]",
                      "6.3 Training and professional education on AMR in the veterinary sector",
                      "6.5 Progress with strengthening veterinary services", 
                      "7.2 National monitoring system for antimicrobials intended to be used in animals (terrestrial and aquatic) (sales/use)",
                      "7.5 (b) AMR surveillance is routinely undertaken in animals for the following categories: [Animal (terrestrial and/or aquatic) isolates linked to animal disease.]",
                      "7.5 (b) AMR surveillance is routinely undertaken in animals for the following categories: [Zoonotic pathogenic bacteria]", 
                      "8.2 Good health, management and hygiene practices to reduce the use of antimicrobials and minimize development and transmission of AMR in animal production (terrestrial and aquatic)",
                      "9.2 Optimizing antimicrobial use in animal health (terrestrial and aquatic)",
                      "10. National assessment of risks for AMR transmission in the environment and pollution control. Legislation and/or regulations to prevent contamination of the environment with antimicrobials [Discharges from intensive animal (terrestrial and aquatic) production (liquid waste and manure) a) disposal into the environment][Are risk reduction actions underway-]")

  read_xls("data/AMR self assessment survey responses 2019-2020 (Excel format).xls") %>%
    #rename_all(~str_replace_all(., "\\s+", "")) %>%
    rename(Countryname = 'Country name') %>%
    mutate(Countryname =  stringi::stri_trans_general(str = Countryname, id = "Latin-ASCII")) %>%
    select(all_of(amr_indicators)) %>%  
    mutate_if(is.character, as.factor) %>%
    mutate(Countryname = fct_recode(Countryname, 
                                    #"Cote d'Ivoire" = "Côte d'Ivoire",
                                    "Tanzania" = "United Republic of Tanzania"  )) %>% 
    filter(Countryname %in% country_names) %>%
    droplevels() %>%
    pivot_longer(cols = !(Countryname)) %>%
    mutate(year = "unknown") %>%
    mutate(units = "none") %>%
    relocate(after = c(Countryname, name, year, value, units)) %>%
    set_names(colnames_list) %>%
    mutate(year = as.factor(year)) %>%
    mutate(indicator = as.factor(indicator))
}

#ingest_indicators.amr()

