
ingest_indicators.cfe_allocations <- function(){
  cfe <- read_docx("data/CFE allocations-2015-20jan2021.docx") 
  cfe2 <- docx_extract_all_tbls(cfe) # a list of the tables as dataframes
  onelist <- cfe2[[1]]
  new_names <- as.character(onelist[2,])
  names(onelist) <- new_names      # assign the new column names
  onelist <- onelist[3:nrow(onelist), ]    # subset off the first two rows so now have the correct column names
  onelist$Amount <- sub('.', '', onelist$Amount)  # remove the first character which is a $ sign
  onelist$Amount <- str_replace_all(onelist$Amount, ",", "") # and then remove the commas
  fourlist <- plyr::ldply(cfe2[c(2:5)], rbind)%>%
    select(-V5) 
  fourlist$V6 <-   str_replace_all(fourlist$V6, ",", "")
  colnames(fourlist) <- colnames(onelist)
  cfe_all <- rbind(onelist, fourlist)
  #colnames(cfe_all)
  cfe_all <- cfe_all %>%
    mutate(year = str_replace(Date, "^.+-", "")) %>%
    mutate(year = paste0("20", year )) %>%  # convert to full years
    filter(year %in% chosen_years) %>%
    mutate(year = as.factor(year)) %>%
    mutate(Country =  stringi::stri_trans_general(str = Country, id = "Latin-ASCII")) %>%
    mutate(Country = as.factor(Country)) %>%
    mutate(Country = fct_recode(Country, 
                                #"Sao Tome and Principe" = "São Tomé and Príncipe",
                                "Congo" = "Republic of Congo",
                                "Democratic Republic of the Congo" = "Democratic Republic of Congo")) %>%
    filter(Country %in% country_names) %>%
    droplevels() %>%
    select(Country, Amount, year) %>%
    mutate(Amount = as.numeric(Amount)) %>%
    group_by(Country, year) %>%
    summarise(value = sum(Amount)) %>%
    #mutate(indicator = paste0("cfe_", Type)) %>%
    #select(Country, Amount, year, indicator) %>%
    mutate(indicator =  as.factor("cfe allocation")) %>%
    mutate(units = "USD") %>%
    relocate(Country, indicator, year, value, units)%>%
    set_names(colnames_list)
  cfe_all
}

#ingest_indicators.cfe_allocations()
