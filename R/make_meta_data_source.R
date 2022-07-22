## create a condensed csv for metadata sources 
## and terms of use
library(dplyr)
library(urltools)

metadata <- read.csv("gheri_africom_indicators_metadata.csv")

md_df <- metadata %>% 
  mutate(base_url = domain(source_url)) 

md_df%>% 
  select(base_url,terms_of_use,potential_violation) %>% 
  distinct(base_url,.keep_all = TRUE) %>% 
  write_csv("meta_data_sources.csv",row.names = FALSE) 
  
system("open meta_data_sources.csv")

## make any changes to meta_data_sources and read in updated
## file

terms_of_use <- read.csv("meta_data_sources.csv")

terms_of_use <- terms_of_use %>% 
  select(-X)

names(terms_of_use)

drop_cols <- md_df %>% 
  select(-one_of(c("terms_of_use","potential_violation"))) 

md_df_tou <- left_join(drop_cols,terms_of_use,by = "base_url")

write_csv(md_df_tou,"gheri_africom_indicators_metadata.csv")
