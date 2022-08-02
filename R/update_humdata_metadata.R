## creates a json file with appropriate descriptive metadata

source("packages.R")

# hdx_metadata <- rvest::read_html("https://centre.humdata.org/providing-metadata-for-your-datasets-on-hdx/")
# 
# hdx_tables <- rvest::html_table(hdx_metadata)
# 
# hdx_standard <- hdx_tables[[3]] %>% 
#   select(X2,X3) %>% 
#   rename(field_name = "X2",
#          field_description = "X3") %>% 
#   filter(field_name != "Metadata field")
# 
# saveRDS(hdx_standard,file = "data/hdx_core_metadata_standard.RDS")

hdx_standard <- readRDS("data/hdx_core_metadata_standard.RDS")

hdx_standard_list <- sapply(hdx_standard$field_name, function(x) NULL)

hdx_gheri <- hdx_standard_list


hdx_gheri$`Title of Dataset` <- "One health database for Africa"
hdx_gheri$Description <- "The “Strategic Coordination to Strengthen AFRICOM One Health and
Veterinary Programs for Global Health Engagement” reviews current capacity 
and programmatic status, gaps, and operations across the continent of Africa. 
This repository combines datasets from multiple sources to produce a single One
Health database containing all the selected indicators. 
The database encompasses 54 countries across the continent (according the UN)."
hdx_gheri$`Dataset contains sub-national data` <- FALSE

## get sources

sources_df <- read_csv("meta_data_sources.csv")

hdx_gheri$Source <- as.character(na.omit(unique(sources_df$organization)))
hdx_gheri$Organisation <- "EcoHealth Alliance"
hdx_gheri$Maintainer <- "Sarah Baum"
hdx_gheri$`Date of Dataset` <- lubridate::today()

# get location metadata

country_df <- read_excel("data/AFRICOM List.xlsx")
countries_string <- paste(country_df$`Country Code`,collapse = ", ")
hdx_gheri$Location <- countries_string

# get field names

field_df <- read_csv("Output/full_data_combined_api.csv")

hdx_gheri$`Field Names` <- names(field_df)
hdx_gheri$`File Types` <- "CSV"
hdx_gheri$`Number of Rows` <- nrow(field_df)
hdx_gheri$License <- "https://creativecommons.org/licenses/by/4.0/legalcode"
hdx_gheri$Methodology <- "Other"
hdx_gheri$`Define Methodology` <- "Other"
hdx_gheri$`Define Methodology` <- "@Sarah"
hdx_gheri$`Update Frequency` <- "Every three months"
hdx_gheri$`Caveats/Comments` <- "@Sarah"
hdx_gheri$Tags <- "@Sarah"

metadata_json <- toJSON(hdx_gheri,pretty = TRUE)

write_json(metadata_json,path = "hdx_metadata.json")

