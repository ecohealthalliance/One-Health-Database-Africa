library(tidyverse)
library(jsonlite)
ingest_indicators.who_oubreaks <- function(){
  
  offset <- 0   # 2000 results returned at a time
  events <- tibble()
  
  repeat {
    
    url <- paste0("https://services.arcgis.com/5T5nSi527N4F7luB/ArcGIS/rest/services/Join_Features_to_AFRO_COUNTRIES_BASIC3/FeatureServer/0/query?where=1%3D1&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&resultType=none&distance=0.0&units=esriSRUnit_Meter&returnGeodetic=false&outFields=*&returnGeometry=false&returnCentroid=false&featureEncoding=esriDefault&multipatchOption=xyFootprint&maxAllowableOffset=&geometryPrecision=&outSR=&datumTransformation=&applyVCSProjection=false&returnIdsOnly=false&returnUniqueIdsOnly=false&returnCountOnly=false&returnExtentOnly=false&returnQueryGeometry=false&returnDistinctValues=false&cacheHint=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&having=&resultOffset=",
                  offset, "&resultRecordCount=&returnZ=false&returnM=false&returnExceededLimitFeatures=true&quantizationParameters=&sqlFormat=none&f=pjson&token=")
    out <- fromJSON(url)
    features <- out$features
    
    if(is_empty(features)) break
    
    attributes <- features$attributes %>% 
      as_tibble() %>% 
      janitor::clean_names() %>% 
      filter(type_of_event !=  "Humanitarian crises") %>% 
      select(object_id, iso_3_code,who_code, country, event, 
             date_notified_to_wco, start_of_reporting_period, end_of_reporting_period,
             total_cases, cases_confirmed, deaths) %>%
      mutate_at(.vars = c( "date_notified_to_wco", "start_of_reporting_period", "end_of_reporting_period"), 
                ~lubridate::as_date(lubridate::as_datetime(as.numeric(str_sub(format(., scientific = FALSE), end = -4)))))
    
    events <- bind_rows(events, attributes)
    
    offset <- offset + 2000
    
  }
  
  # export full dataset
  # write_csv(events, here::here("data", "who_outbreaks_full.csv"))
  
  # add field for length of reporting period
  events <- events %>% 
    group_by(country, event) %>% 
    mutate(length_of_reporting_period_days = as.integer(end_of_reporting_period - start_of_reporting_period )) %>% 
    ungroup()
  
  # id events with more than one start date
  # events %>% 
  #   group_by(country, event) %>% 
  #   summarize(n = n_distinct(start_of_reporting_period)) %>% 
  #   filter(n>1) %>% 
  #   ungroup()
  
  # filter for most recent cumulative event
  events <- events %>% 
    group_by(country, event) %>% 
    mutate(max_total_cases = max(total_cases)) %>% # for qa check
    filter(end_of_reporting_period == max(end_of_reporting_period)) %>% 
    filter(total_cases == max(total_cases)) %>% # sometimes more than one value reported for a given day
    ungroup() %>% 
    select(-object_id) %>% 
    distinct()
  
  # id events where most recent isn't the greatest cumulative value
  # events %>%
  #   filter(max_total_cases != total_cases) %>% View
  
  # id events with more than one date_notified_to_wco (but same end of reporting period)
  # events %>% 
  #   group_by(country, event) %>% 
  #   summarize(n = n_distinct(date_notified_to_wco)) %>% 
  #   ungroup()  %>% 
  #   filter(n>1)
  
  # assume oldest date notified to wco
  events <- events %>% 
    group_by(country, event) %>% 
    filter(date_notified_to_wco == min(date_notified_to_wco)) %>% 
    ungroup()
  
  return(events)
  
  # check there is one row for each event
  # events %>% 
  #   group_by(country, event) %>% 
  #   summarize(n = n()) %>% 
  #   ungroup() %>% 
  #   distinct(n)
  
  # export condensed dataset
 write_csv(events, here::here("data", "who_outbreaks_compact.csv"))
  
  
}
