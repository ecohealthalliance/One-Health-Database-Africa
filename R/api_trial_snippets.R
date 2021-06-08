# Struggling with unicef

pull_data <- GET("https://sdmx.data.unicef.org/ws/public/sdmxapi/rest/data/UNICEF,WASH_HOUSEHOLDS,1.0/.WS_PPL_W-ALB.._T._T?format=sdmx-json")
data = fromJSON(rawToChar(pull_data$content))
d2 <- data[["fact"]]
dim_com <- d2$Dim

# Try WorldBank

#https://datacatalog.worldbank.org/search?query=%20arable-land-hectares-1

pull_data <- GET("http://api.worldbank.org/V2/country/all/indicator/AG.LND.ARBL.HA?date=2011:2019&per_page=1000&format=json")
data = fromJSON(rawToChar(pull_data$content))

#http://api.worldbank.org/v2/country/all/indicator/SP.POP.TOTL?per_page=25

#http://api.worldbank.org/v2/country/all/indicator/SP.POP.TOTL?format=json

pull_data_2 <- GET("http://api.worldbank.org/V2/country/ALL/indicator/ER.LND.PTLD.ZS?format=json")
data2 = fromJSON(rawToChar(pull_data_2$content))


#######
pull_data <- GET("https://apps.who.int/gho/athena/api/GHO/MALARIA002.json")
data = fromJSON(rawToChar(pull_data$content))
d2 <- data[["fact"]]

trying <- d2 %>% unnest_wider(Dim) %>% unnest_wider(code)
