## In this code I am going to look at trying to source the data with an API. 
## Starting with the WHO data.

# install.packages(c("httr", "jsonlite"))

library(httr)
library(jsonlite)


try <- GET("https://apps.who.int/gho/athena/api/GHO/HWF_0002.json")

try2 <- GET("https://apps.who.int/gho/athena/api/GHO/HWF_0002?format=csv")

view(try)
view(try2) # these look the same

data = fromJSON(rawToChar(try$content))
data2 <-(rawToChar(try2$content))

data2

d2 <- data[["fact"]]
#view(d2)

class(d2) ## looks like i need to extract the info from the Dim column
d3 <- d2[1:10,]

# d3 %>% 
#  mutate(Dim = Dim[[]]$code)# %>%
#  separate(Dim, into = c("p", "y", "r", "t", "s"), convert = TRUE)
#
#dim <- map(d3$Dim, toString)
#  
#dims_list <- d3$Dim

#ab <-
#lapply(dims_list, function(x) x%>% select(code)) %>%
#  separate(into = c("p", "y", "r", "t", "s"), convert = TRUE)

#d4 <- d3  %>% mutate(Dim_new = map(Dim, function(x) x%>% select(code)))# %>%
#  separate(Dim_new, into = c("p", "y", "r", "t", "s"), convert = TRUE)

#d2$Dim[[2]]$code

# one option would be to separate out the dims column, make it into a data set for 

dim_com <- d3$Dim

ade <- dim_com %>% reduce(left_join, by = "category") %>%
  t(.) %>%
  as.data.frame(.) %>%
  janitor::row_to_names(row_number = 1) %>%
  dplyr::select(c(YEAR, COUNTRY))

new_df <- cbind(d3, ade)

#https://ghoapi.azureedge.net/api/Indicator?$filter=contains(IndicatorName,%20%27doctor%27)

###Try and put the different bits together

try <- GET("https://apps.who.int/gho/athena/api/GHO/HWF_0002.json")
data = fromJSON(rawToChar(try$content))
d2 <- data[["fact"]]
dim_com <- d2$Dim

ade <- dim_com %>% reduce(left_join, by = "category") %>%
  t(.) %>%
  as.data.frame(.) %>%
  janitor::row_to_names(row_number = 1) %>%
  dplyr::select(c(YEAR, COUNTRY))

new_df <- cbind(d2, ade)

## If did it this way would then need to convert the countrycode to the country name.
## Doesn't seem like the easiest option as I've got it at present

###########################################################################################################
## Look at a different data set to see if just as complex!

codeswho <- GET("https://apps.who.int/gho/athena/api/GHO.json")

cw <- fromJSON(rawToChar(codeswho$content))
cw  ## can use this to look up the codes. Although need to know the exact title WHO use. e.g. rabies deaths is under "Reported number of human rabies deaths"
## look at cw - dimension - code


rabs <- GET("https://apps.who.int/gho/athena/api/GHO/NTD_RAB2.json")
rabs_cont <- fromJSON(rawToChar(rabs$content))
