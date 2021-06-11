
# search the data to find the code of the one we want
pull_data <- GET("https://api.resourcewatch.org/v1/dataset?search=forest")
data = fromJSON(rawToChar(pull_data$content))


# Below is the tree cover loss code
# 0448c79d-0ee0-42ff-9331-aeee70cef301

# It does work to pull the right data but looks like it is a raster 
pd2 <- GET("https://api.resourcewatch.org/v1/dataset/0448c79d-0ee0-42ff-9331-aeee70cef301")
d2 <- fromJSON(rawToChar(pd2$content))


# Looks like it is primarily supplying info for maps. I can't see how to download just the dataset in table format