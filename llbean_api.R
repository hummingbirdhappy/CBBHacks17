require(httr)
require(jsonlite)
require(rjson)

query <- "https://test.api.llbean.com/v1/inventory/item/212880?inventoryLocationId=60%2C40"

raw <- GET(url=query, add_headers(key="X28o7rfOQ1QI8qAxYyk6B1qEh7mW2QJu"))
names(raw)
raw$status_code
content(raw) 

