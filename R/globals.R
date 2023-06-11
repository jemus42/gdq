gdq_base_url <- "https://gamesdonequick.com"

# Guess I need to keep track of that number now.
# Last checked 2023-06-06
# jsonlite::read_json("https://cdn.jsdelivr.net/gh/fawazahmed0/currency-api@1/latest/currencies/usd/eur.json")$eur
.usd_to_eur <- 0.93


# To be removed over time as code is made more... gooder.
globalVariables(c(
  "."
))
