# clear the decks
rm(list=ls()) 
options(stringsAsFactors=FALSE)
library(jsonlite)
library(dplyr)
# library(httr)


# This codes gets "inputs/index_offerings.json"
# parses this json file to get the urls .....
# and then downloads data from the urls (2 x 13 = 26 urls)
# and then stores two lists as an RDA file.

# read the json file amd parse it into an R list
json_file_name <- "inputs/index_offerings.json"
char_json_file <- readChar(json_file_name, file.info(json_file_name)$size)
lst_offerings <- jsonlite::fromJSON(char_json_file)

# get the top level names of the offers
vct_offer_names <- names(lst_offerings$offers)

str_url_prefix <- "https://pricing.us-east-1.amazonaws.com/offers/v1.0/aws/"
str_url_suffix_version <- "/index.json"
str_url_suffix_current <- "/current/index.json"


lst_version <- list()
for (i in 1: length(vct_offer_names)) {
  str_url <-  paste0(str_url_prefix, vct_offer_names[i], 
                     str_url_suffix_version)
  lst_version[[i]] <- jsonlite::fromJSON(str_url)
  print(i)
}
names(lst_version) <- vct_offer_names

# =================================================

lst_current <- list()
for (i in 1: length(vct_offer_names)) {
  str_url <-  paste0(str_url_prefix, vct_offer_names[i], 
                     str_url_suffix_current)
  lst_current[[i]] <- jsonlite::fromJSON(str_url)
  print(i)
}
names(lst_current) <- vct_offer_names

# ====================================================
save(lst_version, lst_current, file = "inputs/binary/offerings.Rdata" )
# use the following to load:  load("inputs/binary/offerings.Rdata")
# ===================================================


