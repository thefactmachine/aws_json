# clear the decks
rm(list=ls()) 
options(stringsAsFactors=FALSE)
library(jsonlite)
library(dplyr)
# library(httr)


# read the json file amd parse it into an R list
json_file_name <- "inputs/index_offerings.json"
char_json_file <- readChar(json_file_name, file.info(json_file_name)$size)
lst_offerings <- jsonlite::fromJSON(char_json_file)

# get the top level names of the offers
vct_offer_names <- names(lst_offerings$offers)

# two urls for AmazonS3
str_version_index <- lst_offerings$offers$AmazonS3$versionIndexUrl
str_current_version <- lst_offerings$offers$AmazonS3$currentVersionUrl


str_url_test_version  <- "https://pricing.us-east-1.amazonaws.com/offers/v1.0/aws/AmazonS3/index.json"
str_url_test_current <- "https://pricing.us-east-1.amazonaws.com/offers/v1.0/aws/AmazonS3/current/index.json"
# both the following functions are from the httr package.

# version is the small one:
lst_test_version <- jsonlite::fromJSON(str_url_test_version)

# current is the big one:
lst_test_current <- jsonlite::fromJSON(str_url_test_current)




