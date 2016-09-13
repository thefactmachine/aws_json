# clear the decks
rm(list=ls()) 
options(stringsAsFactors=FALSE)
library(jsonlite)
library(dplyr)


# read the json file amd parse it into an R list
json_file_name <- "inputs/index_pricing.json"
char_json_file <- readChar(json_file_name, file.info(json_file_name)$size)

# parse the file: convert the JSON text to an R list
lst_pricing <- jsonlite::fromJSON(char_json_file)

# extract the products component
lst_products <- lst_pricing$products
length(lst_products) == 10777


# ==============================================
# This section extracts a data.frame of attributes
# From the the products object.
fn_get_attributes <- function(a_list, str_name) {
  # this function extracts a data.frame of attributes
  # and their values. One data.frame for each list element
  vct_attributes <- a_list[3]$attributes %>% unlist()
  vct_att_name <- names(vct_attributes)
  vct_att_value <- vct_attributes
  vct_sku <- rep(str_name, length(vct_att_name))
  vct_prod_class <- rep(a_list[2]$productFamily, length(vct_att_name))
  df_return <- data.frame(sku = vct_sku, prod_class = vct_prod_class,
                          attr_name = vct_att_name, attr_value = vct_att_value)
  row.names(df_return) <- NULL
  return(df_return)
}

df_result <- lapply(seq_along(lst_products), 
                    function(x) fn_get_attributes(lst_products[[x]], 
                                                  names(lst_products[x]))) %>%
                                                  do.call("rbind", .)
# ======================================================

# We export the data.frame as a zipped object in the outputs directory
HOME <- getwd()
setwd(file.path(HOME, "outputs"))
gz_result <- gzfile("df_result.csv.gz", "w")
write.csv(df_result, gz_result)
close(gz_result)
setwd("..")

# Ende

