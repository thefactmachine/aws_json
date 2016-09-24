# clear the decks
rm(list=ls()) 
options(stringsAsFactors=FALSE)
library(jsonlite)
library(dplyr)


# read the json file amd parse it into an R list
json_file_name <- "inputs/index_pricing.json"
char_json_file <- readChar(json_file_name, file.info(json_file_name)$size)
lst_pricing <- jsonlite::fromJSON(char_json_file)

# This is 37.3 Mb / length is 10761
lst_on_demand <- lst_pricing$terms$OnDemand
# This is 134 Mb / Length is 5672
lst_reserved <- lst_pricing$terms$Reserved

# helper function takes n characters from right
fn_substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}


fn_price_dim <- function(lst_pd) {
  vct_atomic <- c("rateCode", "description", "beginRange", "endRange" ,"unit")         
  # non-atomic "pricePerUnit" "appliesTo" 
  df_atomic <- lst_pd[vct_atomic] %>% as.data.frame()
  # extract single value from the list: pricePerUnit
  str_curr_ticker <- names(lst_pd$pricePerUnit) %>% as.vector()
  flt_curr_amount <- lst_pd$pricePerUnit[[1]]

  df_lists <- data.frame(lth_price_unit = length(lst_pd$pricePerUnit),
                  lth_applies_to =  length(lst_pd$appliesTo),  
                  currency = str_curr_ticker,
                  amount = flt_curr_amount
                  )

  return(cbind(df_atomic, df_lists))
}

fn_call_price_dim <- function(lst_element) {
  # called by lapply (below) calls fn_price_dim()
  # this is for the atomic components
  vct_prim_name <- c( "sku", "offerTermCode", "effectiveDate")
  df_prim_values <- lst_element[vct_prim_name] %>% as.data.frame()
  
  int_lng_term_attr <- length(lst_element$termAttributes)
  df_ta <- data.frame(lth_term_attr = int_lng_term_attr)
  df_prim_values <- cbind(df_prim_values, df_ta)
  
  
  # this is for the complex price dimension
  lst_pd <- lst_element$priceDimensions
  df_pd <- lapply(seq_along(lst_pd), function(x) 
            fn_price_dim(lst_pd[[x]])) %>% do.call("rbind", .)
  # clean up the rateCode
  df_pd$rateCode <- fn_substrRight(df_pd$rateCode, 10)

  # this repeats the atomic components so that they match price
  index <- rep(seq_len(nrow(df_prim_values)), each = nrow(df_pd))
  df_composite <- cbind(df_prim_values[index,],df_pd)
  
  return(df_composite)
  
}

# df_test <- fn_call_price_dim(lst_on_demand$KRXM4TG87VW2NPHA[[1]]);df_test

# takes about 1 minute
df_on_demand <- lapply(seq_along(lst_on_demand), 
                  function(x) fn_call_price_dim(lst_on_demand[[x]][[1]])) %>%
                    do.call("rbind", .)


df_on_demand$lth_term_attr <- NULL
df_on_demand$lth_price_unit <- NULL
df_on_demand$lth_applies_to <- NULL

vct_ordered <- c("sku", "offerTermCode", "rateCode", "effectiveDate", "beginRange", 
                 "endRange" ,"unit" ,"currency", "amount","description")
 
df_on_demand <- df_on_demand[, vct_ordered] 

# notes:
# lth_term_attr is always 0
# lth_price_unit is always 1
# lth_applies_to is always 0

# We export the data.frame as a zipped object in the outputs directory
HOME <- getwd()
setwd(file.path(HOME, "outputs"))
gz_result <- gzfile("df_on_demand.csv.gz", "w")
write.csv(df_on_demand, gz_result)
close(gz_result)
setwd("..")

nrow(df_on_demand)


# Ende

# =====================================
# =====================================
# =====================================








