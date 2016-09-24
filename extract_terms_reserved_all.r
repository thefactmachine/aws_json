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
# lst_on_demand <- lst_pricing$terms$OnDemand
# This is 134 Mb / Length is 5672
lst_reserved <- lst_pricing$terms$Reserved

class(lst_reserved) == "list"
length(lst_reserved) == 5672

# iterate through 5672 lists and get their length
sapply(lst_reserved, length) %>% table()
# so..there is one list of 2 elements; 7 lists of 3 elements; 5664 lists of 5 elements.
# this means the number of elements: (1 * 2) + (3 * 7) + (5 * 5664) = 28343 

# This gets the names of the list; then works out the number of characters
# And then works out the unique lengths (the answer is 16)
sapply(names(lst_reserved), nchar) %>% unique()
# Are all names unique...the answer is yes
names(lst_reserved) %>% unique() %>% length() == 5672
# So we have found out that the names(lst_reserved) is the primary key

# The names of the 5 elements are as follows:
# 1) offerTermCode  [character]
# 2) sku [character]
# 3) effective date [character]
# 4) priceDimensions [list of 2]
# 5) termDimensions [list of ??]

# subset the list such that only the items with 5 elements are returns
# legacy code not used but kept as it is prettty
lst_reserved_5 <- lst_reserved[sapply(lst_reserved , function(x) length(x) == 5)]


lst_names <- lapply(lst_reserved, function(x) names(x[[1]]))
# we want to see if the names for all items following this pattern and in this order.
vct_inner_list_names <- c("offerTermCode", "sku", "effectiveDate" , "priceDimensions", "termAttributes" )
all(sapply(lst_names, function(x) all(x == vct_inner_list_names)))

df_sku_offer_term_i <- data_frame(sku = vector(mode = "character"),   
                                offer_term = vector(mode = "character"), 
                                eff_date = vector(mode = "character"),
                                price_len = vector(mode = "numeric"), 
                                term_len = vector(mode = "numeric"), 
                                term_lease_len = vector(mode = "character"), 
                                term_purch_opt = vector(mode = "character")
                                )
# Iterate though all 5672 list items. 
# For each item go through all N items and get their names
# put everything into a list (should have a list of 28320 elements)


# make a copy
df_sku_offer_term_all <- df_sku_offer_term_i

for (i in 1:length(lst_reserved))  {
  lst_outer <- lst_reserved[[i]]
  for(i_inner in 1:length(lst_outer)) {
    # store in a temporary data frame
    df_sku_offer_term_i[1, "sku"] <- lst_outer[[i_inner]]$sku
    df_sku_offer_term_i[1, "offer_term"] <- lst_outer[[i_inner]]$offerTermCode 
    df_sku_offer_term_i[1, "eff_date"] <- lst_outer[[i_inner]]$effectiveDate
    df_sku_offer_term_i[1, "price_len"] <- length(lst_outer[[i_inner]]$priceDimensions)
    df_sku_offer_term_i[1, "term_lease_len"] <- lst_outer[[i_inner]]$termAttributes$LeaseContractLength
    df_sku_offer_term_i[1, "term_purch_opt"] <- lst_outer[[i_inner]]$termAttributes$PurchaseOption
    # append the temporary data frame to the master data frame
    df_sku_offer_term_all <- rbind(df_sku_offer_term_all, df_sku_offer_term_i)
  } # i_innner
  print(i)
}  # i

# post condition: we have a data_frame with all top level items
# "df_sku_offer_term_all"

# =========== Now we extract the price dimension ==========

lst_price_dim <- list()
length(lst_price_dim) <- nrow(df_sku_offer_term_all)
for (i in 1:nrow(df_sku_offer_term_all)) {
  str_sku <- df_sku_offer_term_all$"sku"[i]
  str_off <- df_sku_offer_term_all$"offer_term"[i]
  str_sku_off <- paste0(str_sku, ".", str_off)
  lst_price_dim_i <- get(str_sku, lst_reserved) %>%
    get(str_sku_off, .) %>%
    get("priceDimensions", .)
  lst_price_dim[[i]] <- lst_price_dim_i
  print(i)
} # for

# post condition: we have lst_price_dim with the priceDimensions for each offer_term
length(lst_price_dim) == nrow(df_sku_offer_term_all)



sapply(lst_price_dim , function(x) length(x)) %>% table()
# total price elements: (1 * 5671 ) + (2 * 22672) = 51015


# =================
#  Process lists with 2 price elements and those with a 
# a single price element differently 

lst_price_dim_2 <- lst_price_dim[sapply(lst_price_dim , function(x) length(x) == 2)]
lst_price_dim_1 <- lst_price_dim[sapply(lst_price_dim , function(x) length(x) == 1)]

fn_expand <- function(int_num) {
  # if the index is odd (i.e 1, 3, 5)
  if(int_num %% 2 == 1) {
    int_index <- int_num %/% 2 + 1
    return(lst_price_dim_2[[int_index]][[1]])
  }
  else {
    int_index <- int_num %/% 2 
    return(lst_price_dim_2[[int_index]][[2]])
  }
}


lst_dim_2 <- lapply(1:(length(lst_price_dim_2)*2), function(z) fn_expand(z))
lst_dim_1 <- lapply(seq_along(lst_price_dim_1), function(q) lst_price_dim_1[[q]][[1]])
# club the suckers together
lst_unrolled <- c(lst_dim_2, lst_dim_1)

# =================

# verify length
sapply(lst_price_dim, length) %>% sum() == length(lst_unrolled)

 
# Check that all pricePerUnits are length of 1
sapply(seq_along(lst_unrolled), function(x) 
  length(lst_unrolled[[x]]$pricePerUnit)) %>% unique() == 1

# check that the lengths are either 5 or 7
sapply(lst_unrolled, length) %>% table()


fn_create_df <- function(int_index)  {
  lst_i <- lst_unrolled[[int_index]]
  if (length(lst_i) == 7) {
    df_ret <- data.frame(
      rateCode = lst_i$rateCode,
      description = lst_i$description,
      unit = lst_i$unit,
      price_unit = names(lst_i$pricePerUnit),
      pricePerUnit = lst_i$pricePerUnit[[1]],
      lenAppliesTo = length(lst_i$appliesTo),
      beginRange = lst_i$beginRange,
      endRange = lst_i$endRange
    )
  } # if
    else {
      df_ret <- data.frame(
        rateCode = lst_i$rateCode,
        description = lst_i$description,
        unit = lst_i$unit,
        price_unit = names(lst_i$pricePerUnit), 
        pricePerUnit = lst_i$pricePerUnit[[1]],
        lenAppliesTo = length(lst_i$appliesTo),
        beginRange = "NULL",
        endRange = "NULL"
      )
    } # else
  return(df_ret)
} # function
  

# total price elements: (1 * 5671 ) + (2 * 22672) = 51015
# takes about 6 minutes (1 minute for the first line)
lst_comp <- lapply(seq_along(lst_unrolled), function(x) fn_create_df(x))
df_comp <-  do.call("rbind", lst_comp)
nrow(df_comp) == 51015

# get rid of lenAppliesTo as all zeros
df_comp$lenAppliesTo <- NULL

# helper function takes n characters from right
fn_substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

# ============================================

# split up rateCode into three chunks
df_comp$sku <- substr(df_comp$rateCode, 1, 16) 
df_comp$offer_cd <-  substr(df_comp$rateCode, 18, 27) 
df_comp$rate_cd <- substr(df_comp$rateCode, 29, 38)
df_comp$rateCode <- NULL
df_comp$lenAppliesTo <- NULL
vct_col_order <- c("sku","offer_cd","rate_cd" ,"unit","price_unit","pricePerUnit","description","beginRange","endRange")
df_comp <- df_comp[, vct_col_order]

# we have to join up df_comp and df_sku_offer_term_all together
# so the primary key of df_sku_offer_term_all is the first TWO fields
# it is 28343 rows
df_sku_offer_term_all %>% 
  dplyr::select(sku, offer_term) %>% 
      dplyr::distinct() %>% 
        dplyr::count() %>% .$n == nrow(df_sku_offer_term_all)

nrow(df_comp) == 51015

df_comp %>% select(sku, offer_cd) %>% distinct() %>% count() %>% .$n == 28343

df_sku_offer_term_all %>% 
  dplyr::select(sku, offer_term) %>% 
  dplyr::distinct() %>% 
  dplyr::count() %>% .$n == 28343

# okay we join the data frames up using sku and offer term.
df_sku_offer_term_all %>% View()
df_comp %>% View()

df_alles <- df_sku_offer_term_all %>% 
  inner_join(df_comp, by = c("sku" = "sku", "offer_term" = "offer_cd")) %>%
  select(sku, offer_term, rate_cd, eff_date, 
         price_len, term_len, term_lease_len, 
         term_purch_opt, unit, price_unit, 
         pricePerUnit, description, beginRange, endRange)

# ============================================

# We export the data.frame as a zipped object in the outputs directory
HOME <- getwd()
setwd(file.path(HOME, "outputs"))
gz_result <- gzfile("df_terms_reserved.csv.gz", "w")
write.csv(df_alles, gz_result)
close(gz_result)
setwd("..")



