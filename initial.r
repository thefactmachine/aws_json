

# clear the decks
rm(list=ls()) 
options(stringsAsFactors=FALSE)
library(jsonlite)
library(dplyr)


# json_file_name <- "index_offerings.json"

# read the json file amd parse it into an R list
json_file_name <- "inputs/index_pricing.json"
char_json_file <- readChar(json_file_name, file.info(json_file_name)$size)
lst_pricing <- jsonlite::fromJSON(char_json_file)

# print the names of the list to screen. There are seven.
names(lst_pricing)
names(lst_pricing) %>% length()


# print the size of the object in megabytes (221.29 Mb)
object.size(lst_pricing) / 1e06

# Of the 7 elements, only 2 are complex (i.e. non-primitive) objects.

# this one (i.e products is 50 Mb)
object.size(lst_pricing$products) / 1e06

# this one (i.e terms is 172.14 Mb)
object.size(lst_pricing$terms) / 1e06


# =====================================================================
lst_products <- lst_pricing$products
# length of this is 10,777 (character vector)
vct_names_products <- names(lst_products)

# are the names of the products unique..? (YES)
(unique(vct_names_products) %>% length()) == length(vct_names_products)

# test whether the length of object name is always 16 characters (yes it is)
all(nchar(vct_names_products) == 16)

# there are 11 unique product families - this is a vector of them
vct_unique_prod_family <- sapply(lst_products, function(x) {x$productFamily}) %>% 
                            unique()

# lets do a count of each of the product families
df_prod_family_count <- sapply(lst_products, function(x) {x$productFamily}) %>% 
                      table() %>% as.data.frame() 

names(df_prod_family_count) <- c("prod_family", "count")

# integrity check
sum(df_prod_family_count$count) == length(lst_products)

# =====================================================================

# we create a set of logical vectors stored in a list. 
# this will enable us to easily extract a specific productFamily
fn_filter <- function(str_name, lst_lcl_prod) { 
  vct_logical <- sapply(seq_along(lst_lcl_prod), 
                        function(x) lst_lcl_prod[[x]][2] == str_name)
  names(vct_logical) <- NULL
  return(vct_logical)
}

# give names to the vector...works better with lapply
names(vct_unique_prod_family) <- vct_unique_prod_family

# this gives a list of number("productFamily") elements. 
# Each element is a logical vector of length(products)
lst_logical <- lapply(vct_unique_prod_family, function(x) fn_filter(x, lst_products))

# =====================================================================


fn_get_attributes <- function(a_list, str_name) {
  
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

HOME <- getwd()

setwd(file.path(HOME, "outputs"))

gz_result <- gzfile("df_result.csv.gz", "w")
write.csv(df_result, gz_result)
close(gz_result)
setwd("..")



nrow(df_result)








# 1) Iterate through logical...pull out the vector and its name
# 2) Filter the lst_products according to the current logcal value
# 3) Get the attribute names and their values



# CRAP CRAP CRAP CRAP CRAP CRAP CRAP CRAP CRAP CRAP CRAP CRAP CRAP
# Delete the following after review.

# CRAP CRAP CRAP CRAP CRAP CRAP CRAP CRAP CRAP CRAP CRAP CRAP CRAP




lst_storage <- lst_products[lst_logical$`Compute Instance`]

df_attribute_count <- sapply(lst_storage, function(x) length(x$attributes)) %>% 
                      table() %>% as.data.frame()

names(df_attribute_count) <- c("num_attributes", "count")

# get rid of the factor
df_attribute_count$num_attributes <- as.numeric(df_attribute_count$num_attributes)

# check table integrity
sum(df_attribute_count$count) == 10149

# total unduplicated attributes
sum(df_attribute_count$num_attributes * df_attribute_count$count) == 47721










df_xxx <- unlist(xxx, recursive = FALSE)
head(df_xxx)















