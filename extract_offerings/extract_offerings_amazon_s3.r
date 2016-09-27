# clear the decks
rm(list=ls()) 
options(stringsAsFactors=FALSE)
library(jsonlite)
library(dplyr)

load("inputs/binary/offerings.Rdata")

s3 <- lst_current$AmazonS3

# products and terms are the only complex objects
s3_products <- s3$products
s3_terms <- s3$terms

length(s3_products) == 1221
names(s3_products)[1] %>% nchar() == 16

sapply(s3_products[[1]]$attributes, function(x) class(x))
# attributes contains simple objects only =====  END OF ATTRIBUTES
# ===== END OF PRODUCTS ==========================================

# ======= START ON TERMS =========================================

# so in terms there is only 1 object ...a list this list has 1221 elements
# each of the element is a sku
length(s3_terms$OnDemand) == 1221


# check the length of each item .... each item has a length of 1
sapply(s3_terms$OnDemand, function(x) length(x)) %>% table()

# goto the next level down .... everything is 5 elements long
sapply(s3_terms$OnDemand, function(x) length(x[[1]])) %>% table()

# now go down to price dimensions
sapply(s3_terms$OnDemand, function(x) length(x[[1]]$priceDimensions)) %>% table()
# we have here: (1185 * 1) + (4 * 2) + (5 * 10) + (6 * 24) = 1387


# setting recursive to FALSE unrolls to the first level only
lst_flat_price <- lapply(s3_terms$OnDemand, function(x) 
                    lapply(x[[1]]$priceDimensions, function(y) y)) %>% 
                      unlist(., recursive = FALSE)

# naming of lst_flat_price
# "MQ4SWZ54XKFN5QMV.MQ4SWZ54XKFN5QMV.JRTCKXETXF.6YS6EN2CT7"
# sku (16) .(1) sku (16) .(1) offerTermCode (10) .(1) rateCode (10)
nchar("MQ4SWZ54XKFN5QMV.MQ4SWZ54XKFN5QMV.JRTCKXETXF.6YS6EN2CT7") == 
  16 + 1 + 16 + 1 + 10 + 1 + 10

# Assert that the length is okay
length(lst_flat_price) == 
  sapply(s3_terms$OnDemand, function(x) length(x[[1]]$priceDimensions)) %>% sum()

# here are the fields. The last two are lists
# "rateCode", "description", "beginRange", "endRange", "unit", "pricePerUnit","appliesTo"   

# check that the pricePerUnit is always length 1.  It is.
sapply(lst_flat_price, function(x) length(x$pricePerUnit)) %>% table()

# check that "appliesTo" is always length 0 .. it is
sapply(lst_flat_price, function(x) length(x$appliesTo)) %>% table()

# create a series of sapply calls which will create vectors.
vct_rate_code <- sapply(lst_flat_price, function(x) x$rateCode)
vct_description <- sapply(lst_flat_price, function(x) x$description)
vct_begin_range <- sapply(lst_flat_price, function(x) x$beginRange)
vct_end_range <-  sapply(lst_flat_price, function(x) x$endRange)
vct_unit <- sapply(lst_flat_price, function(x) x$unit)
vct_ccy <- sapply(lst_flat_price, function(x) names(x$pricePerUnit)[1])
vct_amount <- sapply(lst_flat_price, function(x) x$pricePerUnit[[1]])
names(vct_amount) <- NULL


# split up rate code into its various pieces
vct_sku_d <- substr(vct_rate_code, 1, 16) 
names(vct_sku_d) <- NULL

vct_offer_term <- substr(vct_rate_code, 18, 27)
vct_rate_cd_d <- substr(vct_rate_code, 29, 38)

# lassen wir build the data frame
df_s3_pd <- data.frame (sku = vct_sku_d, 
                        rate_cd = vct_rate_cd_d,
                        offer_term = vct_offer_term,
                        des = vct_description,
                        b_range = vct_begin_range,
                        e_range = vct_end_range,
                        unit = vct_unit,
                        ccy = vct_ccy,
                        amount = vct_amount,
                        row.names = NULL)

# END OF PROCESSING FOR TERMS =======================================

# ====NOW PRODUCTS ======================================================
names(s3_products) %>% unique() %>% length() == 1221
names(s3_products) %>% length() == 1221

# all have a length of three: sku (primitive), product_family(primitive), attributes (list)
sapply(s3_products, function(x) length(x)) %>% table()

# these are the attribute values for 8
# "servicecode" ,"transferType" ,"fromLocation" ,"fromLocationType" "toLocation" "toLocationType" "usagetype" "operation" 
sapply(s3_products, function(x) length(x$attributes)) %>% table()
# (7 * 135) + (8 * 1041) + (9 * 45) = 9678

# mmm so the attributes list can have duplicate values
lst_9 <- s3_products[sapply(s3_products, function(x) length(x$attributes)) == 9]

lapply(lst_9, function(x) names(x$attributes))
# "servicecode"  "location"     "locationType" "availability" "storageClass" "volumeType"   "usagetype"    "operation"    "durability" 

lst_7 <- s3_products[sapply(s3_products, function(x) length(x$attributes)) == 7]
lapply(lst_7, function(x) names(x$attributes))

# get a vector of all attribute names
vct_col_names <- vector(mode = "character")
for (i in 1:length(s3_products)) {
  vct_col_names <- c(vct_col_names, names(s3_products[[i]]$attributes))
}
vct_col_names <- unique(vct_col_names)

#=========================
# set up a template data.frame
df_attributes <- data.frame(matrix(nrow = length(s3_products), 
                    ncol = 2 + length(vct_col_names)))
names(df_attributes) <- c("sku", "prod_family", vct_col_names)
# add in the primary key
df_attributes$sku <- names(s3_products)

# NOW WE ITERATE THROUGH THE LIST OR THROUGH THE DATA FRAME AND POPULATE THE DATA FRAME WITH VALUES
for (i_row in (1:nrow(df_attributes))) {
  lst_current <- get(df_attributes[i_row, 1], s3_products)
  df_attributes[i_row, "prod_family"] <- lst_current$productFamily
  lst_current <- get(df_attributes[i_row, 1], s3_products)
  lst_curr_attributes <- lst_current$attributes
  for (i_attr in (1:length(lst_curr_attributes))) {
    print(i_attr)
    str_curr_value <- lst_curr_attributes[[i_attr]]
    str_col_name <- names(lst_curr_attributes[i_attr])
    df_attributes[i_row, str_col_name] <-  str_curr_value
  } # attribute
} # row

# ==========================================================
# rename so that the original JSON naming convention is preserved
df_products <- df_attributes
df_terms <- df_s3_pd

# products has 1221 rows
# terms has 1387 rows

# lets check the cardinality between the two
df_terms$sku %>% unique() %>% length() == df_products$sku %>% unique() %>% length()
all(df_s3_pd$sku %>% unique() %in% df_attributes$sku)

# 1 product usually has more than 1 term but sometimes has many
# lets look at the primary key for df_terms
df_terms %>% select(sku) %>% distinct() %>% count() %>% .$n

# this is the primary key: sku + rate_cd
df_terms %>% select(sku, rate_cd) %>% distinct() %>% count() %>% .$n

# only 1 value for offer term
unique(df_terms$offer_term)

# many of the columns in products are sparsely populated
# which ones are not

# We can drop the operation column
if (unique(df_products$operation) == "") df_products$operation <- NULL

# This is what is returned:  # "Data Transfer", "API Request", "Fee" ,"Storage" 
#  "API Request", "Fee" ,"Storage" .. 
# are only associated with service_code = AmazonS3 (180 items)
unique(df_products$prod_family)
unique(df_products$servicecode)

df_products %>% group_by(prod_family, servicecode) %>% summarise(count = n())

# lets see what columns are associated with which:
df_tx <- df_products %>% filter(servicecode == "AWSDataTransfer") 

# this little gem converts the data.frame to a matrix
# then converts the matrix to a vector
# from columns 1:9 there are NO NA's
c(as.matrix(df_tx[,1:9])) %>% is.na() %>% any()

# then from columns 10:19 .. ALL are NAs
c(as.matrix(df_tx[,10:19])) %>% is.na() %>% all()

# this will be one of the final tables
df_data_transfer <- df_tx[, 1:9]
nrow(df_data_transfer) == 1041
# =======================================================


# lets see what columns are associated with which:
df_api <- df_products %>% filter(prod_family  == "API Request") 
# set up data columns
vct_data <- c(1:3, 9:13)
vct_no_data <- c(4:8, 14:19)
# confirm that nothing got left out
all(sort(c(vct_data, vct_no_data)) == 1:19)

# expecting false
c(as.matrix(df_api[,vct_data])) %>% is.na() %>% any()

# expecting true
c(as.matrix(df_api[,vct_no_data])) %>% is.na() %>% all()

# final table
df_data_api <- df_api[, vct_data]

nrow(df_data_api) == 81

# ===========================================================
df_fee <- df_products %>% filter(prod_family  == "Fee") 
#df_fee %>% View()
vct_data <- c(1:3, 9:11, 14:15)
vct_no_data <- c(4:8, 12:13, 16:19)
# confirm that nothing got left out
all(sort(c(vct_data, vct_no_data)) == 1:19)
# expecting false
c(as.matrix(df_fee[,vct_data])) %>% is.na() %>% any() == FALSE
# expecting true
c(as.matrix(df_fee[,vct_no_data])) %>% is.na() %>% all() == TRUE
# final table
df_data_fee <- df_fee[, vct_data]
nrow(df_data_fee) == 54

# ===========================================================
df_storage <- df_products %>% filter(prod_family  == "Storage") 

vct_data <- c(1:3, 9:11, 16:19)
vct_no_data <- c(4:8, 12:15)
# confirm that nothing got left out
all(sort(c(vct_data, vct_no_data)) == 1:19)
# expecting false
c(as.matrix(df_storage[,vct_data])) %>% is.na() %>% any() == FALSE
# expecting true
c(as.matrix(df_storage[,vct_no_data])) %>% is.na() %>% all() == TRUE
# final table
df_data_storage <- df_storage[, vct_data]

nrow(df_data_storage) == 45

# ===============================================
# RECONCILE
# ===============================================
# We have partioned df_products
nrow(df_data_storage) + nrow(df_data_fee) + 
  nrow(df_data_api) + nrow(df_data_transfer) == nrow(df_products)

# Now we roll up everthing into a R binary object

save(df_data_storage, df_data_fee, df_data_api, 
     df_data_transfer, df_terms, file = "inputs/binary/offerings_amazon_s3.Rdata")

