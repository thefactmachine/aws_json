

nest <- list(girls = c("susan", "fiona", "louise"), boys = c("iain", "david"))
lst <- list(a = 1:5, b = 10:19, another_list = nest)

class(lst[[3]])  == "list"
class(lst[3])  == "list"

# you dont need the list name
double <- lst[[3]]
double$girls

# this does the same thing
lst[[3]][[1]]


# you need to include the list name
single <- lst[3]
single$another_list$girls

lst[[3]][[1]]
lst[[3]][[2]]

lst[[3]][[1]][1:2]


test_param <- "another_list"
lst$test_param
get(test_param, lst)



a <- 1:10


"rateCode" ,"description" ,"unit" ,"pricePerUnit", "appliesTo"   
[1] "rateCode"     "description"  "beginRange"   "endRange"     "unit"         "pricePerUnit" "appliesTo"   




