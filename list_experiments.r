

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


lst_test <- list(a)

df_test <- data.frame(cola = c("mark,", "fergus", "ben"), 
                      colb = c("orange", "banana", "apple"), 
                      colc = c("pink", "blue", "green"),
                      cold = c("soft", "hard", "silky"))

vct_test <- vector(mode = "character")
fn_create_vct <- function(int_col) {
  
}

sapply(seq_along(df_test), function(x) c(vct_test, df_test[, x]))








