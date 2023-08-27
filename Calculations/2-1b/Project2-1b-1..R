#I'm writing this area so I can run it on my own computer
getwd()
setwd("C:/Users/Aidata/Desktop/")
getwd()
data<- read.csv('data.csv', header=TRUE, sep=";",dec=".")
View(data)


find_cross_product <- function(data, variables) {
  for (variable in variables) {
    data[[variable]] <- gsub(",", ".", data[[variable]])
    data[[variable]] <- as.numeric(data[[variable]])
  }
  my_sum <- function(x) {
    sum_value <- 0
    for (elem in x) {
      if (!is.na(elem)) {
        sum_value <- sum_value + elem
      }
    }
    sum_value
  }
  cross_product_values <- matrix(NA, 1, 1)
  colnames(cross_product_values) <- "CrossProduct"
  
  subset_data <- data[, variables]
  cross_product_values[1, 1] <- my_sum(subset_data[, 1] * subset_data[, 2])
  
  result <- data.frame(CrossProduct = cross_product_values)
  return(result)
}


crossProd<- find_cross_product(data,c("Var1","Var2"))
print(crossProd)



