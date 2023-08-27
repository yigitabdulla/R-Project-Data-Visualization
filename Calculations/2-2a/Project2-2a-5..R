#I'm writing this area so I can run it on my own computer
getwd()
setwd("C:/Users/Aidata/Desktop/")
getwd()
data<- read.csv('data.csv', header=TRUE, sep=";",dec=".")
View(data)

find_sum <- function(data, variables, factors) {
  for (variable in variables) {
    data[[variable]] <- gsub(",", ".", data[[variable]])
    data[[variable]] <- as.numeric(data[[variable]])
  }
  
  my_length <- function(x) {
    count <- 0
    for (elem in x) {
      count <- count + 1
    }
    count
  }
  
  my_unique <- function(x) {
    unique_values <- vector()
    for (elem in x) {
      if (!(elem %in% unique_values)) {
        unique_values <- c(unique_values, elem)
      }
    }
    unique_values
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
  
  unique_factors <- my_unique(do.call("paste", data[, factors, drop = FALSE]))
  sum_values <- matrix(NA, my_length(unique_factors), my_length(variables))
  colnames(sum_values) <- variables
  
  for (i in 1:my_length(unique_factors)) {
    for (j in 1:my_length(variables)) {
      subset_data <- data[do.call("paste", data[, factors, drop = FALSE]) == unique_factors[i], variables[j]]
      sum_values[i, j] <- my_sum(subset_data)
    }
  }
  
  result <- data.frame(Factors = unique_factors, Sum = sum_values)
  return(result)
}

sum_Group <- find_sum(data, c("Var1", "Var2"), c("Group"))
print(sum_Group)

sum_Gender <- find_sum(data, c("Var1", "Var2"), c("Gender"))
print(sum_Gender)

sum_Both <- find_sum(data, c("Var1", "Var2"), c("Gender", "Group"))
print(sum_Both)