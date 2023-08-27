#I'm writing this area so I can run it on my own computer
getwd()
setwd("C:/Users/Aidata/Desktop/")
getwd()
data<- read.csv('data.csv', header=TRUE, sep=";",dec=".")
View(data)

find_variance <- function(data, variables, factors) {
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
  
  my_variance <- function(x) {
    n <- my_length(x)
    mean_value <- my_sum(x) / n
    sum_squares <- 0
    for (elem in x) {
      if (!is.na(elem)) {
        sum_squares <- sum_squares + (elem - mean_value)^2
      }
    }
    sum_squares / (n - 1)
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
  
  var_values <- matrix(NA, my_length(unique_factors), my_length(variables))
  colnames(var_values) <- variables
  
  for (i in 1:my_length(unique_factors)) {
    for (j in 1:my_length(variables)) {
      subset_data <- data[do.call("paste", data[, factors, drop = FALSE]) == unique_factors[i], variables[j]]
      var_values[i, j] <- my_variance(subset_data)
    }
  }
  
  result <- data.frame(Factors = unique_factors, Variance = var_values)
  return(result)
}


var_Group <- find_variance(data, c("Var1", "Var2"), c("Group"))
print(var_Group)

var_Gender <- find_variance(data, c("Var1", "Var2"), c("Gender"))
print(var_Gender)

var_Both <- find_variance(data, c("Var1", "Var2"), c("Gender", "Group"))
print(var_Both)





