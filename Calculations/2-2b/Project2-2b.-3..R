#I'm writing this area so I can run it on my own computer
getwd()
setwd("C:/Users/Aidata/Desktop/")
getwd()
data<- read.csv('data.csv', header=TRUE, sep=";",dec=".")
View(data)

find_correlation <- function(data, variables, factors) {
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
  my_mean <- function(x) {
    sum_value <- 0
    count <- 0
    for (elem in x) {
      if (!is.na(elem)) {
        sum_value <- sum_value + elem
        count <- count + 1
      }
    }
    mean_value <- sum_value / count
    mean_value
  }
  
  my_correlation <- function(x, y) {
    n <- length(x)
    mean_x <- my_mean(x)
    mean_y <- my_mean(y)
    sum_product <- my_sum((x - mean_x) * (y - mean_y))
    sum_sq_x <- my_sum((x - mean_x)^2)
    sum_sq_y <- my_sum((y - mean_y)^2)
    correlation <- sum_product / sqrt(sum_sq_x * sum_sq_y)
    correlation
  }
  
  unique_factors <- my_unique(do.call("paste", data[, factors, drop = FALSE]))
  correlation_values <- matrix(NA, my_length(unique_factors), 1)
  colnames(correlation_values) <- "Correlation"
  
  for (i in 1:my_length(unique_factors)) {
    subset_data <- data[do.call("paste", data[, factors, drop = FALSE]) == unique_factors[i], variables]
    correlation_values[i, ] <- my_correlation(subset_data[, 1], subset_data[, 2])
  }
  
  result <- data.frame(Factors = unique_factors, Correlation = correlation_values)
  return(result)
}




corr_Group <- find_correlation(data, c("Var1", "Var2"), c("Group"))
print(corr_Group)

corr_Gender <- find_correlation(data, c("Var1", "Var2"), c("Gender"))
print(corr_Gender)

corr_Both <- find_correlation(data, c("Var1", "Var2"), c("Gender", "Group"))
print(corr_Both)