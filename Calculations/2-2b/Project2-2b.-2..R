#I'm writing this area so I can run it on my own computer
getwd()
setwd("C:/Users/Aidata/Desktop/")
getwd()
data<- read.csv('data.csv', header=TRUE, sep=";",dec=".")
View(data)


find_covariance <- function(data, variables, factors) {
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
  
  my_covariance <- function(x, y) {
    n <- length(x)
    mean_x <- my_mean(x)
    mean_y <- my_mean(y)
    sum_product <- my_sum((x - mean_x) * (y - mean_y))
    covariance <- sum_product / (n - 1)
    covariance
  }
  
  unique_factors <- my_unique(do.call("paste", data[, factors, drop = FALSE]))
  covariance_values <- matrix(NA, my_length(unique_factors), 1)
  colnames(covariance_values) <- "Covariance"
  
  for (i in 1:my_length(unique_factors)) {
    subset_data <- data[do.call("paste", data[, factors, drop = FALSE]) == unique_factors[i], variables]
    covariance_values[i, ] <- my_covariance(subset_data[, 1], subset_data[, 2])
  }
  
  result <- data.frame(Factors = unique_factors, Covariance = covariance_values)
  return(result)
}


cov_Group <- find_covariance(data, c("Var1", "Var2"), c("Group"))
print(cov_Group)

cov_Gender <- find_covariance(data, c("Var1", "Var2"), c("Gender"))
print(cov_Gender)

cov_Both <- find_covariance(data, c("Var1", "Var2"), c("Gender", "Group"))
print(cov_Both)