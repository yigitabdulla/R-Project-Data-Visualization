#I'm writing this area so I can run it on my own computer
getwd()
setwd("C:/Users/Aidata/Desktop/")
getwd()
data<- read.csv('data.csv', header=TRUE, sep=";",dec=".")
View(data)


find_std_dev <- function(data, variables, factors) {
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
  
  
  
  my_mean <- function(x) {
    sum <- 0
    count <- 0
    for (element in x) {
      if (!is.na(element)) {
        sum <- sum + element
        count <- count + 1
      }
    }
    sum / count
  }
  
  my_sum <- function(x) {
    total <- 0
    for (element in x) {
      if (!is.na(element)) {
        total <- total + element
      }
    }
    total
  }
  
  my_std_dev <- function(x) {
    n <- my_length(x)
    mean_value <- my_mean(x)
    sum_squares <- my_sum((x - mean_value)^2)
    std_dev <- sqrt(sum_squares / (n - 1))
    std_dev
  }
  
  
  unique_factors <- my_unique(do.call("paste", data[, factors, drop = FALSE]))
  std_dev_values <- matrix(NA, my_length(unique_factors), my_length(variables))
  colnames(std_dev_values) <- variables
  
  for (i in 1:my_length(unique_factors)) {
    for (j in 1:my_length(variables)) {
      subset_data <- data[do.call("paste", data[, factors, drop = FALSE]) == unique_factors[i], variables[j]]
      std_dev_values[i, j] <- my_std_dev(subset_data)
    }
  }
  
  result <- data.frame(Factors = unique_factors, StandardDeviation = std_dev_values)
  return(result)
}




std_Group <- find_std_dev(data, c("Var1", "Var2"), c("Group"))
print(std_Group)

std_Gender <- find_std_dev(data, c("Var1", "Var2"), c("Gender"))
print(std_Gender)

std_Both <- find_std_dev(data, c("Var1", "Var2"), c("Gender", "Group"))
print(std_Both)
