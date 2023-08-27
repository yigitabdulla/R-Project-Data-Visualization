

#I'm writing this area so I can run it on my own computer
getwd()
setwd("C:/Users/Aidata/Desktop/")
getwd()
data<- read.csv('data.csv', header=TRUE, sep=";",dec=".")
View(data)

find_mean <- function(data, variables, factors) {
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
  
  unique_factors <- my_unique(do.call("paste", data[, factors, drop = FALSE]))
  mean_values <- matrix(NA, my_length(unique_factors), my_length(variables))
  colnames(mean_values) <- variables
  
  for (i in 1:my_length(unique_factors)) {
    for (j in 1:my_length(variables)) {
      subset_data <- data[do.call("paste", data[, factors, drop = FALSE]) == unique_factors[i], variables[j]]
      mean_values[i, j] <- my_mean(subset_data)
    }
  }
  
  result <- data.frame(Factors = unique_factors, Mean = mean_values)
  return(result)
}



mean_Group <- find_mean(data, c("Var1", "Var2"), c("Group"))
print(mean_Group)

mean_Gender <- find_mean(data, c("Var1", "Var2"), c("Gender"))
print(mean_Gender)

mean_Both <- find_mean(data, c("Var1", "Var2"), c("Gender", "Group"))
print(mean_Both)





