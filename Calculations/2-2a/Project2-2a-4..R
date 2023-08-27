#I'm writing this area so I can run it on my own computer
getwd()
setwd("C:/Users/Aidata/Desktop/")
getwd()
data<- read.csv('data.csv', header=TRUE, sep=";",dec=".")
View(data)

find_range <- function(data, variables, factors) {
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
  
  my_range <- function(x) {
    min_value <- Inf
    max_value <- -Inf
    for (elem in x) {
      if (!is.na(elem)) {
        if (elem < min_value) {
          min_value <- elem
        }
        if (elem > max_value) {
          max_value <- elem
        }
      }
    }
    paste(min_value, max_value, sep = " - ")
  }
  
  unique_factors <- my_unique(do.call("paste", data[, factors, drop = FALSE]))
  range_values <- matrix(NA, my_length(unique_factors), my_length(variables))
  colnames(range_values) <- variables
  
  for (i in 1:my_length(unique_factors)) {
    for (j in 1:my_length(variables)) {
      subset_data <- data[do.call("paste", data[, factors, drop = FALSE]) == unique_factors[i], variables[j]]
      range_values[i, j] <- my_range(subset_data)
    }
  }
  
  result <- data.frame(Factors = unique_factors, Range = range_values)
  return(result)
}


range_Group <- find_range(data, c("Var1", "Var2"), c("Group"))
print(range_Group)

range_Gender <- find_range(data, c("Var1", "Var2"), c("Gender"))
print(range_Gender)

range_Both <- find_range(data, c("Var1", "Var2"), c("Gender", "Group"))
print(range_Both)


