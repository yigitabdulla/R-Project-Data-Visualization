
#I'm writing this area so I can run it on my own computer
getwd()
setwd("C:/Users/Aidata/Desktop/")
getwd()
data<- read.csv('data.csv', header=TRUE, sep=";",dec=".")
View(data)

find_min <- function(data, variables, factors) {
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
  
  my_min <- function(x) {
    min_value <- Inf
    for (elem in x) {
      if (!is.na(elem) && elem < min_value) {
        min_value <- elem
      }
    }
    min_value
  }
  
  unique_factors <- my_unique(do.call("paste", data[, factors, drop = FALSE]))
  min_values <- matrix(NA, my_length(unique_factors), my_length(variables))
  colnames(min_values) <- variables
  
  for (i in 1:my_length(unique_factors)) {
    for (j in 1:my_length(variables)) {
      subset_data <- data[do.call("paste", data[, factors, drop = FALSE]) == unique_factors[i], variables[j]]
      min_values[i, j] <- my_min(subset_data)
    }
  }
  
  result <- data.frame(Factors = unique_factors, Min = min_values)
  return(result)
}

min_Group <- find_min(data, c("Var1", "Var2"), c("Group"))
print(min_Group)

min_Gender <- find_min(data, c("Var1", "Var2"), c("Gender"))
print(min_Gender)

min_Both <- find_min(data, c("Var1", "Var2"), c("Gender","Group"))
print(min_Both)




