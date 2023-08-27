
#I'm writing this area so I can run it on my own computer
getwd()
setwd("C:/Users/Aidata/Desktop/")
getwd()
data<- read.csv('data.csv', header=TRUE, sep=";",dec=".")
View(data)


find_max <- function(data, variables, factors) {
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
  
  my_max <- function(x) {
    max_value <- -Inf
    for (elem in x) {
      if (!is.na(elem) && elem > max_value) {
        max_value <- elem
      }
    }
    max_value
  }
  
  unique_factors <- my_unique(do.call("paste", data[, factors, drop = FALSE]))
  max_values <- matrix(NA, my_length(unique_factors), my_length(variables))
  colnames(max_values) <- variables
  
  for (i in 1:my_length(unique_factors)) {
    for (j in 1:my_length(variables)) {
      subset_data <- data[do.call("paste", data[, factors, drop = FALSE]) == unique_factors[i], variables[j]]
      max_values[i, j] <- my_max(subset_data)
    }
  }
  
  result <- data.frame(Factors = unique_factors, Max = max_values)
  return(result)
}


max_Group <- find_max(data, c("Var1", "Var2"), c("Group"))
print(max_Group)

max_Gender <- find_max(data, c("Var1", "Var2"), c("Gender"))
print(max_Gender)

max_Both <- find_max(data, c("Var1", "Var2"), c("Gender", "Group"))
print(max_Both)







