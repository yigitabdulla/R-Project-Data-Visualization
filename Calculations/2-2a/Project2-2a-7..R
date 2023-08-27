#I'm writing this area so I can run it on my own computer
getwd()
setwd("C:/Users/Aidata/Desktop/")
getwd()
data<- read.csv('data.csv', header=TRUE, sep=";",dec=".")
View(data)

find_median <- function(data, variables, factors) {
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
  
  my_sort <- function(x) {
    n <- my_length(x)
    
    for (i in 1:(n - 1)) {
      for (j in 1:(n - i)) {
        if (!is.na(x[j]) && !is.na(x[j+1]) && x[j] > x[j+1]) {
          temp <- x[j]
          x[j] <- x[j+1]
          x[j+1] <- temp
        }
      }
    }
    
    x
  }
  
  
  my_median <- function(x) {
    n <- my_length(x)
    
    if (n %% 2 == 1) {
      median_value <- x[(n + 1) %/% 2]
    } else {
      sorted_data <- my_sort(x)
      median_value <- (sorted_data[n %/% 2] + sorted_data[(n %/% 2) + 1]) / 2
    }
    
    median_value
  }
  
  
  
  unique_factors <- unique(do.call("paste", data[, factors, drop = FALSE]))
  median_values <- matrix(NA, my_length(unique_factors), my_length(variables))
  colnames(median_values) <- variables
  
  for (i in 1:my_length(unique_factors)) {
    for (j in 1:my_length(variables)) {
      subset_data <- data[do.call("paste", data[, factors, drop = FALSE]) == unique_factors[i], variables[j]]
      median_values[i, j] <- my_median(subset_data)
    }
  }
  
  result <- data.frame(Factors = unique_factors, Median = median_values)
  return(result)
}

med_Group <- find_median(data, c("Var1", "Var2"), c("Group"))
print(med_Group)

med_Gender <- find_median(data, c("Var1", "Var2"), c("Gender"))
print(med_Gender)

med_Both <- find_median(data, c("Var1", "Var2"), c("Gender", "Group"))
print(med_Both)


