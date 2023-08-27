#I'm writing this area so I can run it on my own computer
getwd()
setwd("C:/Users/Aidata/Desktop/")
getwd()
data<- read.csv('data.csv', header=TRUE, sep=";",dec=".")
View(data)

find_observation <- function(data, variables, factors) {
  
  
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
  
  my_observation_count <- function(x) {
    count <- 0
    for (elem in x) {
      count <- count + 1
    }
    count
  }
  
  
  
  unique_factors <- my_unique(do.call("paste", data[, factors, drop = FALSE]))
  unique_variables <- variables
  observation_counts <- matrix(NA, my_length(unique_factors), my_length(unique_variables))
  colnames(observation_counts) <- unique_variables
  
  for (i in 1:my_length(unique_factors)) {
    for (j in 1:my_length(unique_variables)) {
      observation_counts[i, j] <- my_observation_count(data[do.call("paste", data[, factors, drop = FALSE]) == unique_factors[i], unique_variables[j]])
    }
  }
  
  result <- data.frame(Factors = unique_factors,  ObservationNum = observation_counts)
  return(result)
}

observ_Group <- find_observation(data, c("Var1", "Var2"), c("Group"))
print(observ_Group)

observ_Gender <- find_observation(data, c("Var1", "Var2"), c("Gender"))
print(observ_Gender)

observ_Both <- find_observation(data, c("Var1", "Var2"), c("Gender", "Group"))
print(observ_Both)

