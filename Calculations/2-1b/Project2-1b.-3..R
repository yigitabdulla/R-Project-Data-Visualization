#I'm writing this area so I can run it on my own computer
getwd()
setwd("C:/Users/Aidata/Desktop/")
getwd()
data<- read.csv('data.csv', header=TRUE, sep=";",dec=".")
View(data)


calculate_correlation <- function(x, y) {
  my_length <- function(x) {
    count <- 0
    for (element in x) {
      count <- count + 1
    }
    return(count)
  }
  
  my_sum <- function(x) {
    total <- 0
    for (element in x) {
      if (!is.na(element)) {
        total <- total + element
      }
    }
    return(total)
  }
  
  if (my_length(x) != my_length(y)) {
    stop("Input vectors must have the same length.")
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
    
    if (count == 0) {
      stop("Input vector must not be empty or contain only NA values.")
    }
    
    mean_value <- sum / count
    return(mean_value)
  }
  
  n <- my_length(x)
  
  mean_x <- my_mean(x)
  mean_y <- my_mean(y)
  
  numerator <- my_sum((x - mean_x) * (y - mean_y))
  denominator <- sqrt(my_sum((x - mean_x)^2) * my_sum((y - mean_y)^2))
  
  correlation_value <- numerator / denominator
  
  return(correlation_value)
}

find_correlation <- function(data, variables) {
  for (variable in variables) {
    data[[variable]] <- gsub(",", ".", data[[variable]])
    data[[variable]] <- as.numeric(data[[variable]])
  }
  
  correlation_values <- matrix(NA, 1, 1)
  colnames(correlation_values) <- "Correlation"
  
  subset_data <- data[, variables]
  correlation_values[1, 1] <- calculate_correlation(subset_data[, 1], subset_data[, 2])
  
  result <- data.frame(Correlation = correlation_values)
  return(result)
}

corr <- find_correlation(data, c("Var1", "Var2"))
print(corr)