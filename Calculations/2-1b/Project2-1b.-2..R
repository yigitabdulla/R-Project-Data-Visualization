#I'm writing this area so I can run it on my own computer
getwd()
setwd("C:/Users/Aidata/Desktop/")
getwd()
data<- read.csv('data.csv', header=TRUE, sep=";",dec=".")
View(data)


covariance <- function(data, variables) {
  
  subset_data <- data
  for (var in variables) { 
    subset_data[, var] <- gsub(",", ".", subset_data[, var])
    subset_data <- subset_data[complete.cases(subset_data[, var]), ] 
    subset_data[, var] <- as.numeric(subset_data[, var])  
  }
  
  my_mean <- function(x) {
    count <- my_sum(!is.na(x))
    
    if (count == 0) {
      return(NA)  
    }
    
    sum_value <- my_sum(x)
    mean_value <- sum_value / count
    return(mean_value)
  }
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
    total
  }
  
  
  x <- subset_data[, variables[1]] 
  y <- subset_data[, variables[2]] 

  
  n <- my_length(x)
  x_bar <- my_mean(x)
  y_bar <- my_mean(y)
  sum <- my_sum((x - x_bar) * (y - y_bar))
  covar <- sum / (n - 1)
  
  result <- data.frame(Covariance = covar)
  return(result)
}


cov <- covariance(data, c("Var1", "Var2"))
print(cov)



