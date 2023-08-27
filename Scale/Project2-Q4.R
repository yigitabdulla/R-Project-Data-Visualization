
#Im writing this area so I can run it on my own computer
getwd()
setwd("C:/Users/Aidata/Desktop/")
getwd()
data<- read.csv('data.csv', header=TRUE, sep=";",dec=".")
View(data)

custom_scale <- function(data, variables) {
  for (variable in variables) {
    data[[variable]] <- gsub(",", ".", data[[variable]])
    data[[variable]] <- as.numeric(data[[variable]])
  }
  
  my_mean <- function(x) {
    sum_value <- my_sum(x)
    count <- my_sum(!is.na(x))
    mean_value <- sum_value / count
    mean_value
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
  
  my_length <- function(x) {
    count <- 0
    for (elem in x) {
      count <- count + 1
    }
    count
  }
  
  my_std_dev <- function(x) {
    n <- my_length(x)
    mean_value <- my_mean(x)
    sum_squares <- my_sum((x - mean_value)^2)
    std_dev <- sqrt(sum_squares / (n - 1))
    std_dev
  }
  
  scaled_data <- data
  for (col in variables) {
    col_values <- scaled_data[[col]]
    col_mean <- my_mean(col_values)
    col_sd <- my_std_dev(col_values)
    col_values[is.na(col_values)] <- col_mean
    scaled_data[[col]] <- (col_values - col_mean) / col_sd
  }
  
  return(scaled_data[, variables, drop = FALSE])
}


scaled_data <- custom_scale(data, c("Var1"))
print(scaled_data)

scaled_data1 <- custom_scale(data, c("Var2", "Var3"))
print(scaled_data1)
