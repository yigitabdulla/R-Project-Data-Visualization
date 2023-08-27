data1 = read.delim(file.choose(), header = TRUE)

statistics <- function(x) {
  
  na.omit(x)
  x <- sub(",", ".", x)
  x <- as.numeric(x)
  
  n <- length(x)
  
  if (n > 0) {
    min_val <- x[1]
    for (i in 2:n) {
      if (!is.na(x[i]) && x[i] < min_val) {
        min_val <- x[i]
      }
    }
  } else {
    min_val <- NA
  }
  
  if (n > 0) {
    max_val <- x[1]
    for (i in 2:n) {
      if (!is.na(x[i]) && x[i] > max_val) {
        max_val <- x[i]
      }
    }
  } else {
    max_val <- NA
  }
  
  if (n > 0) {
    range_val <- max_val - min_val
  } else {
    range_val <- NA
  }
  
  
  sum_val <- 0
  for (i in 1:length(x)) {
    if (!is.na(x[i])) {
      sum_val <- sum_val + x[i]
    }
  }
  
  if (n > 0) {
    mean_val <- sum_val / n
  } else {
    mean_val <- NA
  }
  
  if (n > 0) {
    if (n %% 2 == 0) {
      median_val <- (x[n/2] + x[(n/2)+1]) / 2
    } else {
      median_val <- x[(n+1)/2]
    }
  } else {
    median_val <- NA
  }
  
  if (n > 1) {
    ss <- 0
    for (i in 1:n) {
      if (!is.na(x[i])) {
        ss <- ss + (x[i] - mean_val)^2
      }
    }
  }
  
  if (n > 1) {
    var_val <- ss / (n - 1)
  } else {
    var_val <- NA
  }
  
  if (n > 1) {
    sd_val <- (var_val)^(1/2)
  } else {
    sd_val <- NA
  }
  
  # Print the results
  cat("Number of observations:", n, "\n")
  cat("Minimum:", min_val, "\n")
  cat("Maximum:", max_val, "\n")
  cat("Range:", range_val, "\n")
  cat("Sum:", sum_val, "\n")
  cat("Mean:", mean_val, "\n")
  cat("Median:", median_val, "\n")
  cat("Sum of squares:", ss, "\n")
  cat("Variance:", var_val, "\n")
  cat("Standard deviation:", sd_val, "\n")
}

x <- data1$Var1
statistics(x)