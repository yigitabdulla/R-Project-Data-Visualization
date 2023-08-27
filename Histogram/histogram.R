data1 = read.delim(file.choose(), header = TRUE)

hist_function <- function(data, varname, nbins = 10, main = "", xlab = "", ylab = "", 
                   xlim = NULL, ylim = NULL, col = "blue") {
  
  # we are getting variable. We have NA values so we have to consider the NA values!
  var <- na.omit(data[[varname]])
  var <- sub(",", ".", var)
  var <- as.numeric(var)
  #print(var)
  
  n <- length(var)
  
  if (n > 0) {
    min_val <- var[1]
    for (i in 2:n) {
      if (!is.na(var[i]) && var[i] < min_val) {
        min_val <- var[i]
      }
    }
  } else {
    min_val <- NA
  }
  
  if (n > 0) {
    max_val <- var[1]
    for (i in 2:n) {
      if (!is.na(var[i]) && var[i] > max_val) {
        max_val <- var[i]
      }
    }
  } else {
    max_val <- NA
  }
  
  # We are creating breaks for histogram
  breaks <- seq(min_val, max_val, length.out = nbins + 1)
  
  # Compute histogram counts
  counts <- rep(0, nbins)
  for (i in 1:nbins) {
    counts[i] <- sum(var >= breaks[i] & var < breaks[i+1])
  }
  
  # Histogram heigths and ylim coordinates
  heights <- counts / diff(breaks)
  ylim = c(0, max(heights))
  
  # Setting plot area
  par(mar = c(6, 5, 3.9, 1.9))
  plot.new()
  plot_window <- c(min(breaks), max(breaks), 0, max(heights) * 1.1)
  plot.window(xlim = plot_window[1:2], ylim = plot_window[3:4])
  
  # Drawing rectangles for histogram
  for (i in 1:nbins) {
    rect(breaks[i], 0, breaks[i+1], heights[i], col = col)
  }
  
  # Add axis labels
  axis(side = 1, at = breaks, labels = breaks)
  axis(side = 2)
  
  # Adding title
  if (main != "") {
    mtext(main, side = 3, line = 1)
  }
  
  # Adding labels
  if (xlab != "") {
    mtext(xlab, side = 1, line = 2.5)
  }
  if (ylab != "") {
    mtext(ylab, side = 2, line = 2.5)
  }
  
}



# we split the plot are for 4 histogram
par(mfrow = c(2, 2))

hist_function(data1, "Var1", nbins = 10, main = "Var1 status", 
       xlab = "Var1", ylab = "Frequency", xlim = c(3,5), col = "yellow")

hist_function(data1, "Var2", nbins = 10, main = "Var2 status", 
       xlab = "Var2", ylab = "Frequency", xlim = c(18,22), col = "red")

hist_function(data1, "Var3", nbins = 10, main = "Var3 status", 
       xlab = "Var3", ylab = "Frequency", xlim = c(50,60))

hist_function(data1, "Var4", nbins = 10, main = "Var4 status", 
       xlab = "Var4", ylab = "Frequency", xlim = c(65,80), col = "purple")

