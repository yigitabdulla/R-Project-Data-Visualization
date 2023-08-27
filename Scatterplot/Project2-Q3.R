#I'm writing this area so I can run it on my own computer
getwd()
setwd("C:/Users/Aidata/Desktop/")
getwd()
data<- read.csv('data.csv', header=TRUE, sep=";",dec=".")
View(data)

library(graphics)


my_plot <- function(x, y, xlab = "", ylab = "", main = "Scatterplot") {
  plot_window <- par("usr")
  
  valid_indices <- is.finite(x) & is.finite(y)
  x_valid <- x[valid_indices]
  y_valid <- y[valid_indices]
  
  plot(x_valid, y_valid, xlab = xlab, ylab = ylab, main = main)
  
  abline(h = 0, col = "gray", lty = "dashed")
  abline(v = 0, col = "gray", lty = "dashed")
  
  points(x_valid, y_valid, pch = 16)
  
  text(plot_window[2], par("usr")[3], xlab, pos = 4)
  text(par("usr")[1], plot_window[4], ylab, pos = 2)
}

draw_scatterplot <- function(data, x, y) {
  if (x %in% colnames(data) && y %in% colnames(data)) {
    data[[x]] <- as.numeric(gsub(",", ".", data[[x]]))
    data[[y]] <- as.numeric(gsub(",", ".", data[[y]]))
    
    my_plot(data[[x]], data[[y]], xlab = x, ylab = y, main = "My Scatterplot")
  } else {
    print("Wrong variable.")
  }
}

draw_scatterplot_matrix <- function(data, x, y) {
  if (x %in% colnames(data) && y %in% colnames(data)) {
    data[[x]] <- as.numeric(gsub(",", ".", data[[x]]))
    data[[y]] <- as.numeric(gsub(",", ".", data[[y]]))
    
    num_vars <- c(x, y)
    pairs(data[, num_vars], main = "Scatterplot Matrix")
  } else {
    print("Use Correct Variable.")
  }
}

draw_scatterplot(data, "Var1", "Var2")
draw_scatterplot_matrix(data, "Var1", "Var2")