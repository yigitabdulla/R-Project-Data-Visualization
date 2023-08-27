#Boxplots are a measure of how well distributed is the data in a data set.
#It divides the data set into three quartiles. This graph represents the minimum,
#maximum, median, first quartile and third quartile in the data set.

data1 = read.delim(file.choose(), header = TRUE)


my_boxplot <- function(x, y, xlab="", ylab="", main="", col="blue") {
  
  y <- na.omit(y)
  y <- sub(",", ".", y)
  y <- as.numeric(y)
  
  n <- length(y)
  
  if (n > 0) {
    min_val <- y[1]
    for (i in 2:n) {
      if (!is.na(y[i]) && y[i] < min_val) {
        min_val <- y[i]
      }
    }
  } else {
    min_val <- NA
  }
  
  if (n > 0) {
    max_val <- y[1]
    for (i in 2:n) {
      if (!is.na(y[i]) && y[i] > max_val) {
        max_val <- y[i]
      }
    }
  } else {
    max_val <- NA
  }
  
  if (n > 0) {
    if (n %% 2 == 0) {
      median_val <- (y[n/2] + y[(n/2)+1]) / 2
    } else {
      median_val <- y[(n+1)/2]
    }
  } else {
    median_val <- NA
  }
  
  max_y <- max_val
  min_y <- min_val
  q1 <- quantile(y, 0.25)
  median_y <- median_val
  q3 <- quantile(y, 0.75)
  
  plot_window <- c(0, length(x)+1, min_y-0.1*(max_y-min_y), max_y+0.1*(max_y-min_y))
  
  plot.new()
  
  par(mar=c(5, 4, 4, 2) + 0.1, xpd=TRUE)
  
  plot_window <- c(0, length(x)+1, min_y-0.1*(max_y-min_y), max_y+0.1*(max_y-min_y))
  plot.window(plot_window, xlim=c(0,length(x)+1), ylim=c(min_y-0.1*(max_y-min_y),max_y+0.1*(max_y-min_y)), xaxs="i", yaxs="i", xlab="", ylab="",log = "")
  
  axis(1, at=1:length(x), labels=FALSE, tick=FALSE)
  mtext(xlab, side=1, line=2.5)
  for (i in 1:length(x)) {
    text(i+0.27, par("usr")[3]-1.5, labels=x[i], srt=0, adj=1, xpd=TRUE)
  }
  
  axis(2, at=seq(min_y,max_y, by=(max_y-min_y)/10), las=3, tick=TRUE)
  mtext(ylab, side=2, line=2.5)
  
  rect(xleft=1-0.2, ybottom=q1, xright=1+0.2, ytop=q3, col=col, border="black")
  segments(x0=1-0.2, y0=median_y, x1=1+0.2, y1=median_y)
  segments(x0=1, y0=q1, x1=1, y1=min(median_y,q1), lty=1, col="black")
  segments(x0=1, y0=q3, x1=1, y1=max(median_y,q3), lty=1, col="black")
  
  title(main=main)
}

par(mfrow=c(2,2))

my_boxplot("Group 1", data1$Var1, xlab="Group", ylab="Values", main="Boxplot for Group 1", col="blue")
my_boxplot("Group 2", data1$Var2, xlab="Group", ylab="Values", main="Boxplot for Group 2", col="green")
my_boxplot("Group 3", data1$Var3, xlab="Group", ylab="Values", main="Boxplot for Group 3", col="yellow")
my_boxplot("Group 4", data1$Var2, xlab="Group", ylab="Values", main="Boxplot for Group 4", col="red")

