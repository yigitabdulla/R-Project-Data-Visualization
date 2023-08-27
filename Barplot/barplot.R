data1 = read.delim(file.choose(), header = TRUE)

my_barplot <- function(x, y, xlab="", ylab="", main="", col="blue") {
  
  max_y <- max(y)
  
  plot_window <- c(0, length(x)+1, 0, max_y*1.1)
  
  plot.new()
  
  par(mar=c(5, 4, 4, 2) + 0.1, xpd=TRUE)
  
  plot_window <- c(0, length(x)+1, 0, max_y*1.1)
  plot.window(plot_window, xlim=c(0,length(x)+1), ylim=c(0,max_y*1.1), xaxs="i", yaxs="i", xlab="", ylab="",log = "")
  
  axis(1, at=1:length(x), labels=FALSE, tick=FALSE)
  mtext(xlab, side=1, line=2.5)
  for (i in 1:length(x)) {
    text(i, par("usr")[3]-4, labels=x[i], srt=0, adj=1, xpd=TRUE)
  }
  
  axis(2, at=seq(0, max_y, by=max_y/10), las=1, tick=TRUE)
  mtext(ylab, side=2, line=2.5)
  
  for (i in 1:length(x)) {
    rect(xleft=i-0.4, ybottom=0, xright=i+0.4, ytop=y[i], col=col)
  }
  
  
  title(main=main)
}


par(mfrow=c(1,2))

male_count = 0
female_count = 0
group1_count = 0
group2_count = 0
group3_count = 0
group4_count = 0


for (i in 1:nrow(data1)) {
  if (data1[i, "Gender"] == "Male") {
    male_count = male_count + 1
  } else if (data1[i, "Gender"] == "Female") {
    female_count = female_count + 1
  }
}


# in group and gender colums we dont have NA value so we can directly count them
for (i in 1:nrow(data1)) {
  if (data1[i, "Group"] == "Group1") {
    group1_count = group1_count + 1
  } else if (data1[i, "Group"] == "Group2") {
    group2_count = group2_count + 1
  }
  else if (data1[i, "Group"] == "Group3") {
    group3_count = group3_count + 1
  }
  else if (data1[i, "Group"] == "Group4") {
    group4_count = group4_count + 1
  }
}

my_barplot(x=c("G1", "G2", "G3", "G4"), y=c(group1_count, group2_count, group3_count, group4_count), col="red", main="Category 1")
my_barplot(x=c("M", "F"), y=c(male_count, female_count), col="blue", main="Category 2")

