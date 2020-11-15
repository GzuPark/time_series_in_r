# --------------------------------------------------
# Explore Data Analysis for Time Series
# 
# R version: 4.0.2
# Date: Nov 14, 2020
# Author: Jongmin Park (jijupax@gmail.com)
# --------------------------------------------------

description <- function(datum) {
  # Print out information of the data
  # Arguments
  #   datum: matrix or data frame whatever
  # Return
  #   None
  print("Summary...")
  print(summary(datum))
  cat("\n\nCheck NA values...\n")
  print(datum[is.na(datum)])  # check missing value
  cat("\n\nPrint first few rows...\n")
  head(datum)
}


airpassenger_plot <- function(datum,
                              title, 
                              is_axes = FALSE, 
                              xrange = c(1949:1961), 
                              yrange = seq(0, 600, by=100)) {
  # Draw line plot of AirPassenger data
  # Arguments
  #   datum: matrix or data frame whatever
  #   title: (string) title of the plot
  #   is_axes: (boolean) draw x & y axes or not, redefine axis interval
  #   xrange: (range) range of x-axis
  #   yrange: (range) range of y-axis
  # Return
  #   None
  plot(datum, axes=is_axes, main=title)
  if (!is_axes) {
    axis(side=1, at=xrange)  # change x-axis interval, 2 -> 1
    axis(side=2, at=yrange)
    box()
  }
}


diabetes_plot <- function(datum, id){
  # Draw line plot of diabetes data
  # Arguments
  #   datum: matrix or data frame whatever
  #   id: patient id
  # Return
  #   None
  tmp <- datum[datum$ID == id, ]
  plot(exp(tmp$VALUE),
       main=paste(id, "'s diabetes values"),
       col="red",
       type="l",
       xlab="",
       ylab="VALUE")
}
