# --------------------------------------------------
# Preprocess Data
# used to UCI Diabetes Data Set
# https://archive.ics.uci.edu/ml/datasets/diabetes
#
# Utility of Normalize and Denomalize data
# 
# R version: 4.0.2
# Date: Nov 14, 2020
# Author: Jongmin Park (jijupax@gmail.com)
# --------------------------------------------------

preprocess <- function(start=1, end=70) {
  # Preprocess diabetes data. Read txt files -> combine -> preprocess -> return
  # Arguments
  #   start: (int) start number of files
  #   end: (int) end number of files
  # Return
  #   df: (data.frame) preprocessed data
  tmp <- list(0)
  
  for(i in start:end) {
    # reference: sprintf
    # https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/sprintf
    file_name <- sprintf("Diabetes-Data/data-%02d", i)
    datum <- read.csv(file_name, header=FALSE, sep='\t')
    colnames(datum) <- c('DATE', 'TIME', 'CODE', 'VALUE')
    datum$CODE <- as.factor(datum$CODE)
    datum$VALUE <- log(as.integer(datum$VALUE) + 0.001)
    tmp[[i]] <- data.frame(ID=i, datum)
  }
  
  df <- do.call("rbind", tmp)
  df <- subset(df, !is.na(df$VALUE))
  df$NO <- row.names(df)
  df$NO <- as.integer(df$NO)
  df$ID <- as.factor(df$ID)
  df$DATE <- stats::ts(df$DATE)
  df$TIME <- stats::ts(df$TIME)
  
  return(df)
}


normalize <- function(x, base) {
  # Normalize using by min & max
  # Arguments
  #   x: will be normalize data
  #   base: criterion data, calculate min & max
  # Return
  #   normalized data
  return((x - min(base)) / (max(base) - min(base)))
}


denormalize <- function(x, base) {
  # Denormalize using by min & max
  # Arguments
  #   x: will be denormalize data
  #   base: criterion data, calculate min & max
  # Return
  #   denormalized data
  return(x * (max(base) - min(base)) + min(base))
}
