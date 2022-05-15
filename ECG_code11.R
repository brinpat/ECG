library(ggpmisc)
library(mgcv)
library(quantmod)
library(splus2R)
library(devtools)
library(data.table)

# How to download mmand:
# devtools::install_github("https://github.com/jonclayden/mmand.git")
# install.packages('https://github.com/jonclayden/mmand.git', repos = NULL, type="source")
library(mmand)

setwd('A_Uni 20_21/Stats Machine Learning/Spring/GroupWork/')

which.peaks <- function(x,partial=TRUE,decreasing=FALSE){
  # https://gist.github.com/jamiefolson/5831746
  
  # Find local maxima and minima in R
  # param x numeric vector to search for peaks
  # param partial whether or not to include partial peaks
  #        at the beginning or end of the vector
  # param decreasing whether to find peaks (the default)
  #        or valleys
  if (decreasing){
    if (partial){
      which(diff(c(TRUE,diff(x)<=0,FALSE))>0)
    }else {
      which(diff(diff(x)<=0)>0)
    }    
  }else {
    if (partial){
      which(diff(c(TRUE,diff(x)>=0,FALSE))<0)
    }else {
      which(diff(diff(x)>=0)<0)
    }
    
  }
}

data <- function(train_number){
  
  # training individual
  x <- X.train[train_number,]
  xindex <- 1:length(x)
  
  #plot(x,type="l")
  
  # Morphological filtering
  # Package adapted using following sources:
  # https://www.rdocumentation.org/packages/EBImage/versions/4.14.2/topics/morphology
  # https://cran.r-project.org/web/packages/mmand/mmand.pdf
  
  kern <- shapeKernel(c(1000), type="disc")
  z <- morph(x,kern)/1000-1 # Baseline construction
  #lines(xindex, z, col='red')
  x <- x - z # Apply baseline
  
  
  remove_outliers <- function(x, na.rm = TRUE, ...) {
    # https://stackoverflow.com/questions/4787332/how-to-remove-outliers-from-a-dataset
    qnt <- quantile(x, probs=c(0.001, 0.999), na.rm = na.rm, ...)
    H <- 1 * IQR(x, na.rm = na.rm)
    y <- x
    y[x < (qnt[1] - H)] <- 0
    y[x > (qnt[2] + H)] <- 0
    y
  }
  
  x <- remove_outliers(x)
  
  #plot(x,type="l")
  
  # Find each "big peak" per heart beat cycle
  thresh <- 3*max(x)/4;
  q <- length(x)
  inds <- which(x >= c(x[1],x[1:q-1]) & x > c(x[2:q], x[q]) & x > thresh)
  
  # Remove close peaks
  for (i in 1:length(inds)-1){
    diff <- inds[i+1]-inds[i]
    if (length(diff)==0){
      
    } else if (is.na(diff)){

      
    } else if (diff < 10){
      inds <- inds[-i]
    }
  }
  
  #points(inds,x[inds],col=2)
  
  # chop the signal up and represent each heartbeat as a vector
  m <- length(inds)-1; # number of full heartbeat cycles
  p <- 200; # length we choose for the processed vectors
  X1.new <- matrix(NA,m,p);
  for (m_ in 1:m){
    
    t.old <- inds[m_]:inds[m_+1]
    
    x.old <- x[t.old]
    
    t.new <- seq(inds[m_],inds[m_+1],length=p)
    x.new <- approx(t.old,x.old,t.new)$y
    X1.new[m_,] <- x.new

  }
  print(dim(X1.new))
  Xmed <- apply(X1.new,2 ,median)
  Xfirst <- X1.new[1,]
  #plot(Xmed,type="l")
  
  return(Xfirst)
}

load('ECG.RData')

# Feature creation

# Myocardial infarction:
# The ECG shows ST elevation or depression
# Pathological Q waves develop on the ECG

# Cardiomyopathy:
# large dagger-like "septal Q waves"
# diffuse T wave changes, sometimes referred to as "giant T Wave Inversion"

raw_data <- matrix(NA, nrow=115, ncol=200)

#raw_data <- data(5)

for (i in 1:nrow(X.train)){
  raw_data[i,] <- data(i)
}

plot(raw_data[8,],type="l")
