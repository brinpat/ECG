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

remove_outliers <- function(x, na.rm = TRUE, ...) {
  # https://stackoverflow.com/questions/4787332/how-to-remove-outliers-from-a-dataset
  qnt <- quantile(x, probs=c(0.001, 0.999), na.rm = na.rm, ...)
  H <- IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- 0
  y[x > (qnt[2] + H)] <- 0
  y
}


data <- function(train_number){
  
  # training individual
  x <- X.train[train_number,]
  xindex <- 1:length(x)
  
  #plot(x,type="l")
  
  # Linear regression model
  xline <- lm(x~xindex)
  
  
  intercept <- xline$coefficients[1]
  grad <- xline$coefficients[2]
  y = grad * xindex + intercept # baseline
  
  x <- x - y
  
  x <- remove_outliers(x)
  
  plot(x,type="l")
  
  # Find each "big peak" per heart beat cycle
  thresh <- 3*max(x)/4 # upper quartile threshold
  q <- length(x)
  inds <- which(x >= c(x[1],x[1:q-1]) & x > c(x[2:q], x[q]) & x > thresh)
  
  # Remove close peaks
  for (i in 1:length(inds)-1){
    diff <- inds[i+1]-inds[i]
    if (length(diff)==0){
      
    } else if (is.na(diff)){

      
    } else if (diff < 0.1*30000/length(inds)){
      inds <- inds[-i]
    }
  }
  
  points(inds,x[inds],col=2)
  
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
  
  Xmed <- apply(X1.new,2 ,median)
  #plot(Xmed,type="l")
  
  Xmean <- apply(X1.new,2 ,mean)
  plot(Xmean,type="l")
  
  pk <- findPeaks(Xmean, thresh = 0.00001)
  tr <- findValleys(Xmean, thresh = 0.00001)
  
  points(pk, Xmean[pk])
  points(tr, Xmean[tr])
  
  # Generative model
  Xstdev <- apply(X1.new,2 ,stdev)
  Xgen <- rnorm(200, mean=Xmean, sd=Xstdev)
  # plot(Xgen,type="l")
  
  return(Xmean)
}

load('ECG.RData')

raw_data <- matrix(NA, nrow=115, ncol=200)

for (i in 1:nrow(X.train)){
  raw_data[i,] <- data(i)
}




