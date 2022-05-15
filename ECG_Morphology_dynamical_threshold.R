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

data <- function(x,p){
  
  # training individual
  xindex <- 1:length(x)
  
  plot(x,type="l")
  
  # Morphological filtering
  # Package adapted using following sources:
  # https://www.rdocumentation.org/packages/EBImage/versions/4.14.2/topics/morphology
  # https://cran.r-project.org/web/packages/mmand/mmand.pdf
  
  kern <- shapeKernel(c(1000), type="disc")
  z <- morph(x,kern)/1000-1 # Baseline construction
  
  baseline_mse <- sum((x-z)^2)/length(x) # Error function for baseline
  
  lines(xindex, z, col='red')
  x_base <- x - z # Apply baseline
  
  x <- remove_outliers(x_base)
  outlier_mse <- sum((x_base-x)^2)/length(x_base)
  
  plot(x,type="l")
  
  # Find each "big peak" per heart beat cycle
  
  lim <- 2*max(x)/4 # morphology limiter
  # Use morphological filtering as threshold
  thresh <- closing(x,kern) # Peak line
  thresh[thresh < lim] <- lim
 
  lines(xindex, thresh, col='red')

  q <- length(x)
  inds <- which(x >= c(x[1],x[1:q-1]) & x > c(x[2:q], x[q]) & x >= thresh)
  
  # Remove close peaks
  for (i in 1:length(inds)-1){
    diff <- inds[i+1]-inds[i]
    if (length(diff)==0){
      
    } else if (is.na(diff)){

      
    } else if (diff < 0.1*30000/length(inds)){
      inds <- inds[-i]
    }
  }
  num_peaks <- length(x[inds])
  
  points(inds,x[inds],col=2)
  
  # chop the signal up and represent each heartbeat as a vector
  m <- length(inds)-1; # number of full heartbeat cycles 
  X1.new <- matrix(NA,m,p);
  for (m_ in 1:m){
    
    t.old <- inds[m_]:inds[m_+1]
    
    x.old <- x[t.old]
    
    t.new <- seq(inds[m_],inds[m_+1],length=p)
    x.new <- approx(t.old,x.old,t.new)$y
      
    X1.new[m_,] <- x.new

  }
  
  if (ncol(X1.new)>200){
    print(1)
  }
  
  # Initially was using median to avoid double heart beats
  Xmed <- apply(X1.new,2 ,median)
  med_mse <- sum((X1.new-Xmed)^2)/length(X1.new) # median error
  #plot(Xmed,type="l")
  
  Xmean <- apply(X1.new,2 ,mean)
  mean_mse <- sum((X1.new-Xmean)^2)/length(X1.new) # mean error
  #plot(Xmean,type="l")
  
  # Generative model
  Xstdev <- apply(X1.new,2 ,stdev)
  Xgen <- rnorm(200, mean=Xmean, sd=Xstdev)
  # plot(Xgen,type="l")
  
  return(c(Xmean,baseline_mse,med_mse,mean_mse,outlier_mse,num_peaks))
}

# length we choose for the processed vectors
load('ECG.RData')
p=200

raw_data <- matrix(NA, nrow=115, ncol=p+5)
for (i in 1:nrow(X.train)){
  raw_data[i,] <- data(X.train[i,],p)
}

# Checking plots
plot(raw_data[1,],type="l")

# Data and errors
baseline_mse <- raw_data[,p+1]
median_mse <- raw_data[,p+2]
mean_mse <- raw_data[,p+3]
outlier_removal_mse <- raw_data[,p+4]
num_ecg_w_outlier<-sum(outlier_removal_mse!=0)
peaks <- raw_data[,p+5]
totalpeaks <- sum(peaks) # 61 under fit or over fit (15 underfit, 46 overfit)
# True total peaks is 4,603

raw_data <- raw_data[,-c((p+1):(p+5))]

# Average loss
base_avloss <- mean(baseline_mse)
med_avloss <- mean(median_mse)
mean_avloss <- mean(mean_mse)
outlier_avloss <- mean(outlier_removal_mse)

total_avloss <- sum(base_avloss,med_avloss,mean_avloss)

# Saving results 
write.csv(raw_data, file = "20features.csv", row.names=FALSE)


