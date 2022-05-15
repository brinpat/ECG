library(car)
library(mvtnorm)
library(tibble)
library(cvms)

# Get QDA predictions
QDA <- function(X,Y,Xtest,Ytest,prior,lambda){
  
  N.0 <- sum(Y==0)
  N.1 <- sum(Y==1)
  N.2 <- sum(Y==2)
  N   <- N.0 + N.1 + N.2
  
  # Find indices of training categories
  inds.0 <- which(Y==0)
  inds.1 <- which(Y==1)
  inds.2 <- which(Y==2)

  # Find means 
  mean.0 <- colMeans(X[inds.0,])
  mean.1 <- colMeans(X[inds.1,])
  mean.2 <- colMeans(X[inds.2,])

  # Find covariance matrices 
  S.0 <- cov(X[inds.0,])
  S.1 <- cov(X[inds.1,])
  S.2 <- cov(X[inds.2,])
  
  # Pooled covariance matrices
  S.pool <- (1/(N-3))*(N.0*S.0 + N.1*S.1 + N.2*S.2)
  
  # Regularized covariance matrices
  SR.0 <- lambda*S.0 + (1-lambda)*S.pool
  SR.1 <- lambda*S.1 + (1-lambda)*S.pool
  SR.2 <- lambda*S.2 + (1-lambda)*S.pool
  
  n <- length(Xtest[,1])
  
  # Categorize test data interms of these discriminants 
  delta <- matrix(NA,3,1)
  catagories <- matrix(NA,n,1) 

  for (i in 1:n){
    delta[1] <- log(dmvnorm(Xtest[i,] , mean=mean.0, sigma=SR.0 )*prior[1])
    delta[2] <- log(dmvnorm(Xtest[i,] , mean=mean.1, sigma=SR.1 )*prior[2])
    delta[3] <- log(dmvnorm(Xtest[i,] , mean=mean.2, sigma=SR.2 )*prior[3])
    catagories[i] <- which(delta==max(delta)) - 1
  }
  
  MCE <- sum((catagories-Ytest)==0)/n
  result <- list(catagories,MCE)
  
  return(result)
}

# Load data
load("ECG.RData")
data <- read.csv("20features.csv")
#test <- read.csv("feature_extracted_test.csv")

# Remove 30 due to its error
X <- data
Y <- y.train

# State prior distributions
pri <- c(1/3,1/3,1/3) 

# Splitting training data 
x_train <- X[seq(1, dim(X)[1], 2),]
y_train <- Y[seq(1, dim(X)[1], 2)]

x_test <-  X[seq(2, dim(X)[1], 2),]
y_test <-  Y[seq(2, dim(X)[1], 2)]


# Hyper-parameter tuning
result = numeric(10)
for(i in 1:10){
  # Get predictions
  result[i] <- QDA(x_train,y_train,x_test,y_test,pri,i/10)[[2]]
}

plot( (1/10)*(1:10), result, type = "l", ylab="MCE", xlab = expression(lambda))


# Using lambda = 0.7
result1 <- QDA(x_train,y_train,x_test,y_test,pri,0.7)

y.hat <- as.vector(t(result1[[1]]))
print(y.hat)


#Confusion matrix
d_multi <- tibble("target" = y_test,
                  "prediction" = y.hat)
conf_mat <- confusion_matrix(targets = d_multi$target,
                             predictions = d_multi$prediction)
plot_confusion_matrix(conf_mat$`Confusion Matrix`[[1]])


# Saving results
predictions <- result
write.csv(predictions, file = "ECG_predictions_group_F_week_4.csv", row.names=FALSE)