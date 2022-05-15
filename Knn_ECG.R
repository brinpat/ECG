library(cvms)
library(dtw)
library(caret)
library(broom)
library(tibble)


# Dynamic time series warping similarity measure
DTW_similarity <- function(x,y){
  alignment <- dtw(x,y)
  return(alignment$distance)
}

# Euclidean distance similarity measure
euclid <- function(x,y){
  return(sqrt(sum((x-y)^2)))
}

# Get mode of data 
get_mode <- function(x) {
  ux <- unique(x)
  return (ux[which.max(tabulate(match(x, ux)))])
}

# Getting nearest neighbors 
get.nn <- function(x0,X,k,FUN){
  n <- dim(X)[1]
  dists <- matrix(NA,n,1)
  for (i in 1:n){
    dists[i] <- FUN(x0,X[i,])
  }
  inds <- order(dists)[1:k]
  return(inds)
}

# Getting predictions
knn.predict <- function(x_train, y_train, x_test,y_test, k,FUN){
  n <- dim(x_test)[1]
  y.hat <- matrix(NA,n,1)
  for (i in 1:n){
    inds <- get.nn(x_test[i,],x_train,k,FUN)
    y.hat[i] <- get_mode((y_train[inds]))
  }
  MCE <- sum((y.hat-y_test)==0)/n
   result <- list(y.hat,MCE)
  return (result)
}

load("ECG.RData")
data <- read.csv("Features_created.csv")
#test <- read.csv("Test_features.csv")


X1 <- data
Y1 <- y.train

# Splitting training data 
x_train <- X1[seq(1, dim(X1)[1], 2),]
y_train <- Y1[seq(1, dim(X1)[1], 2)]

x_test <-  X1[seq(2, dim(X1)[1], 2),]
y_test <-  Y1[seq(2, dim(X1)[1], 2)]

# Testing for k values 
MCE_arr <- numeric(30)
for (i in 1:30){
  result_test <- knn.predict(x_train,y_train,x_test,y_test,i,FUN = DTW_similarity)
  MCE_arr[i] <- result_test[[2]]
}
plot(MCE_arr,type = "l", ylab="MCE" , xlab = "k" )

# k=6
result <- knn.predict(x_train,y_train,x_test,y_test,6,FUN = DTW_similarity)
print(result)


y.hat<- as.vector(t(result[[1]]))

print(y.hat)
print(y_test)

d_multi <- tibble("target" = y_test,
                  "prediction" = y.hat)
conf_mat <- confusion_matrix(targets = d_multi$target,
                             predictions = d_multi$prediction)
plot_confusion_matrix(conf_mat$`Confusion Matrix`[[1]])


# Saving
predictions <- result
write.csv(predictions, file = "ECG_predictions_group_F_week_6.csv", row.names=FALSE)
