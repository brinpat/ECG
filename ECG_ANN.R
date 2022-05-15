library(tensorflow)
library(keras)

load("ECG.RData")

data <- read.csv("Features_created.csv")
test <- read.csv("Test_features.csv")

x_train <- as.matrix(data)
y_train <- y.train
x_test  <- as.matrix(test)
b
model <- keras_model_sequential()%>%  
  layer_flatten(input_shape = c(200, 1)) %>%
  layer_dense(25,activation = "relu")%>%
  layer_dropout(0.6)%>%
  layer_dense(3, activation = "softmax")

summary(model)

#Compile model
model %>% 
  compile(
    loss = "sparse_categorical_crossentropy",
    optimizer = "adam",
    metrics = "accuracy"
  )

#Fit model 
model %>% 
  fit(
    x = x_train, y = y_train,
    epochs = 15,
    validation_split = 0.4,
    verbose = 1
  )

predictions <- predict_classes(model,x_test)
print(predictions)
write.csv(predictions, file = "ECG_predictions_group_F_week_4.csv", row.names=FALSE)