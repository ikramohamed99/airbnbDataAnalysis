TARGET_FIELD<-"price"

# Custom flags
FLAGS <- flags(
  flag_numeric('dropout1', 0.3),
  flag_numeric('dropout2', 0.3),
  flag_numeric('dropout3', 0.3),
  flag_numeric('dropout4', 0.3),
  flag_integer('units1', 128),
  flag_integer('units2', 128),
  flag_integer('units3', 128),
  flag_integer('units4', 128),
  flag_numeric('lr', 0.001)
)

# Custom metric to compute R2
metric_coeff_det <- custom_metric("metric_coeff_det", function(y_true, y_pred) {
  
  SS_res <-  k_sum(k_square(y_true - y_pred))
  SS_tot <-  k_sum(k_square(y_true - k_mean(y_true)))
  R2     <-  1 - SS_res/(SS_tot + k_epsilon())
  
  return(R2)
})

build_model <- function(x_train) {
  
  model <- keras_model_sequential() 
  
  
  model %>% 
    layer_dense(input_shape = ncol(x_train), 
                units=ncol(x_train), 
                activation = "relu") %>% # Input Layer
    layer_dense(units = FLAGS$units1, 
                activation = "relu") %>% # Hidden layer 1
    layer_dropout(FLAGS$dropout1) %>% 
    layer_dense(units = FLAGS$units2, 
                activation = "relu") %>% # Hidden layer 2
    layer_dropout(FLAGS$dropout2) %>%
    layer_dense(units =  FLAGS$units3, 
                activation = "relu") %>% # Hidden layer 3
    layer_dropout(FLAGS$dropout3) %>%    
    layer_dense(units = FLAGS$units4, 
                activation = "relu") %>% # Hidden layer 4
    layer_dropout(FLAGS$dropout4) %>%
    layer_dense(units = 1, 
                activation = 'linear') # Output Layer
  
  
  model %>% compile(
    loss = 'mse',
    optimizer = optimizer_rmsprop(lr = FLAGS$lr),
    metrics = list("mae", tf$keras$metrics$RootMeanSquaredError(), metric_coeff_det))
  
}


train_test<-partition(data, percentage=75)

# Extract labels
y_train<-train_test$train[,which(names(train_test$train) == TARGET_FIELD)]
y_train<-as.matrix(y_train)
# Extract Predictors
x_train<-train_test$train[,which(names(train_test$train) != TARGET_FIELD)]
x_train<-as.matrix(x_train)


model <- build_model(x_train)


# Callback function
early_stop<-callback_early_stopping(monitor = "val_loss", patience = 10, mode = "auto")

model %>% fit(
  x_train,
  y_train,
  epochs = 50,
  shuffle = TRUE,
  batch_size = 128,
  validation_split = 0.2,
  callbacks = list(early_stop),
  verbose = 1)

#plot(history)

y_test<-train_test$test[,which(names(train_test$test) == TARGET_FIELD)]
y_test<-as.matrix(y_test)
# Predictors
x_test<-train_test$test[,which(names(train_test$test) != TARGET_FIELD)]
x_test<-as.matrix(x_test)

model %>% evaluate(
  x_test, y_test,
  verbose = 0
)

save_model_hdf5(model, 'model.h5')