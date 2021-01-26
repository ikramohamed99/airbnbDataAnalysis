# ************************************************
# Modeling
#
# DATE: 11/11/2020
# VERSION: v1.00
# AUTHOR: Dennis Ocaka
#
# UPDATE
# 1.00 11/11/2020 Initial version
# ************************************************
DATASET_FILENAME  <- "airbnb_listings.csv"
TARGET_FIELD      <- "price"


KFOLDS          <- 3                 # Number of folded experiments

#***
#* Train/split
#* 
#* A naive implementation of train test split
#* 
#* 
#* INPUT:
#*       Data Frame -> Dataset to split
#*       Integer    -> Percetange of data to split
#* 
#* 
#* OUTPUT:
#*       List       -> Contains the train/test split
#*
partition<-function(dataset, percentage=80){
  
  
  dataset<-dataset[order(runif(nrow(dataset))),]
  # randomise set
  dataset<-dataset[sample(1:nrow(dataset)),]
  
  # split set
  training_records<-round(nrow(dataset)*(percentage/100))
  training_set <- dataset[1:training_records,]
  testing_set = dataset[-training_records,]
  
  return(list("train"=training_set,
              "test"=testing_set))
}


# ************************************************
# 
# Append a column called "foldID" that indicates the fold number
#
# INPUT   :   data frame         - dataset        - dataset
#
# OUTPUT  :   data frame         - dataset        - dataset with foldID added
#
#
# ************************************************
allocateFoldID<-function(dataset){
  
  recordsPerFold<-ceiling(nrow(dataset)/KFOLDS)
  
  foldIds<-rep(seq(1:KFOLDS),recordsPerFold)
  
  foldIds<-foldIds[1:nrow(dataset)]
  
  dataset$foldId<-foldIds
  
  return(dataset)
} 

# ************************************************
# 
# Split dataset by the class (assume 2-class)
# Calculate the number of records that will appear in each fold
# Give each of these blocks a unique foldID
# combine the datasets & randomise
# The dataset now has a foldID from which we can select the data
# for the experiments
#
# INPUT   :   data frame         - dataset        - dataset
#
# OUTPUT  :   data frame         - dataset        - dataset with foldID added
#
# ************************************************
stratifiedDataset<-function(originalDataset){
  
  positionClassOutput=which(names(originalDataset)==TARGET_FIELD)
  
  # Get the unique class values
  classes<-unique(originalDataset[,positionClassOutput])
  
  # Split dataset into the two classes (so as to keep the class balance the same in the datasets)
  indexClass1<-which(originalDataset[,positionClassOutput]==classes[1])
  split1<-originalDataset[indexClass1,]
  split2<-originalDataset[-indexClass1,]
  
  # Append a column that indicates the fold number for each class
  split1<-allocateFoldID(split1)
  split2<-allocateFoldID(split2)
  
  # Combine the two datasets
  
  newDataset<-rbind(split1,split2)
  
  #Randomise the classes
  newDataset<-newDataset[order(runif(nrow(newDataset))),]
  
  return(newDataset)
}

# ************************************************
# stratifiedSplit() :
#
# Generate the TRAIN and TEST dataset based on the current fold
#
# INPUT   :   data frame         - dataset        - dataset
#
# OUTPUT  :   list               - train & test datasets
# ************************************************

stratifiedSplit<-function(newDataset,fold){
  
  test<-subset(newDataset, subset= foldId==fold, select=-foldId)
  train<-subset(newDataset, subset= foldId!=fold, select=-foldId)
  
  return(list(
    train=train,
    test=test))
}

# ***********************************************************************************
# runExperiment() :
#
#
# INPUT   :   data frame         - dataset        - dataset
#             object function    - FUN            - name of function
#             ...                - optional        - parameters are passed on
#
# OUTPUT  :   data frame         - dataset        - dataset with foldID added
#
# ***********************************************************************************
runExperiment<-function(dataset,FUN, ...){
  
  allResults<-data.frame()
  
  for (k in 1:KFOLDS){
    
    splitData<-stratifiedSplit(newDataset=dataset, fold=k)
    
    measures<-FUN(train=splitData$train,
                  test=splitData$test,
                  plot=(k==KFOLDS),
                  ...)
    
    allResults<-rbind(allResults, data.frame(measures))
  } 
  
  # Return the means from all the experiments back as a list
  getMeans<-colMeans(allResults)
  
  return(as.list(getMeans))
} 

#************************************************************************************
#*
#* Creates a formula for price against all input fields if no fields are specified
#* 
#* 
#* INPUT:
#*       Data Frame -> Dataset
#*       List       -> Input fields
#*       
#* OUTPUT:
#*       Formula
formulate<-function(data, fields=NULL){
  
  if(is.null(fields)){
    inputs<-paste(names(data)[which(names(data)!=TARGET_FIELD)],collapse="+")
  } else{
    inputs<-paste(fields[which(fields!=TARGET_FIELD)], collapse="+")
  }
  
  
  output<-paste(TARGET_FIELD,"~")
  
  return(as.formula(paste(output, inputs)))
} 

#************************************************
#* Recursive Feature Elimination 
#* 
#* Using Caret
#* 
#* INPUTS: 
#*         x -> DATAFRAME: Predictors
#*         y -> Vector: Class labels
#*         repeats -> INT: 
#*         FUN -> Object: function
#* 
#* 
get_features<-function(x, y, repeats, FUN){
  
  # Subset of features
  subsets <- c(2:25, 30, 35, 40, 45, 50, 55, 60, 65)
  
  
  ctrl <- rfeControl(functions = FUN,
                     method = "repeatedcv",
                     repeats = repeats,
                     verbose = FALSE)
  
  lmProfile <- rfe(x, 
                   y,
                   sizes = subsets,
                   rfeControl = ctrl)
  
  
  print(lmProfile)
  
  return(predictors(lmProfile))
  
}

#*******************************************************************
#* rImportance
#* 
#* 
#* Computes the relative importance of the predictors on the output 
#* class
#* 
#*
rImportance<-function(dataset, model){
  
  # determine the relative strength of each on the output prediction
  importance<-as.data.frame(caret::varImp(model, scale = TRUE))
  print(formattable::formattable((importance)))
  
  orderlist<-rownames(importance)[order(importance$Overall, decreasing=TRUE)]
  
  # Removes punctuation characters from field names as these cause issues
  row.names(importance)<-gsub("[[:punct:][:blank:]]+", "", row.names(importance))
  # Plot the % importance ordered from lowest to highest
  par(mar=c(15, 3, 3, 1))
  barplot(t(importance[order(importance$Overall),,drop=FALSE]), horiz=TRUE,
          main="Price", ylab="Importance", las=2, cex.names=0.8)
  
  return(orderlist)
}

#****************************************************************
#* Calculate Root Mean Squared Error RMSE metric
#* RMSE is a measure of how spread out these residuals
#*
#*INPUT: 
#*      vector double - actual - values for expected values
#*      vector double - predicted - values of predicted values
#*OUTPUT :
#*       float - calculated RMSE
rmse<-function(actual,predicted){ 
  return(sqrt(mean((actual-predicted)^2)))
}

#****************************************************************
#* Calculate Mean Absolute Error MAE metric
#* The average of the absolute differences between predictions and 
#* actual values
#* 
#* INPUT: 
#*        vector double - actual - values for expected values
#*        vector double - predicted - values of predicted values
#*        
#* OUTPUT: 
#*       float - calculated RMSE
mae<-function(actual,predicted){
  return((mean(abs(actual-predicted))))
}


#***************************************************************
#*
#*
#* Computes the r2 metric 
#* 
#* INPUT: 
#*       vector double - actual - values for expected values
#*       vector double - predicted - values of predicted values
#*       
#* OUTPUT:
#*       float - calculated R^2
r2<-function(actual, predicted){
  return(cor(actual, predicted) ^2)
}

# ************************************************
#
# Use training_set to evaluate the regression model
#
# INPUT : vector double - probs - probability of being class 1
# Data Frame - testing_data - Dataset to evaluate
# double - threshold - cutoff (probability) 
#
# OUTPUT : List - Named evaluation measures
# - Predicted class probability
#
# ************************************************
evaluate_model<-function(model, testing_set, plot=TRUE) {
  
  # Extract predictor (input) values from TEST set into a data frame
  predictorInput<-testing_set[,which(names(testing_set) != TARGET_FIELD)]
  # Get predictions from the model using the TEST set
  y_predicted<-predict(model, predictorInput)
  # Extract the expected response (output) values from TEST set into a data frame
  y_actual<-testing_set[,TARGET_FIELD]
  
  # Calculate the metrics using functions 
  
  rmse<-round(rmse(actual=y_actual,predicted=y_predicted), digits=2)
  mae<-round(mae(actual=y_actual,predicted=y_predicted), digits=2)
  r2<-round(r2(actual=y_actual,predicted=y_predicted), digits=2)
  
  return(
    list(MAE=mae,
         RMSE=rmse,
         r2=r2)
  )
} 


#*******************************************************************************
#* model_multi_regression()
#*
#* Input
#* 
#*
model_multi_regression<-function(train, test, features, plot=TRUE){
  
  
  # Create Forumla
  formula <- formulate(data, features)
  # Train Model
  model<-lm(formula, data=train)

 
  if (plot==TRUE){
    
    # print an analysis of our multiple regression model
    print(summary(model))
    plot(model)
    
    rImportance(dataset = test, model = model)
  }
  
  # evaluate model
  metrics<-evaluate_model(testing_set = test, model = model)
  
  
  
  return(metrics)
}


#********************************************
#*
#* Create a Deep Neural network 
#* 
#* INPUT: 
#*        list: Train/Test Data
#*        string: Output class
#*        
#* Output:
#*        performance metrics 
#*        
model_MLP<-function(train, test, plot=TRUE){
  
  model<-MLP_REGRESSION(train, plot)

  metrics<-evaluate_MLP(test, model)
  
  return(metrics)

} 

#************************************************************
#*
#* Creates a simple multi-layer perceptron regression model to
#* predict the price of a house 
#* 
#* 
#* INPUT:  
#*         Dataframe -> training set
#*         String    -> output field to predict
#*         
MLP_REGRESSION<-function(train, plot){
  
  # labels
  labels<-train[,which(names(train) == TARGET_FIELD)]
  labels<-as.matrix(labels)
  
  # Predictors
  features<-train[,which(names(train) != TARGET_FIELD)]
  features<-as.matrix(features)
  
  model <- keras_model_sequential() 
  
  model %>% 
    layer_dense(input_shape = ncol(features), units=ncol(features), activation = "relu") %>% 
    layer_dense(units = 64, activation = "relu") %>%  # Layer1
    layer_dropout(0.4) %>%
    layer_dense(units = 128, activation = "relu") %>%  # Layer2
    layer_dropout(0.3) %>%
    layer_dense(units = 64, activation = "relu") %>%  # Layer3
    layer_dropout(0.3) %>%
    layer_dense(units = 128, activation = "relu") %>%  # Layer4
    layer_dropout(0.2) %>%
    layer_dense(units = 1, activation = 'linear') 
  
  # Custom metric to compute R2
  metric_coeff_det <- custom_metric("R2", function(y_true, y_pred) {
    
    SS_res <-  k_sum(k_square(y_true - y_pred))
    SS_tot <-  k_sum(k_square(y_true - k_mean(y_true)))
    R2     <-  1 - SS_res/(SS_tot + k_epsilon())
    
    return(R2)
  })
  
  model %>% compile(
    loss = 'mse',
    optimizer = optimizer_rmsprop(lr = 1e-02),
    metrics = list("mae", tf$keras$metrics$RootMeanSquaredError(), metric_coeff_det))
  
  # Store 
  
  history <- model %>% keras::fit(
    features,
    labels,
    epochs = 50,
    shuffle = TRUE,
    batch_size = 128,
    validation_split = 0.2,
    callbacks = c(
      callback_early_stopping(monitor = "val_loss", patience = 10, mode = "auto")),
    verbose = 0)
  
  
  if(plot==TRUE){
    print(summary(model))
    try(plot(history), silent=TRUE)
  }
  
  return(model)
}


#*******************************************************
#*
#* Evaluate Multi-layer Perceptron
#* 
#* INPUT: 
#*       DataFrame -> Testing set
#*       Object    -> Keras Model
#*       
#* OUTPUT:
#*       Performance Metrics
#* 
#* 
evaluate_MLP<-function(test, model){
  
  # Labels
  labels<-test[,which(names(test) == TARGET_FIELD)]
  labels<-as.matrix(labels)
  # Predictors
  data<-test[,which(names(test) != TARGET_FIELD)]
  data<-as.matrix(data)
  
  # Evaluate on test data and labels
  metrics = model %>% evaluate(data, labels, batch_size = 128, verbose=1)
  
  return(
    list(MAE=metrics[1],
         RMSE=metrics[2],
         r2=metrics[3])
  )
}


#*****************************************************************
#*
#*  Create a random forest regression model on pre-processed data
#*  and evaluates the model returning the metrics for the model
#*  
#*  
#*  INPUT:
#*        data: list -> train_test split
#*        output: String -> Output class
#*        
#*        
#*  OUTPUT:
#*        list: Performance metrics
#*
model_forest_regression<-function(train, test, plot=TRUE){
  
  title<-"Random Forest Regression Model"
  
  
  formula<-formulate(train, NULL)
  
  model<-randomForest(formula,
                      train,
                      mtry = 40,
                      ntree = 1000,
                      importance = TRUE, 
                      respect.unordered.factors = "order",
                      na.action = na.omit)
  
  
  if (plot==TRUE){
    
    plot(model)
    
    print(summary(model))
    
    # determine the relative strength of each on the output prediction
    importance<-randomForest::importance(model, scale=TRUE, type=1)
    importance<-importance[order(importance,decreasing=TRUE),,drop=FALSE]
    
    colnames(importance)<-"Strength"
    
    barplot(t(importance),
            las=2, 
            border = 0,
            cex.names =0.7,
            main=title)
    
    print(formattable::formattable(data.frame(importance)))
  }
   
  metrics <- evaluate_model(model, test)
  

  return(metrics)
}
