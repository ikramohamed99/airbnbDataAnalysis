
# Modeling for price (regression)

# Author: Dennis Ocaka


# ************************************************
# Global Environment variables
TARGET_FIELD<-"price"

# ************************************************

performPriceModels<-function(){
  
  library(keras)
 
  listings_pp<-processListings()
  
  # Drop fields
  
  # Causing Errors
  listings_pp<-subset(listings_pp, select=-c(amenities_washerdryer, weeklyprice, monthlyprice, neighbourhood_lakecity))
  
  # Target class
  
  y<-listings_pp[,which(names(listings_pp) == TARGET_FIELD)]
  x<-listings_pp[,which(names(listings_pp) != TARGET_FIELD)]
  x<-x[,-findCorrelation(cor(x), .75)]

  data<-cbind("price"=as.matrix(y), x)
  
  
  #********************************************************************************************************************
  
  # Feature Selection using recursive feature elimination
  features <- get_features(x, y, 10, lmFuncs)
  print(length(features))
  
  
  allResults <- NULL
  
  # Stratified data test
  
  dataset<-stratifiedDataset(data)
  
  # Experiment with Multi-linear regression model
  measures<-runExperiment(dataset = dataset, FUN = model_multi_regression, features)
  allResults<-data.frame(Multi_linear_regression=unlist(measures))
  print(allResults)

  # Experiment with randomForest::randomForest model
  measures<-runExperiment(dataset = dataset, FUN = model_forest_regression)
  allResults<-cbind(allResults, data.frame(Random_Forest=unlist(measures)))
  print(allResults)
  
  #***********************************************************
  #*Deep Neural Network - Parameter Tuning
  
  #print("Determining optimal hyper-parameters...")
  
  #combinations <- list(
  #  dropout1 = c(0.2,0.3,0.4),
  #  dropout2 = c(0.2,0.3,0.4),
  #  dropout3 = c(0.2,0.3,0.4),
  #  dropout4 = c(0.2,0.3,0.4),
  #  units1 = c(64,128,256),
  #  units2 = c(64,128,256),
  #  units3 = c(64,128,256),
  #  units4 = c(64,128,256),
  #  lr = c(0.0001,0.001,0.01))
  
  #runs <- tuning_run(file = "nn_tuning.R", 
  #                   flags = combinations,
  #                   runs_dir = '_tuning',
  #                   confirm = FALSE,
  #                   sample = 0.01)
  
  
  #best_run <- ls_runs(order = metric_val_root_mean_squared_error, decreasing = F, runs_dir = '_tuning')[1,]
  
  
  #print(best_run$metric_val_root_mean_squared_error)
  #print(best_run$metric_metric_coeff_det)
  #print(best_run$metric_val_mae)
  #View(ls_runs())
  
  # Experiment with keras neural network
  measures<-runExperiment(dataset = dataset, FUN = model_MLP)
  allResults<-cbind(allResults, data.frame(MLP=unlist(measures)))
  
  
  
  print(formattable::formattable(allResults))
  
  
  #print("return from main")
}


# ************************************************

# Load other R script files
source("clean_preprocess.R")
source("functions_pricemodelling.R")
