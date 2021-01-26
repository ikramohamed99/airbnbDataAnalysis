# ************************************************
#
# This is the main "driver" R file that calls functions from other R files
# which perform individual parts of the project like cleaning, modeling, etc.
#
# ************************************************

# Clear global environment
rm(list=ls())


# ************************************************
# Data cleaning and pre-processing
# ************************************************
clean_preprocess<-function(){
  
  # some console output
  cat("\n%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n")
  cat("CLEANING & PRE-PROCESSING BEGIN\n")
  cat("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n\n")
  
  # import the file containing the code and functions for this section
  source("clean_preprocess.R")
  
  # clean and pre-process listings with verbose console output and
  # plotting of graphs enabled
  # 
  # note that this is called here only to output to console and produce graphs,
  # each separate file will call this again to get their own dataset object
  # and may use different parameters
  listings<-processListings(verbose=TRUE, plot=TRUE)
  
  # more console output
  cat("\n%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n")
  cat("CLEANING & PRE-PROCESSING END\n")
  cat("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n")
}


# ************************************************
# Data exploration
# ************************************************
data_exploration<-function(){
  
  # some console output
  cat("\n%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n")
  cat("DATA EXPLORATION BEGIN\n")
  cat("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n\n")
  
  # import the file containing the code and functions for this section
  source("listings_exploration.R")
  
  # run the data exploration
  suppressWarnings(listingsExploration())
  
  # more console output
  cat("\n%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n")
  cat("DATA EXPLORATION END\n")
  cat("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n")
}


# ************************************************
# Model 1: Price prediction regression
# ************************************************
model1_price<-function(){
  
  # some console output
  cat("\n%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n")
  cat("MODEL 1 (PRICE PREDICTION) BEGIN\n")
  cat("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n\n")
  
  # import the file containing the code and functions for this section
  source("model_priceregression.R")
  
  # run the modeling
  performPriceModels()
  
  # more console output
  cat("\n%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n")
  cat("MODEL 1 (PRICE PREDICTION) END\n")
  cat("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n")
}


# ************************************************
# Model 2: Kmeans clustering
# ************************************************
model2_kmeans<-function(){
  
  # some console output
  cat("\n%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n")
  cat("MODEL 2 (KMEANS CLUSTERING) BEGIN\n")
  cat("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n\n")
  
  # import the file containing the code and functions for this section
  source("kmeans_clustering.R")
  
  # run the modeling
  suppressWarnings(performKmeans())
  
  # more console output
  cat("\n%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n")
  cat("MODEL 2 (KMEANS CLUSTERING) END\n")
  cat("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n")
}


# ************************************************
# Model 3: Popularity classification
# ************************************************
model3_popularity<-function(){
  
  # some console output
  cat("\n%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n")
  cat("MODEL 3 (POPULARITY CLASSIFICATION) BEGIN\n")
  cat("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n\n")
  
  # import the file containing the code and functions for this section
  source("model_classification.R")
  
  # run the modeling
  performPopularityModels()
  
  # more console output
  cat("\n%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n")
  cat("MODEL 3 (POPULARITY CLASSIFICATION) END\n")
  cat("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n")
}


# ************************************************

# Clear console
cat("\014")

# Set seed for concistent output between executions
set.seed(123)

# Load libraries
source("libraries.R")

# Run the main functions
clean_preprocess()
data_exploration()
model1_price()
model2_kmeans()
model3_popularity()

print("End of execution")

