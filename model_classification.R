
# Modeling for popularity classification

# Author: Sam Shannon

# Some lab code modified, noted in comments where used


# ************************************************
# Global Environment variables

POPULAR_THRESHOLD = 20 # percentage of days available to be determined "popular"
HOLDOUT <- 70 # legacy; holdout is not used for results, cross validation is used.
OUTPUT_FIELD <- "ispopular" # the output field for the models
BOOST <- 20 # boost setting for the C5 decision tree
FOREST_SIZE_1 <- 100 # number of trees for the "small" random forest
FOREST_SIZE_2 <- 1000 # number of trees for the "big" random forest
KFOLDS <- 5 # number of folds to use for k-fold cross validation
MLP_HIDDEN_NEURONS <- 10 # settings for the MLP neural net
MLP_EPOCHS <- 100

# ************************************************


########################
# Runs a specified classification model several times using
# k-fold cross validation to to ensure coverage of the whole dataset
# and averages the results, returning the classifier's performance measures
# Note: some code used from runExperiment function from lab 4
########################
runExperiment<-function(dataset, title, crossval, FUN, ...){
  
  cat("\n\n    ********************************************\n")
  cat(paste0("    Running Experiment: ", title, " [Cross-Validation: ", 
             ifelse(crossval, paste0("YES (", KFOLDS, " FOLDS)"),
                    paste0("NO (HOLDOUT: ", HOLDOUT, "% TRAIN")), "]\n"))
  cat("    ********************************************\n\n")
  
  if (crossval) { # using cross validation
    
    allResults<-data.frame() # to store results for each fold
    
    for (k in 1:KFOLDS){
      
      # some console output
      cat(paste0("    ", title, " (", k, "/", KFOLDS, ")\n"))
      
      # obtain a fold of the dataset
      splitData<-stratifiedSplit(newDataset=dataset,fold=k)
      
      # run the model and obtain the performance measure results
      measures<-FUN(train=splitData$train,
                    test=splitData$test,
                    plot=(k==KFOLDS),...)
      
      # store the results as another row
      allResults<-rbind(allResults,data.frame(measures))
    }
    
    cat("\n    Experiment complete\n")
    
    # take the mean average of all the fold results
    getMeans<-colMeans(allResults)
    getMeans[1:4]<-as.integer(getMeans[1:4])  # TP, FN, TN, FP are rounded to ints
    
    # return the above with the rounded values
    return(as.list(getMeans))
  }
  else { # using holdout when crossval=FALSE (not used anymore)
    
    cat(paste0("    ", title, " (1/1)\n"))
    
    splitData<-holdoutSplit(dataset) # holdout
    
    measures<-FUN(train=splitData$train,
                  test=splitData$test,
                  plot=TRUE,...)
    
    cat("\n    Experiment complete\n")
    
    measures[1:4]<-as.integer(measures[1:4])
    return(as.list(measures))
  }
}


########################
# C5 decision tree classifier (used as benchmark)
# Note: some code used from fullDT function from lab 4
########################
modelC5DT<-function(train, test, boost=1, plot=TRUE){
  positionClassOutput<-which(names(train)==OUTPUT_FIELD)
  train_inputs<-train[-positionClassOutput] # train data - output field removed
  train_expected<-train[,positionClassOutput] # train data - expected output
  
  myTitle<-"DT C5.0"
  if (boost>1)
    myTitle<-paste(myTitle,"BOOSTED=",boost)
  
  # train the model with the training dataset
  tree<-C50::C5.0(x=train_inputs,
                  y=factor(train_expected),
                  rules=TRUE,
                  trials=boost)
  
  # evaluate the classifier
  measures<-getTreeClassifications(myTree=tree,
                                   testDataset=test,
                                   title=myTitle,
                                   plot=plot)
  
  if (plot==TRUE){
    #print(summary(tree))
    
    # plot the importance of the input fields
    importance<-C50::C5imp(tree, metric = "usage")
    names(importance)<-"Strength"
    importance<-importance[order(importance$Strength,decreasing=TRUE),,drop=FALSE]
    
    # plot the importance fields
    barplot(t(importance),las=2,
            border = 0, cex.names =0.7,
            main=myTitle)
    
    # also output as a table
    print(formattable::formattable(importance))
  }
  
  return(measures)
}


########################
# Random forest classifier
# Note: some code used from randomForest function from lab 4
########################
modelRandomForest<-function(train,test,plot=TRUE,numTrees){
  positionClassOutput<-which(names(train)==OUTPUT_FIELD)
  train_inputs<-train[-positionClassOutput] # train data - output field removed
  train_expected<-train[,positionClassOutput] # train data - expected output
  
  # train the model with the training dataset
  rf<-randomForest::randomForest(train_inputs,
                                 factor(train_expected),
                                 ntree=numTrees ,
                                 importance=TRUE,
                                 mtry=sqrt(ncol(train_inputs)))
  
  # evaluate the classifier
  measures<-getTreeClassifications(myTree=rf,
                                   testDataset=test,
                                   title=paste("Random Forest=",numTrees,"trees"),
                                   plot=plot)
  
  if (plot==TRUE){
    # plot the importance of the input fields
    importance<-randomForest::importance(rf,scale=TRUE,type=1)
    importance<-importance[order(importance,decreasing=TRUE),,drop=FALSE]
    
    colnames(importance)<-paste0("Strength (", numTrees, " trees)")
    
    barplot(t(importance),las=2, border = 0,
            cex.names =0.7,
            main=paste0("Random Forest - Field Importance (", numTrees, " trees)"))
    
    # also output as a table
    print(formattable::formattable(data.frame(importance)))
  }
  
  return(measures)
}


########################
# MLP neural network classifier
# Note: some code used from functions N_MLP_TrainClassifier and N_evaluate_MLP from lab 4
########################
modelMlpNeural<-function(train,test,plot=TRUE){
  mlpTitle<-paste("MLP Neural Network Hidden Neurons=", MLP_HIDDEN_NEURONS)
  
  positionClassOutput<-which(names(train)==OUTPUT_FIELD)
  train_inputs<-train[-positionClassOutput] # train data - output field removed
  train_expected<-train[,positionClassOutput] # train data - expected output
  
  x<-as.matrix(train_inputs)
  y<-keras::to_categorical(train_expected,num_classes = 2)
  mlp_classifier = keras_model_sequential()
  
  # specify neural network layers (3 layer)
  mlp_classifier %>%
    keras::layer_dense(input_shape = ncol(x), units=ncol(x), activation = "relu") %>%
    keras::layer_dropout(0.2) %>%
    keras::layer_dense(units = MLP_HIDDEN_NEURONS, activation = "relu") %>%
    keras::layer_dropout(0.2) %>%
    keras::layer_dense(units = 2, activation = "softmax")
  
  # add a loss function and optimiser
  mlp_classifier %>%
    keras::compile(
      loss = "categorical_crossentropy",
      optimizer = "adagrad",
      metrics = "accuracy"
    )
  
  # train the model with the training dataset
  fit = mlp_classifier %>%
    keras::fit(
      x = x,
      y = y,
      shuffle = T,
      batch_size = 5,
      validation_split = 0.2,
      epochs = MLP_EPOCHS,
      callbacks = c(
        callback_early_stopping(monitor = "val_loss", patience = 8, mode = "auto")),
      verbose=0, view_metrics=0
    )
  
  # optionally plot loss/accuracy graph
  if (plot==TRUE){
    suppressMessages(print(plot(fit)))
  }
  
  test_inputs<-test[-positionClassOutput] # test data - output field removed
  
  # generate membership probabilities for each class
  testPredictedAllClassProbs<-predict(mlp_classifier,as.matrix(test_inputs))
  testPredictedClassProbs<-testPredictedAllClassProbs[,2]
  test_expected<-test[,positionClassOutput]
  
  # evaluate the classifier
  measures<-NdetermineThreshold(test_expected=test_expected,
                                test_predicted=testPredictedClassProbs,
                                plot=plot,
                                title=mlpTitle)
  
  # optionally plot performance graphs
  if (plot==TRUE) {
    NprintMeasures(results=measures,title=mlpTitle)
  }
  
  return(measures)
}


########################
# Driver function
# Performs all the classification experiments and outputs results
########################
performPopularityModels<-function(){
  
  # flags to enable/disable removal of certain fields to reduce dimensionality
  limitAmenities <- TRUE
  removeNeighbourhoods <- FALSE
  
  # obtain a cleaned and preprocessed listings dataset
  # (that is normalised too)
  listings<-processListings(verbose=FALSE, rescale=TRUE, zscale=TRUE)
  
  # remove all but 15 named amenities if variable set to true
  # (to reduce dimensionality and try lower overfitting)
  if (limitAmenities) {
    listings<-ppWhitelistAmenities(listings, c("amenities_hairdryer", "amenities_wirelessinternet",
                        "amenities_24hourcheckin", "amenities_smokingallowed", "amenities_cabletv",
                        "amenities_tv", "amenities_petsliveonthisproperty", "amenities_petsallowed",
                        "amenities_kitchen", "amenities_gym", "amenities_washerdryer",
                        "amenities_familykidfriendly", "amenities_freeparkingonpremises",
                        "amenities_breakfast", "amenities_pool"))
  }
  if (removeNeighbourhoods) {
    listings<-ppRemoveNeighbourhoods(listings)
  }
  
  # some console output
  cat("\n\n********************************************\n")
  cat("Beginning classification experiments for predicting high-demand listings\n")
  cat("********************************************\n")
  
  # derive the output field (1=most popular listings, 0=not as popular)
  if (max(listings$availability90 <= 1)) {
    av_threshold<-POPULAR_THRESHOLD/100
  } else {
    av_threshold<-ceiling(POPULAR_THRESHOLD*90/100)
  }
  ispopular<-ifelse(listings$availability90 <= av_threshold, 1, 0)
  listings<-cbind(listings, ispopular)
  
  # remove availability-based fields as this would allow model to "cheat"
  # since the output field is derived from availability
  listings<-ppRemoveFields(listings, c("availability30", "availability90"))
  
  
  # prepare k-fold stratification
  listings<-stratifiedDataset(listings)
  
  # keep track of classifier results
  allResults<-NULL
  
  
  # Model: C5 DT Boosted
  measures<-runExperiment(dataset=listings, crossval=TRUE, title=paste("DT C5.0 Boost=",BOOST),
                          FUN=modelC5DT)
  allResults<-data.frame(C5DT_Boosted=unlist(measures))
  
  # Model: Random Forest with lower number of trees
  measures<-runExperiment(dataset=listings, crossval=TRUE, title=paste("Random Forest=",FOREST_SIZE_1,"trees"),
                          FUN=modelRandomForest, FOREST_SIZE_1)
  allResults<-cbind(allResults,data.frame(RandomForest_small=unlist(measures)))
  
  # Model: Random Forest with larger number of trees
  measures<-runExperiment(dataset=listings, crossval=TRUE, title=paste("Random Forest=",FOREST_SIZE_2,"trees"),
                          FUN=modelRandomForest, FOREST_SIZE_2)
  allResults<-cbind(allResults,data.frame(RandomForest_big=unlist(measures)))
  
  # Model: MLP Neural Network
  measures<-runExperiment(dataset=listings, crossval=TRUE, title="MLP Neural Network",
                          FUN=modelMlpNeural)
  allResults<-cbind(allResults,data.frame(NN_MLP=unlist(measures)))
  
  
  # collect results
  allResults<-data.frame(t(allResults))
  
  # sort by highest MCC
  allResults<-allResults[order(allResults$MCC,decreasing = TRUE),]
  
  # output results to compare all classifiers
  allResults[,1:4]<-sapply(allResults[,1:4], as.integer)
  allResults$folds<-KFOLDS
  print(formattable::formattable(allResults))

  # produce a bar plot showing the TPR/sensitivity, TNR/specificity, MCC and AUC
  # for each classifier in a more visually interpretable medium
  colours = c("lightpink","lightgoldenrod1", "lightskyblue", "springgreen2")
  results<-t(data.frame(allResults$TPR/100, allResults$TNR/100, allResults$MCC, allResults$AUC))
  rownames(results)<-c("Sensitivity (TPR)", "Specificity (TNR)", "MCC", "AUC")
  colnames(results)<-rownames(allResults)
  barplot(results,main="Classification Performance",beside=T, xlab="Models",ylab="Performance Metric Value", axis.lty="solid", cex.names=0.9, col=colours, ylim=c(0,1.0), legend=TRUE, xlim=c(0,30))
  
  # some console output
  cat("\n\n********************************************\n")
  cat("Experiments complete, results have been output to plots and viewer\n")
  cat("********************************************\n")
}


# ************************************************

# Load other R script files
source("clean_preprocess.R")
source("functions_preprocessing.R")
source("functions_popularitymodelling.R")
