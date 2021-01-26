# Clear global environment
rm(list=ls())

# ************************************************
# Global Environment variables
OUTPUT_FIELD <- "Status" # Field name of the output class to predict
HOLDOUT <- 70 # % split to create TRAIN dataset
SCALE_DATASET <- TRUE # Set to true to scale dataset before MLstage
OUTLIER_CONF <- 0.95 # Confidence p-value for outlier detection

MAX_LITERALS <- 55 # Maximum numner of hotcoding new fields
CUT_OFF<-0.9

manualTypes <- data.frame()

DATASET_FILENAME  <- "airbnb_listings.csv"

TYPE_DISCREET <- "DISCREET" # field is discreet (numeric)
TYPE_ORDINAL <- "ORDINAL" # field is continuous numeric
TYPE_SYMBOLIC <- "SYMBOLIC" # field is a string
TYPE_NUMERIC <- "NUMERIC" # field is initially a numeric


DISCREET_BINS <- 5 # Number of empty bins to determine discreet

DATE_COLLECTED    <- as.Date("04/01/2016", format="%d/%m/%Y")

DROP_COLUMN_NAMES <- c(
  "id", "hostid", "neighbourhood", "neighbourhoodcleansed", "latitude",
  "longitude", "islocationexact", "reviewscoresaccuracy", 
  "reviewscorescleanliness", "reviewscorescheckin", "reviewscorescommunication",
  "reviewscoreslocation", "reviewscoresvalue", "hosttotallistingscount", "city",
  "zipcode", "smartlocation", "squarefeet", "calendarupdated", 
  "hosthasprofilepic", "hostacceptancerate", "availability60", "availability365")

LIBRARIES<-c("outliers",
             "corrplot",
             "MASS",
             "formattable",
             "stats",
             "PerformanceAnalytics")

#*************************************************************
#*
#* Handles data cleaning 
clean<-function(listings){
 
  
  # Remove fields that have identical values for all records
  listings<-ppRemoveIdenticalValueFields(listings)
  
  # Manually remove some fields deemed to be unnecessary (e.g. id numbers)
  listings<-ppRemoveFields(listings, fieldNames=DROP_COLUMN_NAMES)
  
  # Convert dates to a numeric days between that and the date data collected
  listings<-ppConvertDatesToDaysBefore(listings)
  
  # Parse percentage, price, and boolean string fields as numeric values
  listings<-ppConvertPercentages(listings)
  listings<-ppConvertPrices(listings)
  listings<-ppConvertBooleans(listings)
  
  # Rename ugly field name
  names(listings)[names(listings)=="neighbourhoodgroupcleansed"]<-"neighbourhood"
  
  # Correct NA values
  listings<-ppHandleNaValues(listings, "hostsince", action="remove")
  listings<-ppHandleNaValues(listings, "hostresponserate", action="mean")
  listings<-ppHandleNaValues(listings, "bathrooms", action="setzero")
  listings<-ppHandleNaValues(listings, "bedrooms", action="setzero")
  listings<-ppHandleNaValues(listings, "beds", action="remove")
  listings<-ppHandleNaValues(listings, "securitydeposit", action="setzero")
  listings<-ppHandleNaValues(listings, "cleaningfee", action="setzero")
  listings<-ppHandleNaValues(listings, "firstreview", action="setzero")
  listings<-ppHandleNaValues(listings, "lastreview", action="setzero")
  listings<-ppHandleNaValues(listings, "reviewscoresrating", action="setzero") #?
  listings<-ppHandleNaValues(listings, "reviewspermonth", action="setzero")
  
  # Fill in missing weekly and monthly prices by multiplying out daily price
  listings$weeklyprice<-ifelse(is.na(listings$weeklyprice), 
                               listings$price*7, listings$weeklyprice)
  
  listings$monthlyprice<-ifelse(is.na(listings$monthlyprice), 
                                listings$price*30, listings$monthlyprice)
  
  # Convert weekly and monthly prices to percent of above
  listings$weeklyprice<-pmin(listings$weeklyprice/(listings$price*7), 1.0)
  listings$monthlyprice<-pmin(listings$monthlyprice/(listings$price*30), 1.0)
  
  # Cap maximum nights to prevent squashing when data normalised
  listings$maximumnights<-pmin(listings$maximumnights, 100)
  
  # Encode fields that are lists of categorical
  listings<-ppEncodeList(listings, "hostverifications", encodeAsCount=TRUE)
  listings<-ppEncodeList(listings, "amenities", encodeAsCount=FALSE)
  
  # Downsize to only three orders of bed: real bed, other, couch
  listings$bedtype<-ifelse(listings$bedtype!="Real Bed" & 
                             listings$bedtype!="Couch", "Other", listings$bedtype)
  
  # Encode categorical fields that are ordered (e.g. low/med/high)
  listings<-ppEncodeOrderedCategorical(listings, "cancellationpolicy",
                                       c("strict", "moderate", "flexible"))
  listings<-ppEncodeOrderedCategorical(listings, "hostresponsetime",
                                       c("N/A", "a few days or more", "within a day",
                                         "within a few hours", "within an hour"))
  listings<-ppHandleNaValues(listings, "hostresponsetime", action="setzero")
  listings<-ppEncodeOrderedCategorical(listings, "bedtype",
                                       c("couch", "other", "real bed"))
  
  # Encode the remaining unordered categorical fields
  listings<-ppEncodeRemainingCategoricals(listings)
  
  return(listings)
}

#*******************************************************************************
#*
#* Handles data reprocessing:
#*
#* Outlier removal
#* Normalization
#* redundant field removal
preprocess<-function(listings){
  
  print("Preprocessing: ")
  # Determine the type of field for all fields (NUMERIC or SYMBOLIC)
  listings_types<-PREPROCESSING_initalFieldType(listings)
  
  # For NUMERIC fields determine if these are continuous ORDINAL or DISCREET
  listings_types1<-PREPROCESSING_discreetNumeric(listings, listings_types, DISCREET_BINS)
  
  # Display the results
  results<-data.frame(field=names(listings), initial=listings_types, types1=listings_types1)
  print(formattable::formattable(results))
  
  # Determine if any of the Ordinal fields are outliers and replace the outlier with the mean of the field
  ordinals<-listings[, which(listings_types1==TYPE_ORDINAL)]
  ordinalReadyForML<-PREPROCESSING_outlier(ordinals, OUTLIER_CONF)
  
  # Normalise/Scale the Continuous [numeric] Ordinals
  
  ordinalReadyForML<-rescaleEntireFrame(ordinals)
  
  
  processed_listings<-cbind(listings, ordinalReadyForML)
  
  # remove redundant_fields
  print("Removing redundant fields...")
  processed_listings<-PREPROCESSING_redundantFields(processed_listings, CUT_OFF)
  
  
  return(processed_listings)
}

main<-function(){
  
  listings<-readDataset(DATASET_FILENAME)
  print(ncol(listings))
  
  c_listings<-clean(listings)
  print(ncol(c_listings))
  formattable::formattable(c_listings)
  
  
  p_listings<-preprocess(c_listings)
  print(ncol(p_listings))
  
  browser()
}

# ************************************************
# R execution

# Clear console
cat("\014")

# Load libraries here
library(pacman)
pacman::p_load(char=LIBRARIES, install=TRUE, character.only=TRUE)

# Load other R script files
source("functions_preprocessing.R")
source("lab_processing.R")

# Set seed to ensure consistent results across executions
set.seed(123)

# Begin program execution
main()
print("end")

