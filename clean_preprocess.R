
# Cleaning and pre-processing of datasets

# Author: Sam Shannon

# ************************************************
# Global Environment variables

CLEAN_PP_VERBOSE_OUTPUT <- FALSE # whether to output verbose info to console

DATASET_LISTINGS_FILE <- "listings.csv"

MAX_LITERALS      <- 50
OUTLIER_CONF      <- 0.95
CORRELATION_CONF  <- 0.90

DATE_COLLECTED    <- as.Date("04/01/2016", format="%d/%m/%Y")

# Fields to remove during initial cleaning that won't be useful for anything
DROP_FIELD_NAMES_CLEANING <- c(
  
  # URLS
  "listingurl", "thumbnailurl", "mediumurl", "pictureurl", "xlpictureurl",
  "hosturl", "hostthumbnailurl", "hostpictureurl",
  
  # Free text fields
  "name", "summary", "space", "description", "neighborhoodoverview", "notes",
  "transit", "hostname", "hostabout", "hostlocation",
  
  # Common sense / business knowledge removals
  "hosttotallistingscount", "street", "neighbourhood", "neighbourhoodcleansed",
  "city", "zipcode", "smartlocation", 
  "islocationexact", "squarefeet", "calendarupdated"
)

# Fields to remove after cleaning. These may be useful for data exploration,
# but were deemed unnecessary for models and removed to reduce dimensionality
DROP_FIELD_NAMES_PROCESSING <- c(
  
  # IDs
  "id",
  
  # Common sense / business knowledge / post-exploration removals
  "hosthasprofilepic", "hostacceptancerate", "availability60",
  "availability365", "reviewscoresaccuracy", "reviewscorescleanliness", "latitude", "longitude",
  "reviewscorescheckin", "reviewscorescommunication", "reviewscoreslocation",
  "reviewscoresvalue"
)

# ************************************************


# ************************************************
# Clean the listings.csv dataset
# Returns a cleaned data frame ready for data exploration
# ************************************************
cleanListings<-function(verbose=FALSE, plot=FALSE){
  
  CLEAN_PP_VERBOSE_OUTPUT <<- verbose
  
  listings<-readDataset(DATASET_LISTINGS_FILE)
  
  fieldCounter<-list()
  fieldCounter[["Initial"]]<-ncol(listings)
  
  # Remove fields that have identical values for all records
  listings<-ppRemoveIdenticalValueFields(listings)
  fieldCounter[["Remove identical"]]<-ncol(listings)
  
  # Manually remove many fields deemed to be unnecessary, for example free text,
  # unneeded unique id numbers, URLs, and ones deemed not needed by use of
  # common sense and/or business knowledge
  listings<-ppRemoveFields(listings, fieldNames=DROP_FIELD_NAMES_CLEANING)
  fieldCounter[["Remove unecessary"]]<-ncol(listings)
  
  # Convert dates to a numeric days between that and the date data collected
  listings<-ppConvertDatesToDaysBefore(listings)
  
  # Parse percentage and price boolean string fields as numeric values
  listings<-ppParsePercentages(listings)
  listings<-ppParsePrices(listings)
  
  # Rename ugly field name
  names(listings)[names(listings)=="neighbourhoodgroupcleansed"]<-"neighbourhood"
  
  # Replace blank values for host response time with N/A which is what the rest
  # of the unknown values are set to
  listings$hostresponsetime<-ifelse(listings$hostresponsetime=="", "N/A",
                                    listings$hostresponsetime)
  
  # Process list fields where record is a string of attributes (comma separated)
  # this effectively encodes them which would normally be done in pre-processing
  # but this step is needed in cleaning so that these attributes can be
  # extracted out of the string for use in data exploration
  listings<-ppExtractList(listings, "hostverifications", encodeAsCount=TRUE)
  listings<-ppExtractList(listings, "amenities", encodeAsCount=FALSE)
  fieldCounter[["Extract list fields"]]<-ncol(listings)
  
  if (plot==TRUE) {
    fieldCounter<-unlist(fieldCounter)
    graph<-barplot(fieldCounter, main="Number of fields after cleaning steps performed", cex.names=0.8, cex.lab=1.2, ylim=c(0,100), ylab="Number of fields")
    text(x=graph, y=fieldCounter+3, labels=as.character(fieldCounter), cex=1.2)
  }
  
  # Cleaning complete
  return(listings)
}


# ************************************************
# Pre process the listings dataset (cleans first)
# Returns a pre-processed data frame ready for machine learning
# ************************************************
processListings<-function(verbose=FALSE, rescale=TRUE, zscale=TRUE, plot=FALSE){
  
  CLEAN_PP_VERBOSE_OUTPUT <<- verbose
  
  listings<-cleanListings(verbose=verbose, plot=plot)
  
  fieldCounter<-list()
  fieldCounter[["From cleaning"]]<-ncol(listings)
  
  # ================================================
  # 1. Initial manual changes to data
  #
  #    - Remove some fields entirely
  #    - Handle NA values
  #    - Data transformations / tweaking / merging
  # ================================================
  
  # Remove some more manually specified fields
  listings<-ppRemoveFields(listings, fieldNames=DROP_FIELD_NAMES_PROCESSING)
  fieldCounter[["Remove unecessary"]]<-ncol(listings)
  
  # Handle NA values
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
  
  # Cap maximum nights to prevent squashing when data rescaled
  listings$maximumnights<-pmin(listings$maximumnights, 100)
  
  #Cap maximum price following Tukey's Fences :Q3+1.5 IQR
  listings$price<-pmin(listings$price, 262.5)
  
  # Downsize to only three orders of bed: real bed, other, couch
  if (plot) plotBarChartForField(listings$bedtype, "Bed types: before combining")
  listings$bedtype<-ifelse(listings$bedtype!="Real Bed" & 
                             listings$bedtype!="Couch", "Other", listings$bedtype)
  if (plot) plotBarChartForField(listings$bedtype, "Bed types: after combining")
  
  # A condo is just a type of apartment
  if (plot) plotBarChartForField(listings$propertytype, "Room types: before combining")
  listings$propertytype<-ifelse(listings$propertytype=="Condominium",
                                "Apartment", listings$propertytype)
  # There are too many property types, condense rare ones into "other" category
  for(literal in unique(listings$propertytype)){
    match<-listings$propertytype==literal
    count<-length(which(match))
    if(count<50){
      # too few, merge with 'other'
      listings$propertytype<-ifelse(match, 'Other', listings$propertytype)
    }
  }
  if (plot) plotBarChartForField(listings$propertytype, "Room types: after combining")
  
  
  # ================================================
  # 2. Split dataset by ordinal/categorical fields
  # ================================================
  isNumericField<-sapply(listings, is.numeric)
  ordinals<-listings[isNumericField]
  categoricals<-listings[!isNumericField]
  
  
  # ================================================
  # 3. Prepare categoric fields ready for ML
  # ================================================
  
  # Encode categorical fields that are ordered (e.g. low/med/high)
  categoricals<-ppEncodeOrderedCategorical(categoricals, "cancellationpolicy",
                                           c("strict", "moderate", "flexible"))
  categoricals<-ppEncodeOrderedCategorical(categoricals, "hostresponsetime",
                                           c("N/A", "a few days or more",
                                             "within a day", "within a few hours",
                                             "within an hour"))
  categoricals<-ppEncodeOrderedCategorical(categoricals, "bedtype",
                                           c("couch", "other", "real bed"))
  
  fieldCounter[["Encode ordered categ."]]<-ncol(ordinals)+ncol(categoricals)
  
  # Encode the remaining unordered categorical fields
  categoricalsReadyForML<-ppEncodeRemainingCategoricals(categoricals)
  
  fieldCounter[["Encode unordered categ."]]<-ncol(ordinals)+ncol(categoricalsReadyForML)
  
  
  # ================================================
  # 4. Prepare ordinal fields ready for ML
  # ================================================
  
  # Detect and fix outliers
  ordinals<-NPREPROCESSING_outlier(ordinals=ordinals, confidence=OUTLIER_CONF,
                                   plot=FALSE)
  
  # Z-scale and rescale ordinal fields
  if(zscale){
    ordinals<-as.data.frame(scale(ordinals, center=TRUE, scale=TRUE))
  }
  if(rescale){
    ordinals<-Nrescaleentireframe(ordinals)
  }
  
  ordinalsReadyForML<-ordinals
  
  
  # ================================================
  # 5. Combine back into one data frame and remove correlated fields
  # ================================================
  combinedML<-cbind(ordinalsReadyForML, categoricalsReadyForML)
  
  # Remove redundant fields
  combinedML<-NPREPROCESSING_redundantFields(dataset=combinedML, cutoff=CORRELATION_CONF)
  fieldCounter[["Remove redundant"]]<-ncol(combinedML)
  
  fieldCounter<-unlist(fieldCounter)
  par(mar=c(11,6,4,2))
  if (plot==TRUE) {
    graph<-barplot(fieldCounter, main="Number of fields after pre-processing steps performed", cex.names=1.0, cex.lab=1.2, ylim=c(0,110), ylab="Number of fields", las=2)
    text(x=graph, y=fieldCounter+3, labels=as.character(fieldCounter), cex=1.2)
  }
  
  
  print(paste0("Dataset cleaning & pre-processing complete (rescale=", rescale, ", zscale=", zscale, ") Fields= ", ncol(combinedML), " Records= ", nrow(combinedML)))
  return(combinedML)
}


# ************************************************
# R execution

# Load other R script files
source("functions_preprocessing.R")
