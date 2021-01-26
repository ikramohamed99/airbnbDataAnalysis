
# Functions to do with cleaning and pre-processing of dataset

# Author: Sam Shannon

# Some lab functions used or modified and noted in comments where this is the case


if (!exists("CLEAN_PP_VERBOSE_OUTPUT")) {
  CLEAN_PP_VERBOSE_OUTPUT <- FALSE
}

# ************************************************
# readDataset() :
#
# Read a CSV file from the working directory and
# remove punctuation from field names
#
# INPUT: string - csvFilename - CSV filename
#
# OUTPUT: data frame - contents of the loaded CSV file
#
# SOURCE: based heavily upon Prof Nick's NreadDataset 
#         and NPREPROCESSING_removePunctuation functions
# ************************************************
readDataset<-function(csvFilename){
  
  # load in csv file
  dataset<-read.csv(csvFilename, encoding="UTF-8", stringsAsFactors=FALSE)
  
  # remove punctuation from field names
  names(dataset)<-gsub("[[:punct:][:blank:]]+", "", names(dataset))
  
  # output some basic info
  print(paste("CSV dataset", csvFilename, "has been read. Fields=",
              ncol(dataset), "Records=", nrow(dataset)))
  
  return(dataset)
}


# Plots a bar chart showing the frequency of classes in a field
# (for categorical fields)
plotBarChartForField<-function(field, title, showvals=FALSE){
  field<-table(field)
  las<-ifelse(length(field) > 5, 2, 0)
  names(field)<-replace(names(field), names(field)=="", "[EMPTY]")
  par(mar=c(10,6,4,2))
  graph<-barplot(field, main=title, ylab="Occurences in field", col="seagreen3", cex.lab=1.5, ylim=c(0, max(field) + 100), las=las)
  if (showvals) {
    text(x=graph, y=field+70, labels=as.character(field), cex=1.2)
  }
}


# ************************************************
# ppHandleNaValues() :
#
# Handle NA values for a field by taking an action of replacing NA values
# with 0, the mean, the median, or removing the entire record
#
# INPUT: data frame - dataset   - input dataset
#        string     - fieldName - name of field to take action for
#        string     - action    - action to take on NA values ("setzero", "mean", "median", "remove") are valid options
#
# OUTPUT: data frame - dataset with changes applied
# ************************************************
ppHandleNaValues<-function(dataset, fieldName, action="setzero"){
  action<-tolower(action)
  field<-dataset[[fieldName]]
  count<-length(which(is.na(field)))
  
  # replace NA values with the appropriate value
  if(action=="setzero"){
    dataset[fieldName]<-ifelse(is.na(field), 0, field)
    if (CLEAN_PP_VERBOSE_OUTPUT) print(paste0("Replaced ", count, " NA values with 0 for field ", fieldName))
    return(dataset)
  }
  if(action=="mean"){
    dataset[fieldName]<-ifelse(is.na(field), mean(field, na.rm=TRUE), field)
    if (CLEAN_PP_VERBOSE_OUTPUT) print(paste0("Replaced ",count," NA values with MEAN for field ",fieldName))
    return(dataset)
  }
  if(action=="median"){
    dataset[fieldName]<-ifelse(is.na(field), median(field, na.rm=TRUE), field)
    if (CLEAN_PP_VERBOSE_OUTPUT) print(paste0("Replaced ", count, " NA values with MEDIAN for field ",
                 fieldName))
    return(dataset)
  }
  if(action=="remove"){
    dataset<-dataset[!is.na(field),]
    if (CLEAN_PP_VERBOSE_OUTPUT) print(paste0("Removed ", count, " NA values for field ", fieldName))
    return(dataset)
  }
}


# ************************************************
# ppConvertDatesToDaysBefore() :
#
# Detects fields that are a date string and parses the date and then converts
# it into an integer value of the number days before the data was collected
#
# INPUT: data frame - dataset   - input dataset
#        date       - baseDate  - the date on which the data was collected
#
# OUTPUT: data frame - dataset with changes applied
# ************************************************
ppConvertDatesToDaysBefore<-function(dataset, baseDate=DATE_COLLECTED){
  for(i in 1:ncol(dataset)){
    field<-dataset[,i]
    
    # check that every record is a date or NA or empty
    if(all(grepl("^\\d{4}-\\d{2}-\\d{2}", field) | is.na(field) | field=="", perl=TRUE)){
      
      # convert date strings to days before the specified date
      # (usually the date the data was collected)
      dataset[,i]<-as.integer(baseDate-as.Date(field))
      
      if (CLEAN_PP_VERBOSE_OUTPUT)print(paste("Converted field", names(dataset)[i],
                  "from date string to days-before-date numeric"))
    }
    
  }
  return(dataset)
}


# ************************************************
# ppConvertPercentages() :
#
# Converts fields that are percentages (e.g. "100%") into a numeric (e.g. 100)
#
# INPUT: data frame - dataset - input dataset
#
# OUTPUT: data frame - the input dataset with any fields it determines are
#                      percentages changed to be numeric
# ************************************************
ppParsePercentages<-function(dataset){
  for(i in 1:ncol(dataset)){
    field<-dataset[,i]
    
    # check that every record is either a percentage or "N/A" or NA or empty
    if(all(grepl("^(\\d+%|N/A)", field) | is.na(field) | field=="")){
      
      # remove percent sign and coerce to numeric
      dataset[,i]<-suppressWarnings(as.numeric(sub("%", "", field))/100)
      
      if (CLEAN_PP_VERBOSE_OUTPUT) print(paste("Converted field", names(dataset)[i],
                  "from raw percentage string to numeric"))
    }
  }
  return(dataset)
}


# ************************************************
# ppConvertPrices() :
#
# Converts fields that are prices (e.g. "$50.00") into a numeric (e.g. 50.0)
#
# INPUT: data frame - dataset - input dataset
#
# OUTPUT: data frame - the input dataset with any fields it determines are
#                      price strings changed to be numeric
# ************************************************
ppParsePrices<-function(dataset){
  for(i in 1:ncol(dataset)){
    field<-dataset[,i]
    
    # check that every record matches a price pattern or is NA or empty
    if(all(grepl("^[$£€]\\d+(.\\d+)?", field) | is.na(field) | field=="")){
      
      # remove symbols and coerce to numeric, divide by 100 if it had a . otherwise don't
      n<-as.numeric(gsub("[[:punct:]]+", "", field))
      dataset[,i]<-(n / ifelse(grepl(".", field), 100, 1))
      
      if (CLEAN_PP_VERBOSE_OUTPUT) print(paste("Converted field", names(dataset)[i],
                  "from raw price string to numeric"))
    }
  }
  return(dataset)
}


# ************************************************
# ppConvertBooleans() :
#
# Converts fields that are booleans (e.g. "t" or "f") into a numeric (1/0)
#
# INPUT: data frame - dataset - input dataset
#
# OUTPUT: data frame - the input dataset with any fields it determines are
#                      booleans changed to be numeric
# ************************************************
ppConvertBooleans<-function(dataset){
  for(i in 1:ncol(dataset)){
    field<-dataset[,i]
    
    # check that every record is either T or F
    if(all(grepl("^[tf]", field, ignore.case=TRUE) | is.na(field) | field=="")){
      
      # convert to 1 for true, 0 for false
      field<-sub("t", 1, field)
      field<-sub("f", 0, field)
      dataset[,i]<-as.numeric(field)
      
      if (CLEAN_PP_VERBOSE_OUTPUT) print(paste("Converted field", names(dataset)[i],
                  "from raw boolean string to numeric"))
    }
  }
  return(dataset)
}


# ************************************************
# ppRemoveIdenticalValueFields() :
#
# Removes fields for which every record value is identical
# i.e. when there is only one unique value for the entire field
#
# INPUT: data frame - dataset - input dataset
#
# OUTPUT: data fame - the input dataset with the identical fields removed
# ************************************************
ppRemoveIdenticalValueFields<-function(dataset){
  removed<-vector()
  for(field in 1:ncol(dataset)){
    if(length(unique(tolower(dataset[, field])))==1){
      removed<-append(removed, field)
    }
  }
  dataset<-dataset[,-removed]
  if (CLEAN_PP_VERBOSE_OUTPUT) print(paste("Removed", length(removed),
              "field(s) that had identical values for all records"))
  return(dataset)
}


# ************************************************
# ppRemoveFields() :
#
# Remove specified fields from a dataset by name
#
# INPUT: data frame - dataset - input dataset
#        vector string - fieldNames - names of columns to drop from the dataset
#
# OUTPUT: data frame - the input dataset with the specified columns removed
# ************************************************
ppRemoveFields<-function(dataset, fieldNames){
  
  # subset the existing dataset, removing the specified fields
  dataset<-dataset[, -which(names(dataset) %in% fieldNames)]
  
  # output the number of fields removed
  if (CLEAN_PP_VERBOSE_OUTPUT) print(paste("Removed", length(fieldNames), "manually specified field(s)"))
  
  return(dataset)
}


# ************************************************
# ppExtractList() :
#
# Handles fields that are a list of categorical values, for example in this format:
# ['email', 'phone', 'reviews', 'kba']
# And extracts them into individual fields so that they can be used
#
# Optionally, it can instead be converted to an integer count of the number of
# items in the list, which doesn't increase the number of fields in the dataset
#
# INPUT: data frame - dataset       - input dataset
#        string     - fieldName     - the name of the field to process
#        boolean    - encodeAsCount - TRUE to extract into individual fields, FALSE to convert to integer count
#
# OUTPUT: data frame - dataset with changes applied
# ************************************************
ppExtractList<-function(dataset, fieldName, encodeAsCount=FALSE){
  
  field<-dataset[[fieldName]]
  
  # find the unique values
  # have to iterate though every field to ensure all values are identified
  # as there is no data dictionary
  values<-vector()
  for(i in 1:length(field)){
    record<-field[[i]]
    if(record != "None" & !is.na(record)){
      s<-gsub("[[:punct:][:blank:]]+", "", unlist(strsplit(record, ",")))
      values<-append(values, s[which(!(s %in% values))])
    }
  }
  values<-values[values != ""] # remove "" value if it's in there
  
  fieldSplit<-strsplit(gsub("([^,[:^punct:]]|[[:blank:]])", "", field,
                            perl=TRUE), ",")
  
  if(encodeAsCount){
    
    # replace the list with a numeric count of how many items were in the list
    field<-unlist(lapply(fieldSplit, function(x) length(strsplit(x, ","))))
    dataset[fieldName]<-field
    return(dataset)
    
  } else {
    
    # extract list values into a yes/no whether present
    # (not 1/0 because just cleaning at this stage; keep categorical)
    for(value in values){
      hotEncoding<-ifelse(unlist(lapply(fieldSplit, function(x) value %in% x)),
                          1, 0)
      dataset<-cbind(dataset, hotEncoding)
      dataset<-dataset[, which(names(dataset)!=fieldName)]
      cleanName<-gsub("[[:punct:][:blank:]]+", "", tolower(value))
      names(dataset)[ncol(dataset)]<-paste0(fieldName, "_", cleanName)
    }
  }

  return(dataset)
}


# ************************************************
# ppEncodeOrderedCategorical() :
#
# Encodes a field as an ordered categorical
# with each unique literal having a different number
#
# INPUT: data frame    - dataset       - input dataset
#        string        - fieldName     - the name of the field to process
#        string vector - encodeAsCount - vector of literal values in order, first will be assigned 
#                                        lowest value and last will be assigned highest value
#
# OUTPUT: data frame - dataset with changes applied
# ************************************************
ppEncodeOrderedCategorical<-function(dataset, fieldName, orders){
  
  # determine value to assign to each literal
  orders<-tolower(orders)
  names(orders)<-orders
  total<-length(orders)
  for(i in 1:total){
    orders[i]<-round(i/total, digits=2)
  }
  
  # replace literals with corresponding ordered numeric
  field<-dataset[[fieldName]]
  field<-sapply(field, function(x) as.numeric(orders[tolower(x)]))
  dataset[, which(names(dataset)==fieldName)]<-field
  
  # ensure field is now numeric
  if(!is.numeric(field)) {
    print(paste("WARNING - ordered categorical", fieldName,
                "not numeric after encoding!"))
  } else {
    if (CLEAN_PP_VERBOSE_OUTPUT) print(paste("Encoded field", fieldName, "as ordered categorical"))
  }
  return(dataset)
}


# ************************************************
# ppEncodeRemainingCategoricals() :
#
# Encodes categorical fields using one-hot encoding
#
# INPUT: data frame    - dataset       - input dataset
#
# OUTPUT: data frame - dataset with categorical fields one-hot encoded
# ************************************************
ppEncodeRemainingCategoricals<-function(dataset){
  
  # remove fields with too many unique literals
  removed<-vector()
  for(i in 1:ncol(dataset)){
    field<-dataset[,i]
    if(!is.numeric(field)){
      if(length(unique(field)) > MAX_LITERALS){
        removed<-append(removed, i)
        if (CLEAN_PP_VERBOSE_OUTPUT) print(paste("Removed field", names(dataset)[i], "for having too many literals"))
      }
    }
  }
  dataset<-dataset[,-removed]
  
  remove<-vector()
  # encode remaining categoricals
  for(i in 1:ncol(dataset)){
    field<-dataset[,i]
    if(!is.numeric(field)){
      literals<-tolower(unique(field))
      
      if(length(literals)==2){
        
        # For readability when debugging, ensure 1=yes/true, 0=no/false
        # when the literals are yes/no or t/f
        if(literals[1]=="yes" | literals[1]=="no"){
          literals[1]<-"no"
          literals[2]<-"yes"
        }
        if(literals[1]=="t" | literals[1]=="f"){
          literals[1]<-"f"
          literals[2]<-"t"
        }
        
        dataset[,i]<-ifelse(field==literals[1], 0, 1)
          
      } else {
        
        # hot encode
        for(literal in literals){
          hotEncoding<-ifelse(tolower(field)==literal, 1, 0)
          dataset<-cbind(dataset, hotEncoding)
          cleanName<-gsub("[[:punct:][:blank:]]+", "", literal)
          names(dataset)[ncol(dataset)]<-paste0(names(dataset)[i], "_", cleanName)
        }
        remove<-append(remove, i)
        if (CLEAN_PP_VERBOSE_OUTPUT) print(paste0("Encoded field ", names(dataset)[i],
                     " as unordered categorical (", length(literals), " literals)"))
        
      }
    }
  }
  dataset<-dataset[,-remove]
  
  return(dataset)
}


# ************************************************
# ppWhitelistAmenities() :
#
# Keeps only the specified amenities and drops the other ones in the dataset
# (for purposes of dimensionality reduction)
#
# INPUT: data frame    - dataset       - input dataset
#        string vector - whitelist     - vector of names of amenities to remove
#
# OUTPUT: data frame - dataset with all amenities but those specified removed
# ************************************************
ppWhitelistAmenities<-function(dataset, whitelist){
  for (fieldName in names(dataset)) {
    if (grepl("^amenities_", fieldName)) {
      if (!(fieldName %in% whitelist)) {
        dataset<-ppRemoveFields(dataset, c(fieldName))
      }
    }
  }
  return(dataset)
}


# ************************************************
# ppRemoveNeighbourhoods() :
#
# Removes neighbourhood fields from the dataset
# (for purposes of dimensionality reduction)
#
# INPUT: data frame    - dataset       - input dataset
#
# OUTPUT: data frame - dataset with all neighbourhood fields removed
# ************************************************
ppRemoveNeighbourhoods<-function(dataset){
  for (fieldName in names(dataset)) {
    if (grepl("^neighbourhood_", fieldName)) {
      dataset<-ppRemoveFields(dataset, c(fieldName))
    }
  }
  return(dataset)
}


# ************************************************
# NPREPROCESSING_outlier() :
#
# Determine if a value of a record is an outlier for each field
#
# INPUT:   data frame - ordinals   - numeric fields only
#          double     - confidence - Confidence above which is determined an outlier [0,1]
#                                  - Set to negative Confidence if NOT remove outliers
#
# OUTPUT : data frame - ordinals with any outlier values replaced with the median of the field
#
# Note: based upon NPREPROCESSING_outlier() from lab 3
# ************************************************
# ChiSquared method
# Uses   library(outliers)
# https://cran.r-project.org/web/packages/outliers/outliers.pdf
NPREPROCESSING_outlier<-function(ordinals, confidence, plot=FALSE){
  
  #For every ordinal field in our dataset
  for(field in 1:(ncol(ordinals))){
    
    sorted<-unique(sort(ordinals[,field],decreasing=TRUE))
    outliers<-which(outliers::scores(sorted,type="chisq",prob=abs(confidence)))
    
    if (plot) {
      NplotOutliers(sorted,outliers,colnames(ordinals)[field])
    }
    
    #If found records with outlier values
    if ((length(outliers>0))){
      
      # If confidence is positive then replace values with their means, otherwise do nothing
      if (confidence>0){
        outliersGone<-rm.outlier(ordinals[,field],fill=TRUE)
        sorted<-unique(sort(outliersGone,decreasing=TRUE))
        #NplotOutliers(sorted,vector(),colnames(ordinals)[field])
        ordinals[,field]<-outliersGone #Put in the values with the outliers replaced by means
        if (CLEAN_PP_VERBOSE_OUTPUT) print(paste("Outlier field=",names(ordinals)[field],"Records=",length(outliers),"Replaced with MEAN"))
      } else {
        if (CLEAN_PP_VERBOSE_OUTPUT) print(paste("Outlier field=",names(ordinals)[field],"Records=",length(outliers)))
      }
    }
    
  }
  return(ordinals)
}


# ************************************************
# NplotOutliers() :
#
# Scatter plot of field values and colours outliers in red
#
# INPUT: Vector - sorted    -  points to plot as literal values
#        Vector - outliers  - list of above points that are considered outliers
#        String - fieldName - name of field to plot
#
# OUTPUT : None
#
# Note: unmodified copy of NplotOutliers() from lab 3
# ************************************************
NplotOutliers<-function(sorted,outliers,fieldName){
  
  plot(1:length(sorted),sorted,pch=1,xlab="Unique records",ylab=paste("Sorted values",fieldName),bty="n")
  if (length(outliers)>0)
    points(outliers,sorted[outliers],col="red",pch=19)
}


# ************************************************
# Nrescale() :
#
# These are the real values, that we scale between 0-1
# i.e. x-min / (max-min)
#
# INPUT:   vector - input - values to scale
#
# OUTPUT : vector - scaled values to [0.0,1.0]
#
# Note: unmodified copy of Nrescale() from lab 3
# ************************************************
Nrescale<-function(input){
  
  minv<-min(input)
  maxv<-max(input)
  return((input-minv)/(maxv-minv))
}


# ************************************************
# Nrescaleentireframe() :
#
# Rescle the entire dataframe to [0.0,1.0]
#
# INPUT:   data frame - dataset - numeric data frame
#
# OUTPUT : data frame - scaled numeric data frame
#
# Note: unmodified copy of Nrescaleentireframe() from lab 3
# ************************************************
Nrescaleentireframe<-function(dataset){
  
  scaled<-sapply(as.data.frame(dataset),Nrescale)
  return(scaled)
}


# ************************************************
# NPREPROCESSING_redundantFields() :
#
# Determine if an entire field is redundant
# Uses LINEAR correlation,
# so use with care as information will be lost
#
# INPUT: data frame - dataset - numeric values only
#        double     - cutoff  - Value above which is determined redundant [0,1]
#
# OUTPUT : Frame - dataset with any fields removed
#
# Note: unmodified copy of NPREPROCESSING_redundantFields() from lab 3
# ************************************************
NPREPROCESSING_redundantFields<-function(dataset,cutoff){
  
  if (CLEAN_PP_VERBOSE_OUTPUT) print(paste("Before redundancy check Fields=",ncol(dataset)))
  
  #Remove any fields that have a stdev of zero (i.e. they are all the same)
  xx<-which(apply(dataset, 2, function(x) sd(x, na.rm=TRUE))==0)+1
  
  if (length(xx)>0L)
    dataset<-dataset[,-xx]
  
  #Kendall is more robust for data do not necessarily come from a bivariate normal distribution.
  cr<-cor(dataset, use="everything")
  #cr[(which(cr<0))]<-0 #Positive correlation coefficients only
  NPLOT_correlagram(cr)
  
  correlated<-which(abs(cr)>=cutoff,arr.ind = TRUE)
  list_fields_correlated<-correlated[which(correlated[,1]!=correlated[,2]),]
  
  if (nrow(list_fields_correlated)>0){
    
    if (CLEAN_PP_VERBOSE_OUTPUT) {
      print("Following fields are correlated")
      print(list_fields_correlated)
      
      # 240220nrt print list of correlated fields as namesß
      for (i in 1:nrow(list_fields_correlated)){
        print(paste(names(dataset)[list_fields_correlated[i,1]],"~", names(dataset)[list_fields_correlated[i,2]]))
      }
    }
    
    #We have to check if one of these fields is correlated with another as cant remove both!
    v<-vector()
    numc<-nrow(list_fields_correlated)
    for (i in 1:numc){
      if (length(which(list_fields_correlated[i,1]==list_fields_correlated[i:numc,2]))==0) {
        v<-append(v,list_fields_correlated[i,1])
      }
    }
    
    if (CLEAN_PP_VERBOSE_OUTPUT) {
      print("Removing the following fields")
      print(names(dataset)[v])
    }
    
    return(dataset[,-v]) #Remove the first field that is correlated with another
  }
  return(dataset)
}


# ************************************************
# NPLOT_correlagram() :
#
# Plots PLOT_correlagram
#
# INPUT: data frame - cr - n x n frame of correlation coefficients
#
# OUTPUT : None
#
# Note: unmodified copy of NPLOT_correlagram() from lab 3
# ************************************************
NPLOT_correlagram<-function(cr){
  
  #Defines the colour range
  col<-colorRampPalette(c("green", "red"))
  
  #To fir on screen, convert field names to a numeric
  rownames(cr)<-1:length(rownames(cr))
  colnames(cr)<-rownames(cr)
  
  corrplot::corrplot(abs(cr),method="square",
                     order="FPC",
                     cl.ratio=0.2,
                     cl.align="r",
                     tl.cex = 0.6,cl.cex = 0.6,
                     cl.lim = c(0, 1),
                     mar=c(1,1,1,1),bty="n")
}
