
# Data exploration

# Author: Ikra Mohamed

listingsExploration<-function(){
  
  listings_cl<-cleanListings()
  
  #Preview data
  head(listings_cl)
  summary(listings_cl)
  cat("Shape of the dataset:" ,dim(listings_cl))
  
  
  ## Look into variables of interest
  summary(listings_cl$price)
  summary(listings_cl$availability30)
  summary(listings_cl$calculatedhostlistingscount)
  
  
  #Distribution of types of property and types of room 
  #Insight into what types of listings exist in seattle
  roomtype <-  listings_cl %>%
    count(roomtype, sort = TRUE) %>%
    mutate(roomtype = reorder(roomtype, n)) %>%
    ggplot(aes(roomtype, n, fill="green")) +
    geom_col(fill="blue") +
    xlab("Type of room") +
    ylab("Number of listings") +
    coord_flip()
  
  propertytype <- listings_cl %>%
    count(propertytype, sort = TRUE) %>%
    filter(n > 0) %>%
    mutate(propertytype = reorder(propertytype, n)) %>%
    ggplot(aes(propertytype, n)) +
    geom_col(fill="green") +
    xlab("Type of property") +
    ylab("Number of listings") +
    coord_flip()
  
  #Listings by neighbourhood
  neighbourhoodtype<-listings_cl %>%
    count(neighbourhood, sort = TRUE) %>%
    filter(n > 150) %>%
    mutate(neighbourhood = reorder(neighbourhood, n)) %>%
    ggplot(aes(neighbourhood, n)) +
    geom_col(fill="red") +
    xlab("Neighbourhood types in Seattle") +
    ylab("Number of listings") +
    coord_flip()
  
  grid.arrange(roomtype, propertytype,neighbourhoodtype ,nrow = 1)
  
  
  
  #Scatter plot of listings in seattle by neighbourhoods
  
  scatterPlot1 <- ggplot(listings_cl,aes(x = longitude, y = latitude, fill=neighbourhood, 
                                         text = paste(
                                           "Neighbourhood :", neighbourhood, "\n",
                                           "Property type :", propertytype, "\n",
                                           "Price :", price, "\n"))) + geom_point(listings_cl,mapping=aes(x = longitude, y = latitude, color=neighbourhood), alpha=1, size=0.5) + labs(x ="Longitude", y ="Latitude",title = "AirBnB Listings Across Seattle") +theme_bw()
  print(ggplotly(scatterPlot1, tooltip = "text"))
  
  
  #Distribution of types of rooms and properties in seattle
  room_type_location <- ggplot(data = listings_cl) +
    geom_point(mapping = aes(x = longitude, y = latitude, color=roomtype)) + labs(x ="Longitude", y ="Latitude",title = "Airbnb room types in Seattle") +
    labs(color = 'Room Type') +
    theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank())
  
  prop_type_location <- listings_cl %>% 
    filter(propertytype %in% c('House', 'Apartment')) %>% 
    ggplot() +
    geom_point(mapping = aes(x = longitude, y = latitude, color=propertytype)) + labs(x ="Longitude", y ="Latitude",title = "Airbnb property types in Seattle") +
    labs(color = 'Property Type') +
    theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank())
  
  grid.arrange(room_type_location, prop_type_location)
  
  #Distribution -not report friendly 
  #How price (per night) vary across Seattle neighbourhoods
  listings_cl %>%
    mutate(price = as.numeric(gsub("\\$", "", price))) %>%
    ggplot() +
    geom_histogram(mapping = aes(price, fill=neighbourhood), binwidth=10) +
    labs(fill = NULL +
           theme(legend.position = "none",
                 plot.title = element_text(color = "black", size = 14, face = "bold", hjust = 0.5),
                 plot.subtitle = element_text(color = "black", hjust = 0.5),
                 axis.title.y = element_text(),
                 axis.title.x = element_text(),
                 axis.ticks = element_blank()))
  
  
  #Mean price comparison for neighbourhoods 
  mpc_neighbourhoods<-listings_cl %>% 
    filter(!(is.na(neighbourhood))) %>% 
    filter(!(neighbourhood == "Unknown")) %>% 
    group_by(neighbourhood) %>% 
    summarise(mean_price = mean(price, na.rm = TRUE)) %>% 
    ggplot(aes(x = reorder(neighbourhood, mean_price), y = mean_price, fill = neighbourhood)) +
    geom_col(stat ="identity", color = "black", fill="orange") +
    coord_flip() +
    theme_gray() +
    labs(x = "Neighbourhood Group", y = "Price") +
    geom_text(aes(label = round(mean_price,digit = 2)), hjust = 2.0, color = "white", size = 3.5) +
    ggtitle("Mean Price comparison for each Neighbourhood Group", subtitle = "Price vs Neighbourhood Group") + 
    xlab("Neighbourhood Group") + 
    ylab("Mean Price") +
    theme(legend.position = "none",
          plot.title = element_text(color = "black", size = 14, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(color = "black", hjust = 0.5),
          axis.title.y = element_text(),
          axis.title.x = element_text(),
          axis.ticks = element_blank())
  #Mean price comparison with room types
  mpc_roomtypes<-listings_cl %>% 
    filter(!(is.na(roomtype))) %>% 
    filter(!(roomtype == "Unknown")) %>% 
    group_by(roomtype) %>% 
    summarise(mean_price = mean(price, na.rm = TRUE)) %>% 
    ggplot(aes(x = reorder(roomtype, mean_price), y = mean_price, fill = roomtype)) +
    geom_col(stat ="identity", color = "black", fill="red") +
    coord_flip() +
    theme_gray() +
    labs(x = "Room Type", y = "Price") +
    geom_text(aes(label = round(mean_price,digit = 2)), hjust = 2.0, color = "white", size = 3.5) +
    ggtitle("Mean Price comparison with all Room Types", subtitle = "Price vs Room Type") + 
    xlab("Room Type") + 
    ylab("Mean Price") +
    theme(legend.position = "none",
          plot.title = element_text(color = "black", size = 14, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(color = "darkblue", hjust = 0.5),
          axis.title.y = element_text(),
          axis.title.x = element_text(),
          axis.ticks = element_blank())
  #Mean price comparison for bedtypes
  mpc_bedtypes<- listings_cl %>% 
    filter(!(is.na(bedtype))) %>% 
    filter(!(bedtype == "Unknown")) %>% 
    group_by(bedtype) %>% 
    summarise(mean_price = mean(price, na.rm = TRUE)) %>% 
    ggplot(aes(x = reorder(bedtype, mean_price), y = mean_price, fill = bedtype)) +
    geom_col(stat ="identity", color = "black", fill="purple") +
    coord_flip() +
    theme_gray() +
    labs(x = "Room Type", y = "Price") +
    geom_text(aes(label = round(mean_price,digit = 2)), hjust = 2.0, color = "white", size = 3.5) +
    ggtitle("Mean Price comparison with all Bed Types", subtitle = "Price vs Bed Type") + 
    xlab("Bed Type") + 
    ylab("Mean Price") +
    theme(legend.position = "none",
          plot.title = element_text(color = "black", size = 14, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(color = "darkblue", hjust = 0.5),
          axis.title.y = element_text(),
          axis.title.x = element_text(),
          axis.ticks = element_blank())
  grid.arrange(mpc_bedtypes,mpc_roomtypes)
  
  
  #Boxplot for neighbourhoods vs price
  bp_price_neighbourhoods <- ggplot(data = listings_cl, aes(x = neighbourhood, y = price, color = neighbourhood))+geom_boxplot(outlier.shape = NA) +theme(axis.text.x = element_text(angle = 90, hjust = 1))+coord_cartesian(ylim = c(0, 750))
  
  #Make this a whole number 
  #Mean availability comparison for neighbourhoods 
  mac_neighbourhoods<- listings_cl %>% 
    filter(!(is.na(neighbourhood))) %>% 
    filter(!(neighbourhood == "Unknown")) %>% 
    group_by(neighbourhood) %>% 
    summarise(mean_availability30 = mean(availability30, na.rm = TRUE)) %>% 
    ggplot(aes(x = reorder(neighbourhood, mean_availability30), y = mean_availability30, fill = neighbourhood)) +
    geom_col(stat ="identity", color = "black", fill="purple") +
    coord_flip() +
    theme_gray() +
    labs(x = "Neighbourhood Group", y = "Price") +
    geom_text(aes(label = round(mean_availability30,digit = 2)), hjust = 2.0, color = "white", size = 3.5) +
    ggtitle("Mean Availability (30 days) comparison for each Neighbourhood Group", subtitle = "Availability vs Neighbourhood Group") + 
    xlab("Neighbourhood Group") + 
    ylab("Mean avilability") +
    theme(legend.position = "none",
          plot.title = element_text(color = "black", size = 14, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(color = "black", hjust = 0.5),
          axis.title.y = element_text(),
          axis.title.x = element_text(),
          axis.ticks = element_blank())
  grid.arrange(mac_neighbourhoods,mpc_neighbourhoods)
  
  #Boxplot for neighbourhoods vs availability
  bp_availability_neighbourhoods<-ggplot(data = listings_cl, aes(x = neighbourhood, y = availability30, color = neighbourhood))+geom_boxplot(outlier.shape = NA) +theme(axis.text.x = element_text(angle = 90, hjust = 1))+coord_cartesian(ylim = c(0, 30))
  
  grid.arrange(bp_availability_neighbourhoods, bp_price_neighbourhoods, nrow=3)
  
  #Mean hostlistingscount comparison for neighbourhoods 
  listings_cl %>% 
    filter(!(is.na(neighbourhood))) %>% 
    filter(!(neighbourhood == "Unknown")) %>% 
    group_by(neighbourhood) %>% 
    summarise(mean_hlc = mean(calculatedhostlistingscount, na.rm = TRUE)) %>% 
    ggplot(aes(x = reorder(neighbourhood, mean_hlc), y = mean_hlc, fill = neighbourhood)) +
    geom_col(stat ="identity", color = "black", fill="blue") +
    coord_flip() +
    theme_gray() +
    labs(x = "Neighbourhood Group", y = "Price") +
    geom_text(aes(label = round(mean_hlc,digit = 2)), hjust = 2.0, color = "white", size = 3.5) +
    ggtitle("Mean number of homes a host owns for each Neighbourhood Group", subtitle = "Host listings count (HLC) vs Neighbourhood Group") + 
    xlab("Neighbourhood Group") + 
    ylab("Mean HLC") +
    theme(legend.position = "none",
          plot.title = element_text(color = "black", size = 14, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(color = "black", hjust = 0.5),
          axis.title.y = element_text(),
          axis.title.x = element_text(),
          axis.ticks = element_blank())
  #Boxplot for neighbourhoods vs hostlistingscount
  print(ggplot(data = listings_cl, aes(x = neighbourhood, y = calculatedhostlistingscount, color = neighbourhood))+geom_boxplot(outlier.shape = NA) +theme(axis.text.x = element_text(angle = 90, hjust = 1))+coord_cartesian(ylim = c(0, 40)))
  
  
  #Boxplot for roomtypes vs price
  print(ggplot(data = listings_cl, aes(x = roomtype, y = price, color = roomtype)) +geom_boxplot(outlier.shape = NA) +theme(axis.text.x = element_text(angle = 90, hjust = 1))+coord_cartesian(ylim = c(0, 750)))
  
  
  #Boxplot for bedtypes vs price
  print(ggplot(data = listings_cl, aes(x = bedtype, y = price, color = bedtype)) +geom_boxplot(outlier.shape = NA) +theme(axis.text.x = element_text(angle = 90, hjust = 1))+coord_cartesian(ylim = c(0, 750)))
  
  #Boxplot for roomtypes vs availability30
  avail_room_bp<- ggplot(data = listings_cl, aes(x = roomtype, y = availability30, color = roomtype)) +geom_boxplot(outlier.shape = NA) +theme(axis.text.x = element_text(angle = 90, hjust = 1))+coord_cartesian(ylim = c(0, 30))
  
  
  #Boxplot for bedtypes vs availability30
  avail_bed_bp<-ggplot(data = listings_cl, aes(x = bedtype, y = availability30, color = bedtype)) +geom_boxplot(outlier.shape = NA) +theme(axis.text.x = element_text(angle = 90, hjust = 1))+coord_cartesian(ylim = c(0, 30))
  
  grid.arrange(avail_bed_bp, avail_room_bp, nrow=3)
  
  
  #Boxplot of how prices vary across accomodates bedrooms and bathrooms 
  box_accommodates <- ggplot(listings_cl, aes(x=as.factor(accommodates), y=price)) + 
    geom_boxplot(outlier.size = 0.6, outlier.shape = 20, lwd = 0.6, fatten = 0.9, fill = "indianred3", alpha = 0.92)+
    scale_y_continuous(name = "Price [in $]", breaks = seq(0,1000,200), limits = c(0,2000))+
    labs(x = "Accommodates")
  
  box_bedrooms <-  ggplot(listings_cl, aes(x=as.factor(bedrooms), y=price)) + 
    geom_boxplot(outlier.size = 0.6, outlier.shape = 20, lwd = 0.6, fatten = 0.9, fill = "darkolivegreen3", alpha = 0.92)+
    scale_y_continuous(name = "Price [in $]", breaks = seq(0,1000,200), limits = c(0,2000))+
    labs(x = "Bedrooms")
  
  box_bathrooms <-  ggplot(listings_cl[is.na(listings_cl$bathrooms)==F,], aes(x=as.factor(bathrooms), y=price)) + 
    geom_boxplot(outlier.size = 0.6, outlier.shape = 20, lwd = 0.6, fatten = 0.9, alpha = 0.92)+
    scale_y_continuous(name = "Pirce [in $]", breaks = seq(0,1000,200), limits = c(0,2000))+
    labs(x = "Bathrooms")+
    theme(plot.caption = element_text(color = "gray45", size = 7))
  
  grid.arrange(box_accommodates, box_bedrooms, box_bathrooms, nrow=3)
  
}

# ************************************************

# Load other R script files
source("clean_preprocess.R")
