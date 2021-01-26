source("call.R")

#Raw Dataset
listings_dataset <- read.csv("listings.csv",na.strings=c("","NA"))
listings_pp_ns <- processListings(rescale=FALSE, zscale=FALSE)
listings_pp <-processListings()


#Examine Data quality
sapply(listings_dataset, function(x) sum(is.na(x)))

#Price column analysis
price_boxplot <- boxplot(listings_cl$price, horizontal = FALSE, main = "Box Plot for Price", ylab = "Price ($)", col = "green")
summary(listings_cl$price)

#Price column analysis after outlier removal
price_boxplot_outlier_removed <- boxplot(listings_pp_ns$price, horizontal = FALSE, main = "Box Plot for Price", ylab = "Price ($)", col = "green")
summary(listings_pp_ns$price)

#Availability
availability_boxplot <- boxplot(listings_cl$availability90, horizontal = FALSE, main = "Box Plot for Availability for the next 30 days", ylab = "Availability (days)", col = "green")
summary(listings_cl$availability90)


#Correlation of all columns

#Top 20 most correlated variables
corr_cross(listings_pp, # name of dataset
           max_pvalue = 0.05, # display only significant correlations (at 5% level)
           top = 20 # display top 10 couples of variables (by correlation coefficient)
)

#Initial hypothesis
#Top 10 most correlated variables to price
corr_var(listings_pp, # name of dataset
         price, # name of variable to focus on
         top = 20 # display top 20 correlations
)

#Top 10 most correlated variables to availability
corr_var(listings_pp, # name of dataset
         availability30, # name of variable to focus on
         top = 20 # display top 20 correlations
)


grep("price", colnames(listings_pp))
grep("availability30", colnames(listings_pp))
grep("amenities", colnames (listings_pp))


#Use for data cleaning 
#Amenities correlation
#Use to justify removal of many amenities columns 
amen.df <- listings_pp[c(27:66,10,19)] 
C_amen<-cor(amen.df)
corrplot(C_amen,method = "circle", order = "hclust",tl.col="black", tl.srt = 45)

#Which amenities are most correlated to price
corr_var(amen.df, # name of dataset
         price, # name of variable to focus on
         top = 20 # display top 10 correlations
)

#Which amenities are most correlated to availability30
corr_var(amen.df, # name of dataset
         availability30, # name of variable to focus on
         top = 20 # display top 10 correlations
)

# CLEAN UP #################################################

# Clear packages
detach("package:datasets", unload = TRUE)  # For base

# Clear plots
dev.off()  # But only if there IS a plot

# Clear all objects in "global environment"
rm(list=ls())

# Clear console
cat("\014")  # ctrl+L

# Clear mind :)

