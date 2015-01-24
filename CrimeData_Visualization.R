#The code below is R. All packages have been detailed. 
#Please set your working directory first and make sure the data file is there.
#The code will create a pdf file called JRojas_Crime_Report in the current directory 
#with all necessary plots to follow the report. 
library("ggplot2")
library("Amelia")
library("stats")
library("RcppArmadillo")
library("ggvis")
library("Rcpp")
library("methods")
library("lattice")
library("datasets")
library("foreign")
library("grid")
library("gridExtra")
library("graphics")
library("grDevices")
library("utils")
setwd("/Users/jrojas/Dropbox/R/TL_Crime_Visualization")
pdf("JRojas_Crime_Report.pdf", width = 8 , height= 8)
crime_data <- read.csv(file="SFPD_Incidents_-_Previous_Three_Months.csv", header=TRUE, na.strings="NA")
names(crime_data)
summary(crime_data)
#Data is essentially categorical, which variables are so?
categ_variables <- sapply(crime_data,class) %in% c('factor','character')
cat_indexes <- which(categ_variables %in% TRUE)
numeric_variables <- sapply(crime_data,class) %in% c('numeric','integer')
num_indexes <- which(numeric_variables %in% TRUE)
c_var <- crime_data[NULL,cat_indexes] #Categorical Variables
n_var <- crime_data[NULL,num_indexes] #Numerical Variables

#Figure 1. missmap function from Amelia package. Unexpectedly, there is no missing data!
#First I create the plot and assign it to Fig_1, then I print it.
missmap(crime_data, legend=TRUE, col=c('black','sienna'), ylabels="", y.at="", main="Fig.1")


#Function to order the data by categorical variable and plot it -------
order_plot <- function(col) {
  d <- table(col)
  d_frame <- as.data.frame(d)
  colnames(d_frame) <- c("var1","var2")
  d_frame <- transform(d_frame, var1= reorder(var1,var2))
  ggplot(d_frame) +geom_bar(aes(x = var1,  y = var2), stat="identity", fill= "sienna") + coord_flip() + xlab("") + ylab("")
  
}
#Figure 2
order_plot(crime_data$PdDistrict) + ggtitle("Fig.2")

#Figure 3
order_plot(crime_data$Category) + ggtitle("Fig.3")




#Figure 4: PdDistric - DayOfWeek
ggplot(crime_data) + geom_bar(aes(x=DayOfWeek), position="dodge", fill="sienna") + ggtitle("Fig.4") +  facet_wrap(~PdDistrict) + theme(axis.text.x=element_text(angle=45, hjust=1)) + xlab("Day of the week") + ylab("N?? Crimes") 

#Figure 5: Category - DayOfWeek
ggplot(crime_data) + geom_bar(aes(x= DayOfWeek), position="dodge", fill="sienna") + ggtitle("Fig.5") + facet_wrap(~ Category) + theme(axis.text.x=element_text(angle=45, hjust=1)) + xlab("Day of the week")+ ylab("N?? Crimes")

#Now I round Time to the nearest hour and turn it a factor.
#This is an unconventional and unproductive way of doing it but I haven't found any other way.
#This is from an example I found in Stackoverflow.
x <- as.character(crime_data$Time)
tt <- strptime(paste("2001-01-01", x), format="%Y-%m-%d %H:%M")
crime_data$hour <- as.factor(format(round(tt,units="hours"), format="%H:%M"))
#hour is a new variable in my data frame crime_data

#Figure 6 Histogram of hour.
hour_breaks = c("00:00", "03:00", "06:00", "09:00", "12:00", "15:00", "18:00","21:00")
ggplot(crime_data) + geom_histogram(aes(x=hour), fill= "sienna") + ggtitle("Fig.6") + theme(axis.text.x=element_text(angle=90, hjust=8)) + scale_x_discrete(breaks= hour_breaks) + ylab("N. Crimes")

#Figure 7 PdDistrict - hour
ggplot(crime_data) + geom_bar(aes(x=hour), fill="sienna", position="dodge") + ggtitle("Fig.7") + facet_wrap(~ PdDistrict)  + theme(axis.text.x=element_text(angle=90, hjust=8)) + scale_x_discrete(breaks = hour_breaks) + ylab("N. Crimes")

#Figure 8 Category - hour
ggplot(crime_data) + geom_bar(aes(x=hour), fill="sienna", position="dodge") + ggtitle("Fig.8") + facet_wrap(~ Category)  + theme(axis.text.x=element_text(angle=90, hjust= 1, vjust=1)) + scale_x_discrete(breaks = hour_breaks) + ylab("N. Crimes")


#Answering Q1: I live in Southern. What should I be concerned about ? 
southern_crime_data <- subset(crime_data, PdDistrict== "SOUTHERN")
#Figure 9
ggplot(southern_crime_data) + geom_bar(aes(x=Category), fill="#599ad3", position="dodge") + theme(axis.text.x=element_text(angle=45, hjust=1)) + ylab("N. Crimes") + ggtitle("Fig.9")

southern_theft_crime_data <- subset(southern_crime_data, Category  == "LARCENY/THEFT")
#Extract the month from Date and assign it to new variable month.
date_character <- as.character(southern_theft_crime_data$Date)
southern_theft_crime_data$month <- substr(date_character,1,2)
southern_theft_crime_data$month <- as.factor(southern_theft_crime_data$month)
#Figure 10
ggplot(southern_theft_crime_data) + geom_bar(aes(x= month, fill= DayOfWeek)) + ylab("N. Crimes") + ggtitle("Fig.10")
date_character <- as.character(southern_crime_data$Date)

#Figure 11
ggplot(southern_theft_crime_data) + geom_bar(aes(x= hour), fill= "#599ad3" ,position="dodge") + ggtitle("Fig.11") + facet_wrap(~ DayOfWeek) + theme(axis.text.x=element_text(angle=90, hjust=1)) + scale_x_discrete(breaks= hour_breaks) + ylab("N. Crimes")

#Answering Question 2: What is the best combination of hour/district to have the car parked?
car_crime_data <- subset(crime_data, Category == "VEHICLE THEFT")
#Figure 12
ggplot(car_crime_data) + geom_bar(aes(x=hour), fill="#f9a65a", position="dodge") + ggtitle("Fig.12") + facet_wrap(~ PdDistrict) + theme(axis.text.x=element_text(angle=90, hjust=1)) + scale_x_discrete(breaks= hour_breaks) + ylab("N. Crimes")


#Answering Question 3: Are there months in which certain crimes occur more frequently ?
#Figure 13

date_character <- as.character(crime_data$Date)
crime_data$month <- substr(date_character,1,2)
crime_data$month <- as.factor(crime_data$month)
ggplot(crime_data) + geom_bar(aes(x= month), fill="#9e66ab") + facet_wrap(~ Category) + ylab("N. Crimes") + ggtitle("Fig.13")


#--------NOT WORKING: function to plot_faceting-------
plot_facet <- function(data_set,var_1,var_2) {
  ggplot(data_set) + geom_bar(aes(x= var_1), position="dodge", fill="blue") + facet_grid(var_2 ~ .)
}
#It would be great if you could write a solution.
#I'm now focusing in writing more efficient code but I think I lack experience. For now,
#this is my submission. 
#---------------------------

#For the plots seen above, another option was to use mosaicplot from vcd package.
layer_points(ggvis(crime_data, x = PdDistrict, y = Category))
dev.off()

