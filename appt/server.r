library(shiny)
library(gridExtra)
require(gridExtra)
require(ggplot2)



# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
#district function returns a data frame with data only from the distric input in the selectInput from UI
  data_try <- reactive({
    crime_data <- read.csv(file="crime_data.csv", header=TRUE, na.strings="NA")
    set <- subset(crime_data, crime_data$PdDistrict == as.character(input$input_1))
    date_character <- as.character(set$Date)
    set$month <- substr(date_character,1,2)
    set$month <- as.factor(set$month)
    x <- as.character(set$Time)
    tt <- strptime(paste("2001-01-01", x), format="%Y-%m-%d %H:%M")
    set$hour <- as.factor(format(round(tt,units="hours"), format="%H:%M"))
    
    return(set)
  })

  
  output$plot_month <- renderPlot ({
    b <- data_try()
    ggplot(b) + geom_bar(aes(x= month), fill="#599ad3", position="dodge") + theme(axis.text.x=element_text(angle=45, hjust=1)) + ylab("N. Crimes")
    
  })
  output$plot_day <- renderPlot ({
    c <- data_try()
    ggplot(c) + geom_bar(aes(x =DayOfWeek)) + theme(axis.text.x=element_text(angle=45, hjust=1)) + xlab("Day of the week") + ylab("N.Crimes") 
  })
  output$plot_hour <- renderPlot ({
    d <- data_try()
    hour_breaks = c("00:00", "03:00", "06:00", "09:00", "12:00", "15:00", "18:00","21:00")
    ggplot(d) + geom_bar(aes(x=hour)) +  theme(axis.text.x=element_text(angle=90, hjust=8)) + scale_x_discrete(breaks = hour_breaks) + ylab("N. Crimes")
    
  })
})
  



