#runApp("name_app", diplay.mode = "showcase")

library(shiny)
#runApp(dir_name,mode = normal, auto, showcase)
# Define UI for application that draws a histogram

#Layouts are placed in fluidPage function
crime_data <- read.csv(file="crime_data.csv", header=TRUE, na.strings="NA")
shinyUI(fluidPage(
  titlePanel("First Shiny App"),
  
  
  sidebarLayout(
    
    sidebarPanel(h3("Who am I?"),
                 p("My name is Joan Rojas. I'm a student soon to graduate with a BSc 
                  in Mechanical Engineering from",
                   a("ETSEIB-UPC.", href = "http://www.etseib.upc.edu/en")),
                 br(),
                 p("For the past few months I have been learning R by completing some MOOCS 
                   in Coursera."),
                 br(),
                 selectInput("input_1", h5("Which district interests you the most?")
                             ,choices = levels(crime_data$PdDistrict), selected ="SOUTHERN")
                 
    ),
    mainPanel(
             tabsetPanel(
              tabPanel("Introduction",  h2("What is this app?"),
                       p("This is my first Shiny and so it's intended for further learning. 
I will show basic plots of the Crime Data
                         Project from", a("Leada", href="http://www.teamleada.com"),"in 
                         a more interactive way."),
                       p("This is only a first version of the app, I am very aware that there
                         are many features that could improve it. Hopefully I will make it better
                         in the future."),
                       p("My intention is not to become an expert in R programming but to be 
able to put it to use to have basic understanding of 
                         data. "),
                       br(),
                       p("If you are interested in the project, you can find the data set and my code
                         in my", a("Github's profile.", 
                       href="https://github.com/joanrojas"))
                       
                       ),
              tabPanel("Dataset structure", h2("Understanding the data")
              ),
              tabPanel("Month", plotOutput("plot_month", width= 450, height= 450)
              ),
              tabPanel("Day of the week", plotOutput("plot_day",width= 450, height= 450)
              ),
              tabPanel("Hour", plotOutput("plot_hour",width= 450, height= 450)
              )
             )
    )
  ))
)

