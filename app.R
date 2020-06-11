#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#

library(shiny)
library(rvest)
library(tidyverse)
library(jsonlite)
library(rsconnect)
library(httr)

#######
# In case of emergency
# key=90165a9f1025498db18f1a86b5bf9312
#######

#usethis::edit_r_environ("project")
#readRenviron(".Renviron")

# Define UI for application
ui <- fluidPage(
    
    # Application title
    titlePanel("Weather on New Year's Day"),
    
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            textInput("text",label = "Text",value = "Enter City"),
            actionButton("action",label = "Go")
        ),
        
        
        
        # Show maximum temperature in Fareinheit
        mainPanel(
            textOutput("max"),
            textOutput("temperature"),
            textOutput("min"),
            textOutput("temperature2"),
            plotOutput("plot")
            
        )
    )
    
    
)

# Define server logic required to output max temperature
server <- function(input, output) {
    
    max <- eventReactive(input$action, {
        r <- GET("https://api.weatherbit.io/v2.0/history/daily?units=I&start_date=2020-01-01&end_date=2020-01-02&key=90165a9f1025498db18f1a86b5bf9312",
                 query = list(city = input$text))
        
        json <- content(r, as = "text")
        data <- fromJSON(json)
        data$data$max_temp
    })
    
    min <- eventReactive(input$action, {
        r <- GET("https://api.weatherbit.io/v2.0/history/daily?units=I&start_date=2020-01-01&end_date=2020-01-02&key=90165a9f1025498db18f1a86b5bf9312",
                 query = list(city = input$text))
        
        json <- content(r, as = "text")
        data <- fromJSON(json)
        data$data$min_temp
    })
    
    output$max <- renderText({
        print("Max")
    })
    
    output$temperature <- renderText({ 
        paste(max(),"F")
    })
    
    output$min <- renderText({
        print("Min")
    })
    
    output$temperature2 <- renderText({
        paste(min(),"F")
    })
    
    output$plot <- renderPlot({
        boxplot(min(),max(),names = c("Min","Max"),ylab="Fahrenheit",
                main=paste("Temperature in",input$text))
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
