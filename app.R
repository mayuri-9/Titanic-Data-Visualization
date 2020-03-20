#Load packages
library(shiny)
library(shinydashboard)
library(shinyWidgets)
#Call source file helper functions
source('helper.R')

#Create the title
titledata <- tags$label(tags$img(src = "titanic.jpg", height = '60', width = '65'),
                        tags$b(" Titanic Data Visualization"))

#user interface
ui <- fluidPage(  
  tags$head(
    tags$style(HTML("
                    @import url('my-font.url');
                    .col-sm-6 {
                    width:100%;
                    }
                    #run{
                    background-color:green;
                    color:white;
                    }

                    .h3, h3{
                    font-weight: 500;
                    line-height: 1.5;
                    color: darkslateblue;
                    font-family: none;
                    font-size: 19px;
                    }

                    .nav-tabs>li.active>a, 
                      .nav-tabs>li.active>a:focus
                    , .nav-tabs>li.active>a:hover {
                      background-color: #dbf9ff;
                    }

                    .tab-content>.active
                    {font-family: none;}
                    "
    ))),
  
  fluidRow(style="color: aliceblue;
    background-color: midnightblue;",
           
           column(6,offset = 3,
                  titlePanel(titledata, windowTitle = "Titanic Data Visualization")),
           column(6)
  ),
  
  fluidRow(
    br()
  ),
  
  fluidRow(
    column(12,
           tabBox(                        
             id = "tabset1",
             tabPanel("Home", 
                     sidebarLayout(
                       sidebarPanel(
                          helpText("Create graphs with 
                                   information from the Titanic dataset."),
                          
                          selectInput("var", 
                                      label = "Choose a variable to display",
                                      choices = c("Port of Embarkation", "Family Size",
                                                  "Gender","Ticket Class"),
                                      selected = "Gender"),
                          
                          sliderInput("range", 
                                      label = "Age in years:",
                                      min = 0, max = 80, value = c(0, 80)),
                        
                          submitButton("Run Analysis", icon("play"))),
                        
                        mainPanel(plotOutput("graph"))                      
             )),
          
             tabPanel("Information", h3("The sinking of the Titanic is one of the most infamous shipwrecks in history.

                      On April 15, 1912, during her maiden voyage, the widely considered “unsinkable” RMS Titanic sank after colliding with an iceberg. Unfortunately, there weren’t enough lifeboats for everyone onboard, resulting in the death of 1502 out of 2224 passengers and crew.
                      
                      While there was some element of luck involved in surviving, it seems some groups of people were more likely to survive than others."))    
  
))))

server <- function(input, output) {
  output$graph <- renderPlot({

  data <- switch(input$var,
                   "Port of Embarkation" = 'embark',
                    "Gender" = 'sex',
                    "Ticket Class" = 'pc',
                   "Family Size" = 'fs')
  if (data == 'embark')
  {embark_plot(input$range[1], input$range[2])}
  else if (data == 'sex')
  {gender_plot(input$range[1], input$range[2])}
  else if(data == 'pc')
  {pclass_plot(input$range[1], input$range[2])}
  else
  {fsize_plot(input$range[1], input$range[2])}
  
  })

}

# Run the application 
shinyApp(ui = ui, server = server)

