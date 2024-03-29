# Yolo example
library(shiny)
library(contentid)
library(dplyr)
library(ggplot2)
library(lubridate)

sha1 <- 'hash://sha1/317d7f840e598f5f3be732ab0e04f00a8051c6d0'
delta.file <- contentid::resolve(sha1)

# fix the sample date format, and filter for species of interest
delta_data <- read.csv(delta.file) %>% 
  mutate(SampleDate = mdy(SampleDate))  %>% 
  filter(grepl("Salmon|Striped Bass|Smelt|Sturgeon", CommonName))

cols <- names(delta_data)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
  # Application title
  titlePanel("Yolo Bypass Fish and Water Quality Data"),
  p("Data for this application are from: "),
  tags$ul(
    tags$li("Interagency Ecological Program: Fish catch and water quality data from the Sacramento River floodplain and tidal slough, collected by the Yolo Bypass Fish Monitoring Program, 1998-2018.",
            tags$a("doi:10.6073/pasta/b0b15aef7f3b52d2c5adc10004c05a6f", href="http://doi.org/10.6073/pasta/b0b15aef7f3b52d2c5adc10004c05a6f")
    )
  ),
  tags$br(),
  tags$hr(),

    verticalLayout(
      # Sidebar with a slider input for depth axis
      sidebarLayout(
        sidebarPanel(
          sliderInput("date",
                      "Date:",
                      min = as.Date("1998-01-01"),
                      max = as.Date("2020-01-01"),
                      value = c(as.Date("1998-01-01"), as.Date("2020-01-01")))
        ),
        # Show a plot of the generated distribution
        mainPanel(
          plotOutput("distPlot")
        )
      ),
      
      tags$hr(),
      
      sidebarLayout(
        sidebarPanel(
          selectInput("x_variable", "X Variable", cols, selected = "SampleDate"),
          selectInput("y_variable", "Y Variable", cols, selected = "Count"),
          selectInput("color_variable", "Color", cols, selected = "CommonName")
        ),
        
        # Show a plot with configurable axes
        mainPanel(
          plotOutput("varPlot")
        )
      ),
      tags$hr()
))

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
       ggplot(delta_data, mapping = aes(x = SampleDate, y = Secchi))+
        geom_point(color = "red", size = 4)+
        xlim(input$date)+
        theme_light()
    })
    output$varPlot <- renderPlot({
      ggplot(delta_data, mapping = aes(x = .data[[input$x_variable]],
                                       y = .data[[input$y_variable]], 
                                       color = .data[[input$color_variable]]))+
        geom_point(size = 4)+
        theme_light()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)


