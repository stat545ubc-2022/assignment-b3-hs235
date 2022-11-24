#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.

library(shiny)
library(tidyverse)
library(readr)

#setwd("C:/Users/h1792/Documents/Personal/UBC M.Eng/STAT 545B/Assign3/assignment-b3-hs235")

#bcl <- read_csv("~/Personal/UBC M.Eng/STAT 545B/Assign3/assignment-b3-hs235/bcl-data.csv")
bcl <- read_csv("bcl-data.csv")

ui <- fluidPage(
  
  # Adding Feature 1: Displaying an image of BC Liquor Store to increase app's visual appeal for enhanced user experience
  titlePanel(title = div(img(src="pic.png", height="145px", width="255px", alt="error with image", deleteFile=FALSE),"BC Liquor Store Data")),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("priceInput", "Price", 0, 100, 
                  value = c(25, 40), pre = "$"), 
      
      # Amended radio buttons to multi-input check box 
      checkboxGroupInput(inputId = "typeInput",
                         label = "Choose Drink Type(s):",
                         choices = c("BEER", "REFRESHMENT", "SPIRITS", "WINE"),
                         selected = c("BEER", "WINE")),
      
      textOutput("filteredResult")
    ),
    mainPanel(
      # Adding Feature 2: Inclucing another tab to display 
      tabsetPanel(
      tabPanel("Frequency Distribution of Alcohol Content", plotOutput("alcohol_hist")),
      tabPanel("Frequency Distribution of Sweetness", plotOutput("sweetness_hist"))),
      tableOutput("data_table")
    )
  ), 
  a(href="https://github.com/daattali/shiny-server/blob/master/bcl/data/bcl-data.csv", 
    "Link to the original data set")
)

server <- function(input, output) {
  
  filtered_data <- 
    reactive({
      bcl %>% filter(Price > input$priceInput[1] & 
                       Price < input$priceInput[2] & 
                       Type == input$typeInput)
    })
  
  output$alcohol_hist <- 
    renderPlot({
      filtered_data() %>% 
        ggplot(aes(Alcohol_Content)) + geom_histogram()
    })
  
  output$sweetness_hist <- 
    renderPlot({
      filtered_data() %>% 
        ggplot(aes(Sweetness)) + geom_histogram()
    })
  
  # Feature 2: 
  output$filteredResult<-
    renderText({
      tempCount <- nrow(filtered_data())
      if (is.null(tempCount)) {
        tempCount<-0
      }
      paste("No of products found per above selection ", tempCount)
    })
  
  output$data_table <- 
    renderTable({
      filtered_data()
    })
}


# Run the application 
shinyApp(ui = ui, server = server)
