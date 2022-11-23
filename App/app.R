#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.


library(shiny)
library(tidyverse)

# bcl <- read_csv("~/Personal/UBC M.Eng/STAT 545B/Assign3/assignment-b3-hs235/bcl-data.csv")
bcl <- read_csv("bcl-data.csv")

ui <- fluidPage(
  titlePanel("BC Liquor Store Data"), 
  h5("Welcome to my shiny app!"), 
  br(), 
  sidebarLayout(
    sidebarPanel(
      sliderInput("priceInput", "Price", 0, 100, 
                  value = c(25, 40), pre = "$"), 
      radioButtons("typeInput", "Type", 
                   choices = c("BEER", "REFRESHMENT", 
                               "SPIRITS", "WINE"))
    ),
    mainPanel(
      plotOutput("alcohol_hist"), 
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
  
  output$data_table <- 
    renderTable({
      filtered_data()
    }) 
}


# Run the application 
shinyApp(ui = ui, server = server)
