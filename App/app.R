# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.

library(shiny)
library(tidyverse)
library(shinythemes)
library(DT)
library(readr)

# Loading dataset
bcl <- read_csv("bcl-data.csv")

# Setting a visually appealing theme for the app
# Creating a top page navigation layout for the app
ui <-navbarPage("BC Liquor Store Data", theme = shinytheme("sandstone"),
  
  tabPanel("Plots",
    
  # Adding Feature 1: Displaying an image of BC Liquor Store to increase app's visual appeal for enhanced user experience
  titlePanel(title = "Basic Exploratory data analsyis: Visual Information"),
  h3("Uawr can make selections"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("priceInput", "Price", 0, 100, 
                  value = c(25, 40), pre = "$"), 
      
      # Amended radio buttons to multi-input check box 
      checkboxGroupInput(inputId = "typeInput",
                         label = "Choose Drink Type(s):",
                         choices = c("BEER", "REFRESHMENT", "SPIRITS", "WINE"),
                         selected = c("BEER", "WINE")),
      
      # Displaying results for Feature 2 just below the filters for appropriate layout
      textOutput("filteredResult")
    ),
    
    mainPanel(
      # Adding Feature 3: Including a 'tab' layout to plot another histogram allowing for comparison between different data attributes
      tabsetPanel(
      tabPanel("Frequency Distribution of Alcohol Content", plotOutput("alcohol_hist")),
      tabPanel("Frequency Distribution of Sweetness", plotOutput("sweetness_hist")))
    )
  )),
  
  tabPanel("Table",
           titlePanel(title = "Basic Exploratory data analsyis: Tabular Information"),
           h4("This table delineates filtered information as per user criteria set in the tab 'Plots'"),
           h4("Table is interactive for further sorting and filtering"),
           
           sidebarLayout(
             sidebarPanel(
               img(src="pic.png", height="203px", width="357px", alt="error with image", deleteFile=FALSE)
             ),
           
             mainPanel(
               # changing table rendering...
               DT::dataTableOutput("data_table")  
             )
             
           )),
  
  tabPanel("Data",
           h3("Data with applicable filters can be downloaded as .csv:"),
           downloadButton("downloadData", "Data Download"),
           
           h3("Acknowledgements:"),
           a(href="https://github.com/daattali/shiny-server/blob/master/bcl/data/bcl-data.csv", 
             "Link to the original data set")
           )
)

server <- function(input, output) {
  
  # Making the dataset reactive to user inputs i.e. filters
  filtered_data <- 
    reactive({
      bcl %>% filter(Price > input$priceInput[1] & 
                       Price < input$priceInput[2] & 
                       Type == input$typeInput)
    })
  

  # Displaying histogram for 'alcohol_content' attribute
  output$alcohol_hist <- 
    renderPlot({
      filtered_data() %>% 
        ggplot(aes(Alcohol_Content)) + geom_histogram()
    })
  
  # Displaying histogram for 'sweetness' attribute
  output$sweetness_hist <- 
    renderPlot({
      filtered_data() %>% 
        ggplot(aes(Sweetness)) + geom_histogram()
    })
  
  # Displaying histogram for 'country' attribute
  output$country_hist <- 
    renderPlot({
      filtered_data() %>% 
        ggplot(aes(Country)) + geom_histogram()
    })
  
  
  # Feature 2: Computing the result for total item selections present in dataset for the combined user filters
  output$filteredResult<-
    renderText({
      tempCount <- nrow(filtered_data())
      if (is.null(tempCount)) {
        tempCount<-0
      }
      paste("No of products found per above selection ", tempCount)
    })
  
  output$data_table <- 
    # changing data rendering
    DT::renderDataTable({
      filtered_data()
    })
  
  # Adding downloading data..
  output$downloadData <- downloadHandler(
    filename = function(){
      paste("BC-Liquor-Dataset", ".csv", sep="")
    },
    content = function(file){
      write.csv(filtered_data(), file)
    }  
      )  
}


# Run the application 
shinyApp(ui = ui, server = server)
