# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.

library(shiny)
library(tidyverse)
library(shinythemes)
library(DT)

# Loading dataset
bcl <- read_csv("bcl-data.csv")

# Feature 6: Better UI through setting a visually appealing theme for the app
# Feature 6: Better UI through top page navigation for different app aspects
ui <-navbarPage("BC Liquor Store Data", theme = shinytheme("sandstone"),
  
  tabPanel("Plots",
    
  # Adding Feature 1: Displaying an image of BC Liquor Store to increase app's visual appeal for enhanced user experience
  titlePanel(title = "Basic Data Exploration: Visual"),
  h4("This app serves as a dashboard to conduct basic EDA on the BC liquor store dataset."),
  h4("Data filters:"),
  
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
           titlePanel(title = "Basic Data Exploration: Tabular"),
           h4("Table delineates filtered information per user selections"),
           h4("Table is interactive for further sorting and filtering"),
           
           sidebarLayout(
             sidebarPanel(
               img(src="pic.png", height="203px", width="357px", alt="error with image", deleteFile=FALSE)
             ),
           
             mainPanel(
               # Using package DT, rendering table output
               DT::dataTableOutput("data_table")  
             )
             
           )),
  
  tabPanel("Data",
           # Including the data download button
           h3("Data with applicable filters can be downloaded as .csv:"),
           downloadButton("downloadData", "Data Download"),
           
           # Displaying acknowledgements
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
  
  # Adding Feature 5: changing data rendering for table to enable interactivity
  output$data_table <- 
    DT::renderDataTable({
      filtered_data()
    })
  
  # Adding Feature 4: Downloading filtered data
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
