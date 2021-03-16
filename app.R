#Install all packages needed
library(fredr)
library(tidyverse)
library(jsonlite)
library(lubridate)
library(shiny)
library(ggplot2)
library(plotly)

#Retreived the api key from the federal reserve online, and set it
apikey <- "307b97660ca03452f413e3de10806d02"
fredr_set_key(apikey)

# #Loaded each treasury bond in from the FRED api, which will update when new data is added
# if the dataset is reloaded. Retrieved the 1,3, and 6 month T-bills along with the
# 1,2,3,5,7,10,20, and 30 year T-bonds and T-bills. 
Y1MTbill <- fredr(
    series_id = "DGS1MO"
)
Y3MTbill <- fredr(
    series_id = "DGS3MO"
)
Y6MTbill <- fredr(
    series_id = "DGS6MO"
)
Y1Tbill <- fredr(
    series_id = "DGS1"
)
Y2Tnote <- fredr(
    series_id = "DGS2"
)
Y3Tnote <- fredr(
    series_id = "DGS3"
)
Y5Tnote <- fredr(
    series_id = "DGS5"
)
Y7Tnote <- fredr(
    series_id = "DGS7"
)
Y10Tnote <- fredr(
    series_id = "DGS10"
)
Y20Tbond <- fredr(
    series_id = "DGS20"
)
Y30Tbond <- fredr(
    series_id = "DGS30"
)


# Define UI for dataset viewer app ----
ui <- fluidPage(
    
    # App title ----
    titlePanel("Treasury Bond Yields"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            
            
            # Input: Choose which bond you want to see the plot and rate for
            selectInput(inputId = "bonds",
                        label = "Choose a Bond Type:",
                        choices = c("1MO", "3MO", "6MO", "1Y", "2Y", 
                                    "3Y", "5Y", "7Y","10Y", "20Y", "30Y")),
            
            
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            plotlyOutput("InterestRatePlot"),
            uiOutput("dynamic"),
            verbatimTextOutput("hover_info"),
            
            # Output: Formatted text for caption ----
            textOutput("summary_title"),
            
            # Output: Verbatim text for data summary ----
            verbatimTextOutput("summary")
            
        )
    )
)

# Define server logic to summarize and view selected dataset
server <- function(input, output) {
    
    #Reactive function which switches the input when a new bond is selected, and retrieves
    #the corresponding new dataset. 
    datasetInput <- reactive({
        switch(input$bonds,
               "1MO" = Y1MTbill,
               "3MO" = Y3MTbill,
               "6MO" = Y6MTbill,
               "1Y" = Y1Tbill,
               "2Y" = Y2Tnote,
               "3Y" = Y3Tnote,
               "5Y" = Y5Tnote, 
               "7Y" = Y7Tnote, 
               "10Y" = Y10Tnote, 
               "20Y" = Y20Tbond, 
               "30Y" = Y30Tbond)
    })
    
    #Plot render, takes the currently chosen dataset and uses the plotly package
    #to make an interactive graph of the interest rate time series
    output$InterestRatePlot<-renderPlotly({
        bonds <- datasetInput() 
        ggplot(bonds, aes(x = date, y = value, color = series_id)) + geom_line()
    })
    
    #A description which reflects which dataset is chosen when displaying the sentence. 
    #Less than or equal to a year: Treasury Bill. 2-10 Years: Treasury Note. 10+ Years: Treasury Bond
    output$summary_title <- renderText({
        bonds <- datasetInput()
        if (input$bonds %in% c("1MO", "3MO", "6MO", "1Y")) {
            paste("Today's Interest Rate for a", input$bonds, "Treasury Bills", sep = " ")
        } 
        else if (input$bonds %in% c("2Y", "3Y", "5Y", "7Y", "10Y")) {
            paste("Today's Interest Rate for a", input$bonds, "Treasury Notes", sep = " ")
        } 
        else if (input$bonds %in% c("20Y", "30Y")) {
            paste("Today's Interest Rate for a", input$bonds, "Treasury Bonds", sep = " ")
        } 
    })
    
    #Shows the last value of the dataset, which corresponds to today's yield on that bond. 
    output$summary <- renderPrint({
        bonds <- datasetInput()
        tail(bonds$value, 1)
    })
    
}

# Create Shiny app ----
shinyApp(ui, server)