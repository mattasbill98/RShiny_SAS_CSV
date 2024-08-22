# R Shiny App that converts SAS datasets to csv 
# and enables some simple graphing exploration

library(shiny)
library(dplyr)
library(ggplot2)
library(haven)
library(DT)
library(plotly)


ui <- fluidPage(
  titlePanel("SAS to CSV File Converter and Scatter Plot"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("upload", "Choose a SAS file", accept = ".sas7bdat"),
      uiOutput("varselect1"),
      uiOutput("varselect2"),
      downloadButton('downloadData', 'Download Data')
    ),
  
    mainPanel(
      tabsetPanel(
        id = "tabs",
        tabPanel("Data", DTOutput("dataTable")),
        tabPanel("Scatter Plot", plotOutput("scatterPlot"))
        #tableOutput('contents')
      )
    )
  )
)

server <- function(input, output){
  sas_data <- reactive({
    file <- input$upload
    ext <- tools::file_ext(file$datapath)
    
    req(file)
    validate(need(ext == "sas7bdat", "Please upload a sas file"))
    
    #Read the sas dataset
    sas_data <- read_sas(file$datapath)
  })
  
  # Render the data table
  output$dataTable <- renderDT({
    req(sas_data())
    datatable(sas_data(), options = list(pageLength = 15))
  })
  
  #Select the first variable
  output$varselect1 <- renderUI({
    req(sas_data())
    selectInput("var1", "X-axis variable", names(sas_data()))
  })
  
  #select second variable
  output$varselect2 <- renderUI({
    req(sas_data())
    selectInput("var2", "Y-axis variable", names(sas_data()))
  })
  
  #Print Scatter Plot
  output$scatterPlot <- renderPlot({
    req(input$var1, input$var2)
    p = ggplot(data = sas_data()) +
      aes_string(x = input$var1, y = input$var2) +
      geom_point()
    plot(p)
    observeEvent(input$update, print(as.numeric(input$update)))
  })
  
  #Download data to CSV
  output$downloadData <- downloadHandler(
    filename = function() { 
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(sas_data(), file)
    })
}

#Run the app
shinyApp(ui = ui, server = server)


