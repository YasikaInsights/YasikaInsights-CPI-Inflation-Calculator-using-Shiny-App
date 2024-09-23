#Problem- CPI Inflation Calculator

library(shiny)
library(readr)
library(ggplot2)

cpi.data <- read.csv("cpi.csv")


ui <- fluidPage(
  titlePanel("CPI Inflation Calculator"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("amount", "Amount:", value = 100),
      selectInput(inputId="base_month", label="Please select the base month", choices=month.name),
      selectInput(inputId= "base_year", label="Please select the base year", choices=unique(cpi.data$year)),
      selectInput(inputId="current_month", label="Please select the current month", choices=month.name),
      selectInput(inputId= "current_year", label="Please select the current year", choices=unique(cpi.data$year)),
      
      actionButton(inputId="update", label="Calculate")
    ),
    
    mainPanel(
      h4("Converted Amount:"),
      textOutput("converted_amount"),
      plotOutput("cpi_trends")
    )
  )
)


server <- function(input, output, session) {
  observeEvent(input$update, {
    base_cpi <- cpi.data$cpi[cpi.data$month == match(input$base_month, month.name) & cpi.data$year == input$base_year]
    current_cpi <- cpi.data$cpi[cpi.data$month == match(input$current_month, month.name) & cpi.data$year == input$current_year]
    
    
    if (length(base_cpi) == 0 || length(current_cpi) == 0 || base_cpi == 0) {
      output$converted_amount <- renderText({
       
      })
    } else {
      conversion_ratio <- current_cpi / base_cpi
      converted_amount <- conversion_ratio * input$amount
      
      output$converted_amount <- renderText({
       
        base_month <- input$base_month
        current_month <- input$current_month
        
    
        paste("$100 in", base_month, input$base_year, "has the same buying power as", 
              "$", round(converted_amount, 2), "in", current_month, input$current_year)
      })
      
   
      output$cpi_trends <- renderPlot({
        ggplot(data = cpi.data, aes(x = as.Date(paste(year, month, "01", sep = "-")), y = cpi)) +
          geom_line(color = "blue", linewidth = 0.8) +
          labs(x = "Date", y = "CPI") +
          theme_classic()
      })
    }
  })
}


shinyApp(ui = ui, server = server)