library(shiny)
library(plotly)
library(rvest)
library(DT)
library(lubridate)
library(dplyr)

# ui is the main design of the app
ui <- fluidPage(
  titlePanel("Historical price data of top 10 cryptocurrencies"),
  sidebarLayout(
    sidebarPanel(
      # Create a sidebar panel to allow user to select the type of coin and date range
      selectInput("coin", "Select a coin:", c("Bitcoin", "Ethereum", "Ripple",
                                              "Bitcoin Cash"="Bitcoin-cash", "EOS", "Litecoin",
                                              "Cardano", "Stellar", "IOTA", "Tron")),
      selectInput("range", "Choose range of data:" , c("Past 7 days"=7, "Past 14 days"=14,
                                                       "Past 30 days"=30, "Past year"=365,
                                                       "All time"=Sys.Date() - as.Date("2013-04-28")))
    ),
    mainPanel(
      tabsetPanel(type = "tabs", 
                  tabPanel("Description", br(), "This Shiny App is a very simple tool
                           to collect and visualise historical price data of some 
                           crptocurrencies. \n Select the desired cryptocurrency 
                           and day range with the dropdown menu at the left sidepanel.
                           The data will be fetched and displayed in the tab 'Data', and 
                           the appropriate graph."),
                  tabPanel("Data", br(), DT::dataTableOutput("BTCtable")),
                  tabPanel("Candlesticks", br(), plotlyOutput("plot1"))
                  )
      )
    )
  )

# This is the function that processes the user input and fectch the data from server
shinyServer <- function(input, output, session){
  output$selected_coin <- renderText({input$coin})
  BTCtable <- reactive({
    cCoin <- input$coin
    cRange <- as.numeric(input$range)
    BTCtable <- read_html(paste0("https://coinmarketcap.com/currencies/",
                                 cCoin,"/historical-data/?start=20",
                                 format.Date(Sys.Date()-cRange, "%y%m%d"),"&end=20",
                                 format.Date(Sys.Date()-1, "%y%m%d")))%>%
      html_table()
    BTCtable <- as.data.frame(BTCtable[[1]])
    BTCtable$Date <- mdy(BTCtable$Date)
    arrange(BTCtable, Date)
    
  })
  
  # Create data table of the fetched html table
  output$BTCtable = DT::renderDataTable({
    BTCtable()
  })
 

  # Create plot using plotly
  output$plot1 <- renderPlotly({
    p <- BTCtable()%>%
      plot_ly(x=~Date, 
                high=~High,
                low=~Low,
                open=~`Open*`,
                close=~`Close**`,
                type = "candlestick") %>%
      layout(xaxis = list(rangeslider = list(visible = F)))
  })
}
# Run the app through shiny server
shinyApp(ui, shinyServer)