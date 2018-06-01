library(shiny)
library(plotly)
library(rvest)
library(DT)

ui <- fluidPage(
  titlePanel("Historical price data of top 10 cryptocurrencies"),
  sidebarLayout(
    sidebarPanel(
      selectInput("coin", "Select a coin:", c("Bitcoin", "Ethereum", "Ripple",
                                              "Bitcoin Cash"="Bitcoin-cash", "EOS", "Litecoin",
                                              "Cardano", "Stellar", "IOTA", "Tron")),
      selectInput("range", "Choose range of data:" , c("Past 7 days"=7, "Past 14 days"=14,
                                                       "Past 30 days"=30))
    ),
    mainPanel(
      tabsetPanel(type = "tabs", 
                  tabPanel("Description", br(), "This Shiny App is a very simple tool
                           to collect and visualise historical price data of some 
                           crptocurrencies. \n Select the desired cryptocurrency 
                           and day range with the dropdown menu at the left sidepanel.
                           The data will be fetched and displayed in the tab Data, and 
                           the appropriate graph."),
                  tabPanel("Data", br(), DT::dataTableOutput("BTCtable")),
                  tabPanel("Graph", br(), plotlyOutput("plot1"))
                  
                  )
      )
    )
  )

shinyServer <- function(input, output, session){
  output$selected_coin <- renderText({input$coin})
  BTCtable <- reactive({
    cCoin <- input$coin
    cRange <- as.numeric(input$range)
    BTCtable <- read_html(paste0("https://coinmarketcap.com/currencies/",
                                 cCoin,"/historical-data/?start=2018",
                                 format.Date(Sys.Date()-cRange, "%m%d"),"&end=2018",
                                 format.Date(Sys.Date()-1, "%m%d")))%>%
      html_table()
  })
  
  output$BTCtable = DT::renderDataTable({
    as.data.frame(BTCtable())
  })
  
  output$plot1 <- renderPlotly({
    p <- plot_ly()%>%
      add_trace(as.data.frame(BTCtable())$Date, y=~as.data.frame(BTCtable())$High, 
                type = "scatter", mode="lines") %>%
      layout(xaxis = list(title="Time in days"), yaxis=list(title="Price in $"))
  })
}

shinyApp(ui, shinyServer)