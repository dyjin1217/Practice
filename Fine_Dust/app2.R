library(shiny)

shinyUI(fluidPage(
  
  titlePanel("Air Quality - PM10"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput('aq_metric', 'Air Quality Metric', choices = c("PM10"="pm10",
                                                                 "PM25"="pm25"), selected = "pm10")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      h2("PM10 Metrics by Province"),
      
      conditionalPanel(condition = "input.aq_metric == 'pm10'",
                       h3("PM10 Trend"),
                       plotOutput("pm10_plot"),
                       h3("PM10 Table"),
                       DT::dataTableOutput("pm10_table")
      ),
      conditionalPanel(condition = "input.aq_metric == 'pm25'",
                       h3("PM25 Trend"),
                       plotOutput("pm25_plot"),
                       h3("PM25 Table"),
                       DT::dataTableOutput("pm25_table")
      )
    )
  )))


shinyServer(function(input, output) {
  # PM10 -----   
  output$pm10_plot <- renderPlot({
    aq_pm10_df %>% 
      select(-항목) %>% 
      gather(시도, PM10, -시간, convert = TRUE) %>% 
      mutate(시간 = ymd(시간),
               PM10 = as.integer(PM10)) %>% 
      ggplot(aes(x=시간, y=PM10, group=시도, color=시도)) +
      geom_line() +
      geom_point() +
      scale_x_date(date_labels = "%m-%d") +
      labs(x="") +
      theme_minimal()
  })
  
  output$pm10_table = DT::renderDataTable({
    
    aq_pm10_df %>% 
      datatable()
  })
  # PM25 -----   
  output$pm25_plot <- renderPlot({
    aq_pm25_df %>% 
      select(-항목) %>% 
      gather(시도, PM25, -시간, convert = TRUE) %>% 
      mutate(시간 = ymd(시간),
               PM25 = as.integer(PM25)) %>% 
      ggplot(aes(x=시간, y=PM25, group=시도, color=시도)) +
      geom_line() +
      geom_point() +
      scale_x_date(date_labels = "%m-%d") +
      labs(x="") +
      theme_minimal()
  })
  
  output$pm25_table = DT::renderDataTable({
    
    aq_pm25_df %>% 
      datatable()
  })
  
})

shinyApp(ui = ui, server = server)
