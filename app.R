#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(plotly)
library(DT)
library(car)
library(carData)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Map plot
 plotlyOutput("plot"),
  verbatimTextOutput("click"),
  
   # Application title
   titlePanel("States and Data"),
   
   # Sidebars with a slider input , radio button , submit button 
   sidebarLayout(
     sidebarPanel(("Enter the information"),
                  selectInput("Name", " State names",choices = st$state.name, selected = 1),
                  
                  selectInput( "var", "1.Select Histogram",choices = c("Population"=1, "Income"=2
                                                              ),selected = 1,selectize = FALSE),
                  
                  sliderInput("bin",
                              "Number of bins:",
                              min = 1,
                              max = 50,
                              value = 50),
                  submitButton("Submit"),
                  
                  checkboxInput("Plot1","show/hide Histogram", value = TRUE),
                  checkboxInput("Plot2", "show/hide Boxplot", value =TRUE),
                  selectInput( "var1", "2.Select Boxplot",choices = c("Population"=1, "Income"=2
                  ),selected = 1,selectize = FALSE),
                   submitButton("Submit"),
                  radioButtons("Radio","3.Select Rate Chart(Panel 3) ",list("Illiteracy-Rate",
                                                               "Life-Expectancy-Rate",
                                                               "Murder-Rate",
                                                               "High-school-Rate"))  ,                                         
                                                               submitButton("Submit") ),
      
      # Show  plots 
     mainPanel(
       
       tabsetPanel(type = "tab",
       tabPanel("Plots",
                tableOutput("dname"),
                plotOutput("distPlot"),
                
               h3("Summary of the variables"),
               verbatimTextOutput("sum"),
                plotlyOutput("box")),
       
       tabPanel("Data",
                h3("Data of the variables"),
                plotlyOutput("scatter1"),DT::dataTableOutput("table") ),
      # tabPanel("ScatterPlot",plotlyOutput("scatter1")),
       
       tabPanel("Rates",
                h3("Rates in US States"),
                plotlyOutput("dradio"),
                h3("Data of the variables"),
                DT::dataTableOutput("pietable")))
       
     )
   )
)



# Plotly map
server <- function(input, output) {
  st <- data.frame(state.x77, state.abb, state.area, state.region, state.division,
                   state.center, state.name)
  
  output$plot <- renderPlotly({
    # specify some map projection/options
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      lakecolor = toRGB('white')
    )
    plot_ly(z = st$Population, text = state.name, locations = state.abb,
            type = 'choropleth', locationmode = 'USA-states') %>%
      layout(title ='<b>US Population in 1975<b>',geo = g)
  })
  output$click <- renderPrint({
    d <- event_data("plotly_click")
    if (is.null(d)) "Click on a state to view Population" else d
  })
  
 
  st1 <- data.frame(state.x77[,1:2],state.region,state.division)
  output$dname <- renderTable({
    n1= input$Name
    n <- st1[n1,]
  })
  
  
  
  # Text output for select
  output$text1 <- renderText({
   colm=as.numeric(input$var)
   paste("US States ",names(st[colm]))
 })
 
 # Histogram
 output$distPlot <- renderPlot({
   colm= as.numeric(input$var)
   x    <- st[,colm] 
   bins <- seq(min(x), max(x), length.out = input$bin )
   
   if(input$Plot1){
     
    
     
     hist(x, breaks = bins, col = 'red', border = 'white', xlab = paste("US States "),
          xlim = c(100,22000), main = "Histogram ")
  }
 })
 
 # Boxplot
 
 output$box <-renderPlotly({
   colm1=as.numeric(input$var1)
   x1    <- st[,colm1] 
  
   
  if(input$Plot2){
    
    
    output$sum <- renderPrint({
      summary(st[,as.numeric(input$var1)])
    })
    
   
    plot_ly(y=x1, type= "box", boxpoints ="all",jitter =0.3, pointpos =-1.8)}
 })
 
 

 
#Panel2   
 #scatter 
 stt <- data.frame(state.x77[,c(1,2,7,8)])
 
 output$scatter1 <- renderPlotly({ 
   
 plot_ly(st, x = st$Income, y = st$Population , type = 'scatter',color = st$Life.Exp,size = st$Income,
            sizes = c(10, 50),
            mode = "markers", marker = list( opacity = 0.5,sizemode = 'diameter'),
            hoverinfo = 'text',
            text = paste('State Name =', state.name)) %>%
      colorbar(title = "Life expectancy in years")%>%
      layout(title = "x = Income, y = Population ",
             xaxis = list(title = "Per capita income",
                          yaxis = list(title = "Population")))
    })
 

 
 #Table for scatter plot
 output$table <- DT::renderDataTable (data.frame(state.name,st$Population,st$Income,st$Frost,
                                                 st$state.area))
 
 #Panel3 for pie chart and table 
 
 # Pie chart with radio button
    
   datast <- data.frame(state.x77[,3:6])
   
   r <- reactive({
     input$Radio
   })
   
   output$dradio <- renderPlotly({ 
       
    if(r() == "Illiteracy-Rate"){ 
        
    if(r() == "Illiteracy-Rate"){ 
      plot_ly(datast, labels = st$state.name, values = datast$Illiteracy, type = 'pie') %>%
        layout( 
          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))}
    else
      
      if(r() == "Life-Expectancy-Rate"){ 
        plot_ly(datast, labels = st$state.name, values = datast$Life.Exp, type = 'pie') %>%
          layout( 
            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))}
    else
      
      if(r() == "Murder-Rate"){ 
        plot_ly(datast, labels = st$state.name, values = datast$Murder, type = 'pie') %>%
          layout( 
            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))}
    else
      
      
      if(r() == "High-school-Rate"){ 
        plot_ly(datast, labels = st$state.name, values = datast$HS.Grad, type = 'pie') %>%
          layout( 
            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))}
     
})
     
  
   
  #  table for pie chart
   output$pietable <- DT::renderDataTable(data.frame(datast)) 
   
  
    
  }
  
# Run the application 
shinyApp(ui = ui, server = server)

