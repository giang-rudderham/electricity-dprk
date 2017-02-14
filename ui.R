library(shiny)

shinyUI(fluidPage(
  
  titlePanel("Energy Usage in DPRK, 2008"),
  
  sidebarLayout(
    mainPanel("dprkmap", 
              plotOutput("dprkmap")),
    sidebarPanel( "sidebar panel")
  )
  ),
  
  hr(),
  
  
  br(),
  br(),
  br(),
  br(),
  br(),
  p(strong("Created by:"), "Giang Rudderham 
    (giang.rudderham@gmail.com)"),
  p(strong("Data source:"), "DPRK 2008 Population Census")
  )