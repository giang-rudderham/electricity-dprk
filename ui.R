library(shiny)
library(leaflet)

shinyUI(navbarPage("Energy usage in DPRK (North Korea), 2008", id = "nav",
                   
                   tabPanel("Map of DPRK",
                            div(class = "outer",
                                
                                tags$style(type = "text/css",
                                           ".outer {position: fixed; top: 41 px; left: 0;
                                           right: 0; bottom: 0; overflow: hidden; padding: 0}"),
                                
                                leafletOutput("dprkmap", width = "100%", height = "1075px"),
                                
                                absolutePanel(id = "controls", class = "panel panel-default",
                                              fixed = TRUE, draggable = TRUE, top = "auto",
                                              left = 20, right = "auto", bottom = 10, 
                                              width = 580, height = "auto", 
                                              
                                              strong("Select to display visualizations of electricity usage for cooking and/or heating."),
                                              checkboxInput("cooking", 
                                                            "Percentage of households that use electricity as cooking fuel",
                                                            value = FALSE),
                                              checkboxInput("heating",
                                                            "Percentage of households that use electronic heating",
                                                            value = TRUE),
                                              strong("Click on a province to learn more about energy sources for cooking and heating in that province."),
                                              
                                              plotOutput("barCooking", height = 275),
                                              
                                              plotOutput("barHeating", height = 275)
                                ),
                                
                                tags$div(id = "cite",
                                         strong("Data source:"), tags$em("DPRK 2008 Population Census.", 
                                                                        "Please send questions and comments
                                                                         to author Giang Rudderham (giang.rudderham@gmail.com)")
                                         )
                                )
                            )
                   # ,
                   # 
                   # tabPanel("Histogram",
                   #          plotOutput("hist", width = "80%", height = "375px"),
                   #          br(),
                   #          hr(),
                   #          br(),
                   #          fluidRow(
                   #            shiny::column(10, offset = 4,
                   #                          sliderInput("yearhist", "Choose a year:",
                   #                                      min = 1991, max = 2015, value = 2015,
                   #                                      animate = TRUE, sep = "")
                  #            )
                   #         )
                   #)
                             ))
