

shinyUI(

  navbarPage(id="main", windowTitle = "Bachelors", position = "fixed-top", title = NULL,
   tabPanel("Dashboard", icon = icon("dashboard"),
            
            
            
            br(),
            br(),
            br(),
            fluidRow(column(5, offset = 3, align = "center", h3c("Dashboard"))),
            fluidRow(column(5, offset = 1, uiOutput("tableSelector3"))),
            fluidRow(column(5, offset = 1, uiOutput("tableSelector4"))),
            fluidRow(
              
              column(3,
                     div( 
                          C3GaugeOutput("Gauge1", height = 150), style = "width:75%; margin: auto")),
              
              column(3,
                     div( 
                          C3GaugeOutput("Gauge2", height = 150), style = "width:75%; margin: auto")),
              
              column(3,
                     div( 
                          C3GaugeOutput("Gauge3", height = 150), style = "width:75%; margin: auto")),
              
              column(3,
                     div( 
                          C3GaugeOutput("Gauge4", height = 150), style = "width:75%; margin: auto"))
            ),
            
            tags$head(tags$script(src="updatehc.js")),
            br(),
            
            fluidRow(column(5, offset = 1, uiOutput("tableSelector")),
                     column(5, offset = 0, uiOutput("tableSelector2"))),

            fluidRow(column(5, offset = 1, highchartOutput("memoryc", height = 400)),
                     column(5, offset = 0, highchartOutput("memoryc2", height = 400)))
            
            #br(),
            
            
   ),
   tabPanel(("Forecast"),
            br(),
            br(),
            
            h3c("Forecast"),
            fluidRow(column(5, offset = 1, uiOutput("tableSelector5"))),
            fluidRow(column(10, align = "center", plotOutput("forecast")))

   ),
   tabPanel(("Correlation"),
            br(),
            br(),
            
            h3c("Correlation"),
            fluidRow(column(5, offset = 1, uiOutput("tableSelector6"))),
            fluidRow(column(8, align = "center", plotOutput("correlation")))

   )
   


))

