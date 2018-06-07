library(highcharter)
library(DBI)
library(forecast)
library(lubridate)
library(ggpubr)
library(dplyr)
library(zoo)
library(shiny)
library(C3)


connectDB <- function(df, userinput){
  index <- which(df$dbNames == userinput)
  dbActivePath <- toString(df[index,]$paths)
  if (!is.null(userinput)){
    if (isLocalDB(dbActivePath)) {
      con2 <- dbConnect(RSQLite::SQLite(), dbname=dbActivePath)
      return(con2)
    }
    else {
      dbfile <- tempfile() 
      download.file(dbActivePath, dbfile, mode = "wb")
      con2 <- dbConnect(RSQLite::SQLite(), dbname=dbfile)
      return(con2)
    }
  }

}

getDBs <- function(...){
  dbNames <- c()
  paths <- c()
  openfile <- file("dbloc.txt", "r")
  while (TRUE) {
    line = readLines(openfile, n = 1)
    database <- strsplit(line, split = " - ")
    if (length(line) == 0){
      break
    }
    dbNames = c(dbNames, database[[1]][1])
    paths = c(paths, database[[1]][2])
    
  }
  databases <- data.frame(dbNames, paths)
  close(openfile)
  return(databases)
}

isLocalDB <- function(path){
  if (grepl("C:/", path)) {
    return(TRUE)
  }
  else {
    return(FALSE)
  }
}

correlationData <- function(datatype) {
  con <- dbConnect(RSQLite::SQLite(), dbname="C:/Users/Ako/Documents/R/sqlite.db")
  
  if (equals(datatype,3)){
    co2data1 <-  dbGetQuery(con, "select * from 'item0003'")
    co2data2 <-  dbGetQuery(con, "select * from 'item0008'")
    
    co2data1 <- Interpolate(co2data1)
    co2data2 <- Interpolate(co2data2)
    
    data <- data.frame(SmartHome = co2data1$approx[1:nrow(co2data1)], SmartOffice = co2data2$approx[1:nrow(co2data1)])
  }
  if (equals(datatype,2)){
    tempdata1 <-  dbGetQuery(con, "select * from 'item0004'")
    tempdata2 <-  dbGetQuery(con, "select * from 'item0009'")

    tempdata1 <- Interpolate(tempdata1)
    tempdata2 <- Interpolate(tempdata2)
    
    data <- data.frame(SmartHome = tempdata1$approx[1:nrow(tempdata2)], SmartOffice = tempdata2$approx[1:nrow(tempdata2)])
  }
  
  if (equals(datatype,1)){
    noisedata1 <-  dbGetQuery(con, "select * from 'item0005'")
    noisedata2 <-  dbGetQuery(con, "select * from 'item0012'")
    
    noisedata1 <- Interpolate(noisedata1)
    noisedata2 <- Interpolate(noisedata2)
    
    data <- data.frame(SmartHome = noisedata1$approx[1:nrow(noisedata1)], SmartOffice = noisedata2$approx[1:nrow(noisedata1)])
  }
  return(data)
}

forecastData <- function(datatype) {
  con <- dbConnect(RSQLite::SQLite(), dbname="C:/Users/Ako/Documents/R/sqlite.db")
  len <- nrow(dbGetQuery(con, "select * from 'item0005'"))
    
  if (equals(datatype,1)){
    data <- dbGetQuery(con, "select * from 'item0003'")
    data <- data.frame(time = data$time[(nrow(data)-len):nrow(data)], value = data$value[(nrow(data)-len):nrow(data)])
    data <- Interpolate(data)
  }
  if (equals(datatype,2)){
    data <- dbGetQuery(con, "select * from 'item0004'")
    data <- data.frame(time = data$time[(nrow(data)-len):nrow(data)], value = data$value[(nrow(data)-len):nrow(data)])
    data <- Interpolate(data)
  }
  if (equals(datatype,3)){
    data <- dbGetQuery(con, "select * from 'item0005'")
    data <- data.frame(time = data$time, value = data$value)
    data <- Interpolate(data)
  }

  return(data)
}

getNumericTables <- function(con){
  req(con)
  tables <- data.frame(dbListTables(con))
  tables <- head(tables, nrow(tables)-2)
  
  items <- data.frame(dbGetQuery( con,'select itemname from items'))
  df <- data.frame(tables, items)
  
  fittingTables <- c()
  fittingItems <- c()
  
  a <- 1
  for (i in df[[1]]) {
    if (is.numeric(head(dbReadTable(con, i)$value,1)[[1]])){
      item <- df$itemname[[a]]
      fittingTables <- c(fittingTables, i)
      fittingItems <- c(fittingItems, item)
    }
    a <- a+1
  }
  
  df <- data.frame(fittingItems, fittingTables)
  return(df)
  
}

Interpolate <- function(df){
  df$time <- as.POSIXct(df$time)
  
  len <- length(df$time)
  start <- df$time[1]
  end <- df$time[len]

  ts <- seq(start, end, by = "10 min")
  ts <- data.frame(time = ts)
  
  df5 <- data.frame(time = seq(start, end, by = "10 min")) %>%
    full_join(df, by = "time") %>%
    mutate(approx = na.approx(value))
  
  df5 <- df5 %>% group_by(time) %>% summarise_all(funs(first(.[!is.na(.)])))
  df <- df5[complete.cases(df5), ]
  
  return(df)
}


shinyServer(function(input, output, session) {
  
  observe({
    x <- input$variable1
    
    if (is.null(x))
      x <- character(0)
    
    updateSelectInput(session, "inSelect",
                      label = paste("Select input label", length(x)),
                      choices = x,
                      selected = tail(x, 1)
    )
  })
  
  databases <- getDBs()

  output$tableSelector3 <- renderUI({
    selectInput("variable3", "Select database",  databases$dbNames)
    
  })
  
  con3 <- reactive({
    req(input$variable3)
    val <- connectDB(databases,input$variable3)
    return(val)
  })

  tables <- reactive({
    tables <- getNumericTables(con3())
    return(tables)
  })
  
  
  output$tableSelector <- renderUI({
    tables <- tables()
    selectInput("variable1", "Select value to display in graph 1",  tables$fittingItems)
  })
  
  output$tableSelector2 <- renderUI({
    validate(need(input$variable3, "Just a moment"))
    tables <- tables()
    selectInput("variable2", "Select value to display in graph 2",  tables$fittingItems, selected = tables$fittingItems[[2]])
  })
  
  

  output$tableSelector4 <- renderUI({
    
    tables <- tables()
    selectizeInput("variable4", "Select Gauges",  tables$fittingItems, options = list(maxItems = 4), selected = head(tables$fittingItems, 4), multiple = TRUE)
    
  })
  
  
  
  
  
  MemUsedC <- reactive({
    con2 <- con3()
    req(input$variable1)
    tables <- tables()
    table <- tables[which(tables$fittingItems == input$variable1), ]$fittingTables
    value <- dbGetQuery(con2, paste('select * from', table))
    
    return(value)
  })
  
  MemUsedC2 <- reactive({
    con2 <- con3()
    req(input$variable2)
    tables <- tables()
    table <- tables[which(tables$fittingItems == input$variable2), ]$fittingTables
    value <- dbGetQuery(con2, paste('select * from', table))
    dfDate <- data.frame(as.Date(value$time))
    dfTime <- as.POSIXct(value$time)
    return(value)
  })
  
  
  output$memoryc <- renderHighchart({
    df <- MemUsedC()
    df$time <- as.numeric(as.POSIXct(df$time))*1000
    hc <- data.frame(time = df$time, value = df$value) %>%
      hchart("line", hcaes(time, value)) %>%
    hc_xAxis(type = "datetime", dateTimeLabelFormats = list(day = '%b %d'))
    hc$x$type <- "stock"
    
    hc
    
  })
  
  output$memoryc2 <- renderHighchart({
    
    df <- MemUsedC2()
    df$time <- as.numeric(as.POSIXct(df$time))*1000
    hc2 <- data.frame(time = df$time, value = df$value) %>%
      hchart("line", hcaes(time, value)) %>%
    hc_xAxis(type = "datetime", dateTimeLabelFormats = list(day = '%b %d'))
    hc2$x$type <- "stock"
    hc2
  })

  
  ###
  ### gauges
  ###
  
  output$Gauge1 <- renderC3Gauge({
    validate(need(input$variable4, "Just a moment, rendering elements"))
    con2 <- con3()
    tables <- tables()
    table <- tables[which(tables$fittingItems == input$variable4[[1]]), ]$fittingTables
    val <- dbGetQuery(con2, paste("select value from ",table," order by time desc limit 1"))
    
    C3Gauge(value = c(val$value))
  })

  output$Gauge2 <- renderC3Gauge({
    validate(need(input$variable4, "Just a moment, rendering elements"))
    if (length(input$variable4) >= 2){
      
      con2 <- con3()
      tables <- tables()
      table <- tables[which(tables$fittingItems == input$variable4[[2]]), ]$fittingTables
      
      val <- dbGetQuery(con2, paste("select value from ",table," order by time desc limit 1"))
      C3Gauge(value = c(val$value))
    }
  })

  output$Gauge3 <- renderC3Gauge({
    validate(need(input$variable4, "Just a moment, rendering elements"))
    if (length(input$variable4) >= 3){
      
      con2 <- con3()
      tables <- tables()
      table <- tables[which(tables$fittingItems == input$variable4[[3]]), ]$fittingTables
      
      val <- dbGetQuery(con2, paste("select value from ",table," order by time desc limit 1"))
      C3Gauge(value = c(val$value))
    }
  })

  output$Gauge4 <- renderC3Gauge({
    validate(need(input$variable4, "Just a moment, rendering elements"))
    if (length(input$variable4) >= 4){
      
      con2 <- con3()
      tables <- tables()
      table <- tables[which(tables$fittingItems == input$variable4[[4]]), ]$fittingTables
      
      val <- dbGetQuery(con2, paste("select value from ",table," order by time desc limit 1"))
      C3Gauge(value = c(val$value))
    }
    
  })
  
  ###########################################
  
  output$tableSelector5 <- renderUI({
    
    tables <- c("CO2", "Temperature", "Noise")
    selectInput("variable5", "Select Forecast",  tables)
    
  })
  
  
  output$forecast <- renderPlot({
    validate(need(input$variable5, "Just a moment, rendering forecast"))
    if (input$variable5 == "CO2") {
      data <-  forecastData(1)
      s = c(1510783989, 1510783990, 1510783991, 1510783992, 1510783993, 1510783994, 1510783995)
      s2 = seq(data$time[1], data$time[length(data$time)], by = "24 hour")
    }
    
    if (input$variable5 == "Temperature") {
      data <-  forecastData(2)
      s = c(1508463483, 1508463484, 1508463485, 1508463486, 1508463487, 1508463488, 1508463489)
      s2 = seq(data$time[1], data$time[length(data$time)], by = "120 hour")
    }
    
    if (input$variable5 == "Noise") {
      data <-  forecastData(3)
      s = c(1506583180, 1506583181, 1506583182, 1506583183, 1506583184, 1506583185, 1506583186)
      s2 = seq(data$time[1], data$time[length(data$time)], by = "192 hour")
    }

    
    data = ts(data[,3], start=data$time[1],frequency = 144)
    
    ARIMAfit = auto.arima((data), D=1)
    summary(ARIMAfit)
    plot(forecast(ARIMAfit,h=144), main = input$variable5, xaxt="n")
    axis(1, at = s, labels = s2)
  })
  
  output$tableSelector6 <- renderUI({
    
    tables <- c("Noise", "Temperature", "CO2")
    selectInput("variable6", "Select Correlation",  tables)
    
  })
  
  output$correlation <- renderPlot({
    validate(need(input$variable6, "Just a moment, rendering correlation"))
    if (input$variable6 == "Noise") {
      data <- correlationData(1)
      first <- "SmartHome"
      second <- "SmartOffice"
    }
    
    if (input$variable6 == "Temperature") {
      data <- correlationData(2)
      first <- "SmartHome"
      second <- "SmartOffice"
      
    }
    
    if (input$variable6 == "CO2") {
      data <- correlationData(3)
      first <- "SmartHome"
      second <- "SmartOffice"
    }
    ggscatter(data, x = first, y = second, 
              add = "reg.line", conf.int = TRUE, 
              cor.coef = TRUE, cor.method = "pearson")
  })

})
