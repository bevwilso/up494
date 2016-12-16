library(shiny)
library(shinydashboard)
library(DT)
library(shinythemes)
library(plotly)
library(dplyr)
library(RColorBrewer)
library(ggplot2)
library(data.table)

circuit_clerk <- read.csv("circuit_clerk.csv")
circuit_clerk$File.Date <- as.Date(circuit_clerk$File.Date, "%m/%d/%y")
circuit_clerk$Sentence.Date <- as.Date(circuit_clerk$Sentence.Date, "%m/%d/%y")
circuit_clerk <- setDT(circuit_clerk)
setkey(circuit_clerk, Charge.Type)
circuit_clerk <- data.frame(circuit_clerk)

#make data table for race and charge data
rc.db <- circuit_clerk[, c("Race", "Charge.Type")]
rc.db <- table(rc.db)
rc.db <- t(rc.db)
rc.db <- as.data.frame.matrix(rc.db)
colnames(rc.db) <- c("A", "B", "H", "U", "W", "WH")
rc.db <- tibble::rownames_to_column(rc.db, "Charge")

shinyServer(
  function(input, output){
    output$charge.explanation <- renderText({
      paste("DUI Class A: This is a first or second charge for a DUI conviction. If a person has three DUI convictions or if someone was seriously injured in the DUI accident, the person will generally be charged with a Class 2, 3, or 4 felony depending on the extent of bodily harm",
            "",
            "Class X Felony: Between 6 and 30 years in State Penitentiary and/or Fine of up to $25,000",
            "Examples: Aggravated Criminal Sexual Assault",
            "",
            "Class 1 Felony: Between 4 and 15 years in State Penitentiary and/or Fine of up to $25,000",
            "Examples: Criminal Sexual Assault, Theft of $10,000 to $100,000, Possession of Heroin, Cocaine, LSD",
            "",
            "Class 2 Felony: Between 3 and 7 years in State Penitentiary and/or Fine of up to $25,000",
            "Examples: Arson, Some drug possession (2000 grams-5000 grams of marijuana, Theft of $2,000 to $10,000)",
            "",
            "Class 3 Felony: Between 2 and 5 years in State Penitentiary and/or Fine of up to $25,000",
            "Examples: Aggravated Battery, Theft between $300 to $2,000",
            "",
            "Class 4 Felony: Between 1 and 3 years in State Penitentiary and/or Fine of up to $25,000",
            "Examples: Aggravated Assault, Stalking, Some drug possession (30-500 grams of marijuana), Felony DUI",
            "",
            "Class A Misdemeanor: Up to 1 year in Jail and/or Fine of up to $2,500", 
            "Examples: Battery, DUI, Possession of Marijuana (10-30 grams), Possession of Firearms, Reckless Driving",
            "",
            "Class B Misdemeanor: Up to 6 months in Jail and/or Fine of up to $1,500", 
            "Examples: Possession of Marijuana (2.5-10 grams), Harassment",
            "",
            "Class C Misdemeanor: Up to 30 days in Jail; and/or Fine of up to $1,500",
            "Examples: Assault, Possession of Marijuana (under 2.5 grams)",
            "",
            "Classification Source: https://www.avvo.com/legal-guides/ugc/illinois-felony-classes",
            "                       http://www.myillinoisdefenselawyer.com/illinois-criminal-code-and-laws/",
            sep = "\n")})
    
    output$bargraph <- renderPlotly({
      plot_ly(rc.db, x = ~Charge, y = ~A, type = 'bar', name = 'Asian', marker = list(color='#B22222')) %>%
        add_trace(y = ~B, name = 'Black', marker = list(color='#32CD32')) %>%
        add_trace(y = ~H, name = 'Hispanic', marker = list(color='#FF8C00')) %>%
        add_trace(y = ~W, name = 'White', marker = list(color='#4169E1')) %>%
        add_trace(y = ~WH, name = 'White Hispanic', marker = list(color='#9370DB')) %>%
        add_trace(y = ~U, name = 'Unknown', marker = list(color='#696969')) %>%
        layout(title = "Race and Charge Type", yaxis = list(title = 'Number of People'), xaxis = list(title = 'Type of Charge', tickangle = 45),
               margin = list(b=200), barmode = 'group')
    })
    
    output$table <- renderDataTable(datatable({
      
      circuit_clerk <- circuit_clerk[which(circuit_clerk$File.Date >= input$dates[1] & circuit_clerk$File.Date <= input$dates[2]),]
      
      if (input$agency != "All") {
        circuit_clerk <- circuit_clerk[circuit_clerk$Charge.Agency == input$agency,]
      }
      if (input$race != "All") {
        circuit_clerk <- circuit_clerk[circuit_clerk$Race == input$race,]
      }
      if (input$type != "All") {
        circuit_clerk <- circuit_clerk[circuit_clerk$Charge.Type == input$type,]
      }
      circuit_clerk
    },
    rownames = FALSE,
    options = list(autoWidth = FALSE,
                   columnDefs = list(list(width = "200px", targets = c("2", "12"))))))
  })