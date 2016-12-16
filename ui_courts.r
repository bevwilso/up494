library(shiny)
library(shinydashboard)
library(shinythemes)
library(DT)
library(ggplot2)
library(plotly)
library(data.table)

###user interface for courts visualization
circuit_clerk <- read.csv("circuit_clerk.csv")
circuit_clerk$File.Date <- as.Date(circuit_clerk$File.Date, "%m/%d/%y")
circuit_clerk$Sentence.Date <- as.Date(circuit_clerk$Sentence.Date, "%m/%d/%y")
circuit_clerk <- setDT(circuit_clerk)
setkey(circuit_clerk, Charge.Type)
circuit_clerk <- data.frame(circuit_clerk)

shinyUI(navbarPage(
  theme = shinytheme("flatly"),
  title = "Champaign County Circuit Clerk Records",
  tabPanel('Data Table',
           wellPanel(
             column(4,
                    dateRangeInput("dates", "File Date Range",
                                   start = min(circuit_clerk$File.Date),
                                   end = max(circuit_clerk$File.Date),
                                   startview = "month",
                                   separator = " to ")
                    ),
             column(3,
                    selectInput("agency",
                                "Agency:",
                                c("All",
                                  unique(as.character(circuit_clerk$Charge.Agency))))
                    ),
             column(2,
                    selectInput("race",
                                "Race:",
                                c("All",
                                  unique(as.character(circuit_clerk$Race))))
                    ),
             column(3,
                    selectInput("type",
                                "Type of Charge:",
                                c("All",
                                  unique(as.character(circuit_clerk$Charge.Type))))
                    )),
           fluidRow(
             DT::dataTableOutput("table")
             )
          ),
  tabPanel(title = 'Race and Charges',
           mainPanel(plotlyOutput("bargraph"), width = 500),
           wellPanel(verbatimTextOutput("charge.explanation"))
           )
          )
)
