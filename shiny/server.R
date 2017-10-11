#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(googlesheets)
library(ggplot2)
library(dplyr)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  
  
  output$p1 <- renderPlot({
    plot_variable(input$variable)
  })
  
  output$p2 <- renderPlot({
    plot_variable_by(input$variable,
                     input$by_variable)
  })
  output$v1 <- renderValueBox({
    valueBox(value = nrow(df),
             subtitle = 'Participants',
             icon = icon("cog", lib = "glyphicon"),
             color = 'light-blue')
  })
  output$v2 <- renderValueBox({
    valueBox(value = round(mean(df$years, na.rm = TRUE), digits = 1),
             subtitle = 'Average age of participants',
             icon = icon('table'),
             color = 'red')
  })
  output$v3 <- renderValueBox({
    valueBox(value = round(length(which(df$sex == 'Female')) / nrow(df) * 100, digits = 1),
             subtitle = '% female',
             icon = icon('calendar'),
             color = 'orange')
  })
  
})
