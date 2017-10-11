dashboardPage(
  dashboardHeader(title = "Survey results"),
  dashboardSidebar(
    selectInput("variable",
                "Variable",
                c("Joe's height" = 'height',
                  "Joe's sleep last night" = 'sleep',
                  "Joe's age" = 'age')),
    selectInput('by_variable',
                'Examine results by:',
                c('', "Participant age" = 'years',
                  "Participant sex" = 'sex', "Nothing" = 'nothing'))
  ),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(plotOutput("p1", height = 250)),
      box(plotOutput("p2", height = 250)),
      valueBoxOutput('v1'),
      valueBoxOutput('v2'),
      valueBoxOutput('v3')
    )
  )
)

# library(shiny)
# 
# # Define UI for application that draws a histogram
# shinyUI(fluidPage(
#   
#   # Application title
#   titlePanel("Survey results: was the crowd wise?"),
#   
#   # Sidebar with a slider input for number of bins 
#   sidebarLayout(
#     sidebarPanel(
#        selectInput("variable",
#                    "Variable",
#                    c('height',
#                      'sleep',
#                      'age')),
#        selectInput('by_variable',
#                    'Examine results by:',
#                    c('', 'years', 'sex'))
#     ),
#     
#     # Show a plot of the generated distribution
#     mainPanel(
#        plotOutput("p1"),
#        plotOutput("p2")
#     )
#   )
# ))
