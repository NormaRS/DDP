# Developing Data Products
# Course Project Part 1 Shiny application
# Norma Ruiz
# Noviembre 2015

library(shiny)
#shinyUI(pageWithSidebar(
#  headerPanel("Portfolio composition investing in 2 risky assets"),
#  sidebarPanel(
#    sliderInput('aapl_wt', 'APPLE weight',value = 0.5, min = -1, max = 1.5, step = 0.1,)
#  ),
#  mainPanel(
#    plotOutput('myplot')
#  )
#))

shinyUI(fluidPage(
  titlePanel("Portfolio analysis - investing in 2 risky assets"),
  fluidRow(
    column(9, offset = 1,
           br(),
           pre(includeText("include01.txt")),
           br(),
           br()
      )
    ) ,
  fluidRow(
    column(4, 
           wellPanel(
             sliderInput('aapl_wt', 'APPLE weight',value = 0.5, min = -1, max = 2, step = 0.1,)
           )
         ),

  column(8, 
    plotOutput('myplot')
    )
  )
))