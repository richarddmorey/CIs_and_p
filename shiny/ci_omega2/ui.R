#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel(HTML("CI for ω<sup>2</sup> (one-way ANOVA)")),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("omega2",
                  HTML("True ω<sup>2</sup>:"),
                  min = 0,
                  max = 1,
                  value = .2,step = .01),
       tags$hr(),
       sliderInput("Fstat",
                   "Observed F:",
                   min = 0,
                   max = 30,
                   value = 8, step = .1),
       selectInput("N", "N per group:",
                  c(10,20,30)),
       selectInput("J", "Number of groups:",
                  c(2,3,4)),
       selectInput("CC", "Confidence coefficient:",
                  c("50%"=.5,"84%"=.84,"90%"=.90,"95%"=.95,"99%"=.99),selected = .95)
      
      
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("plot")
    )
  )
))
