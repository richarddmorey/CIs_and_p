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
  titlePanel("CI for normal mean (known variance)"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("mu",
                  "True mean:",
                  min = 70,
                  max = 130,
                  value = 95,step = .2),
       tags$hr(),
       sliderInput("xbar",
                   "Observed mean:",
                   min = 85,
                   max = 115,
                   value = 100),
       selectInput("N", "Sample size:",
                  c(10,25,50,100)),
      selectInput("CC", "Confidence coefficient:",
                  c("50%"=.5,"84%"=.84,"90%"=.90,"95%"=.95,"99%"=.99),selected = .95)
      
      
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("plot")
    )
  )
))
