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
  titlePanel("Sample Size Tester"),
  
  # Sidebar with several sliders
  sidebarLayout(
    sidebarPanel(
            h2("Testing the Sample Size needed for A/B Testing"),
            h3("Plot problems may arise if sample sizes differ greatly"),
            h6("a .1 ratio is E[B] = 1.1*E[A]"),
       sliderInput("meanA",
                   "Mean of A",
                   min = 1,
                   max = 100,
                   value = 50),
       sliderInput("sdA", 
                   "Standard Deviation of A",
                   min = 1, 
                   max = 10, 
                   value = 5
                   ),
       sliderInput("meanB",
                   "Mean of B",
                   min = 1, max = 100, 
                   value = 50),
       sliderInput("sdB",
                   "Standard Deviation of B",
                   min = 1, max = 10, value = 5), 
       sliderInput("trueDif", "True Difference in Means as ratio", min = 0, max = 1,
                   value = 0, step  = .01),
       numericInput("sampleA", "Sample Size of A (max 50,000)", min = 1, max = 50000,
                    value = 30),
       numericInput("sampleB", "Sample Size of B (max 50,000)", min = 1, max = 50000,
                    value = 30),
      
       checkboxInput("varEqual", "Assume the true variances equal?", value = TRUE),
       submitButton("Finished Adjustments")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("freq1"),
       textOutput("ttest")
       
    )
  )
))
