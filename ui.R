#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# 
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Assessing Basketball Players by Game Score and Results"),
  
  # 
  sidebarLayout(
    sidebarPanel(
      h2("Which players would you like to assess?"),
     
      checkboxInput("durant","Kevin Durant (GSW)",value = TRUE),
      checkboxInput("harden","James Harden (HOU)",value = TRUE),
      checkboxInput("westbrook","Russell Westbrook (OKC)",value = TRUE),
      checkboxInput("james","Lebron James(CLE)",value = TRUE),
      checkboxInput("leonard","Kawhi Leonard (SAN)",value = TRUE),
      
      h4("Highlight an area of the plot to specify games"),
      tableOutput("SlopeTable"),
      submitButton("Finished Selecting")
      ),
    # 
    mainPanel(
            tabsetPanel(type = "tabs",
                tabPanel("Main",br(),        
       plotOutput("gameplot", brush = brushOpts( id = "brush1")),
       h3("Relationship between Individual Performance and Team Point Differential"),
       h6("Marginal Result, is the increase in Results (for example, your team winning by 2
          points instead of 1) for a 1pt increase in Game Score (a single number representing
          all the individual contributions of a player)")
                        ),
                tabPanel("documentation",br(),textOutput("documentation"))
                )
        )
)
))

